#lang racket
(require "define.rkt" "util/table.rkt" "util/utility.rkt")
(provide PL/T-analyzer print-env print-st)

(define debug? #f)

(define (PL/T-analyzer syntax-tree)
  (let ([symbol-table (make-table 2)]
        [env (make-env "" (set) #f)])
    
    (define st-lookup (symbol-table 'lookup))
    (define st-insert! (symbol-table 'insert!))
    (define (env-lookup id env) (set-member? (env-st env) id))
    (define (env-insert! id env) (set-env-st! env (set-add (env-st env) id)))
    (define (top-env? env) (string=? (env-name env) ""))
    
    (define (decorate-name name env) (string-append (env-name env) "@" name))
    (define (decorated? name) (string-contain? name #\@))
    (define (make-new-env name parent)
      (make-env (decorate-name name parent) (set) parent))
    
    (define (const-tree-value t)
      (define (const-array t)
        (apply vector
               (map const-tree-value (second (tree-content t)))))
      (match (tree-type t)
        [(or 'int 'real)
         (when debug?
           (printf "[[[ ~a -> ~a, ~a ]]]\n"
                   (tree-content t)
                   (string->number (tree-content t))
                   (tree-pos t)))
         (string->number (tree-content t))]
        ['bool (string=? (tree-content t) "#t")]
        ['string (tree-content t)]
        ['null 'null]
        ['const-array (const-array t)]
        [x (error 'const-tree-value "Unknown type '~a', ~a.~%"
                  x (tree-pos t))]))
    
    (define (const-type x)
      (cond [(eq? x 'null) (type-info 'null 'null #t)]
            [(vector? x)
             (type-info 'array
                        (type-info-value (const-type (vector-ref x 0)))
                        #t)]
            [else
             (type-info 'atom
                        (cond [(exact-integer? x) 'int]
                              [(real? x) 'real]
                              [(boolean? x) 'bool]
                              [(string? x) 'string]
                              [else (error 'const-type
                                           "Type of '~a' is unknown" x)])
                        #t)]))
    
    (define const? (member-tester '(int real bool string null const-array)))
    
    (define (get-type exp env) ; return symbol directly
      (define (get-arith-type t)
        ; may int or real
        (call/cc
         (lambda (return)
           (let ([unknown? #f])
             (for-each
              (lambda (x)
                (match (get-type x env)
                  [(type-info 'atom 'real _) (type-info 'atom 'real null)]
                  [(type-info 'atom 'var _) (set! unknown? #t)]
                  [_ (void)]))
              (tree-content t))
             (type-info 'atom
                        (if unknown? 'var 'int)
                        null)))))
      
      (define (get-array-type t)
        (match (tree-type t)
          ['const-array
           (type-info 'array (first (tree-content t)) #t)]
          ['new-array
           (type-info 'array (first (tree-content t)) #f)]
          ['temp-array
           (call/cc
            (lambda (ret)
              (for-each
               (lambda (elem)
                 (match (get-type elem env)
                   [(type-info 'null _ _) (void)]
                   [(type-info 'atom 'var _) (void)]
                   [(type-info 'atom _ _)
                    (ret
                     (type-info 'array
                                (type-info-value (get-type elem env))
                                #f))]))
               (tree-content t))
              (type-info 'array 'var #f)))]
          [x (error 'GET-ARRAY-TYPE "Unknown array type '~a'" x)]))
      
      ; get the type of expression tree
      (if (tree? exp)
          (match (tree-type exp)
            ['ident
             (let* ([id (tree-content exp)]
                    [real-id (get-real-id id env)])
               (if (not real-id)
                   #f
                   (st-lookup real-id 'type)))]
            ['array-ref
             (let* ([arr-id (first (tree-content exp))]
                    [real-id (get-real-id (id-name arr-id) env)])
               (if (not real-id)
                   #f
                   (type-info 'atom
                              (type-info-value (st-lookup real-id 'type))
                              (type-info-const? (st-lookup real-id 'type)))))]
            ['size (type-info 'atom 'int #f)]
            ; the exp must be checked before pass in 'get-type'
            ['call (caadr
                    (type-info-value
                     (st-lookup (id-name (car (tree-content exp))) 'type)))]
            [(? const?) (const-type (const-tree-value exp))]
            [(? arith-op?) (get-arith-type exp)]
            [(? cond-op?) (type-info 'atom 'bool #f)]
            [(? str-op?) (type-info 'atom 'string #f)]
            [(? array?) (get-array-type exp)]
            [x (error 'get-type "unknown type -- ~a, ~a.~%" x (tree-pos t))])
          (error 'GET-TYPE "Unexcepted value '~a', ~a.~%" exp (tree-pos t))))
    
    
    ; ------------------------------ exp checker ------------------------------
    (define (make-exp-checker env)
      
      (define (check-arith t)
        (match (tree-type t)
          [(or 'int 'real) #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               [(or (type-info 'atom 'real _) (type-info 'atom 'int _))
                (rename! t real-id) #t]
               [(type-info 'atom 'var _)
                (rename! t real-id) #t]
               [#f (printf "ARITH ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [x (printf "ARITH ERROR: '~a' is ~a, except ~a, ~a.~%"
                          (id-name t) (type-info->list x) 'number (id-pos t))
                  #f]))]
          ['array-ref (check-array-ref t '(int real) 'ARITH)]
          ['call (check-call-exp t 'arith)]
          ['size (check-size t)]
          [(? arith-op?) 
           (list-and
            (map check-arith (tree-content t)))]
          [x (printf "ARITH ERROR: Unexcepted type ~a, ~a~%" x (tree-pos t))
             #f]))
      
      (define (check-condition t)
        (define (can-compare? l-type r-type)
          (let ([lt (type-info-type l-type)]
                [rt (type-info-type r-type)]
                [lv (type-info-value l-type)]
                [rv (type-info-value r-type)])
            (cond [(eq? lt rt)
                   (cond [(eq? lv rv) #t]
                         [(or (eq? lv 'var) (eq? rv 'var)) #t]
                         [(or (and (eq? lv 'int) (eq? rv 'real))
                              (and (eq? lv 'real) (eq? rv 'int))) #t]
                         [else #f])]
                  [(or (eq? lt 'null) (eq? rt 'null)) #t]
                  [else #f])))
        
        (match (tree-type t)
          ['bool #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               [(type-info 'atom 'bool _) (rename! t real-id) #t]
               [(type-info 'atom 'var _) (rename! t real-id) #t]
               [#f (printf "CONDITION ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [type (printf "CONDITION ERROR: '~a' is ~a, except ~a, ~a.~%"
                             (id-name t) type 'bool (id-pos t))
                     #f]))]
          ['array-ref (check-array-ref t 'bool 'COND)]
          ['call (check-call-exp t 'cond)]
          ['not (check-exp (tree-content t))]
          [(? logic-op?) (list-and (map check-exp (tree-content t)))]
          [(? comp-op?)
           (let ([left-type (get-type (first (tree-content t)) env)]
                 [right-type (get-type (second (tree-content t)) env)])
             (when (and (check-exp (first (tree-content t)))
                        (check-exp (second (tree-content t))))
               (cond [(can-compare? left-type right-type) #t]
                     [else
                      (printf "COMPARE ERROR: can't compare '~a' with '~a',  ~a.~%"
                              (type-info->list left-type)
                              (type-info->list right-type)
                              (tree-pos t))
                      #f])))]
          [x (printf "CONDITION ERROR: '~a' can't be used for condition, ~a.~%"
                     x (tree-pos t))
             #f]))
      
      (define (check-str t)
        (match (tree-type t)
          ['string #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               [(type-info 'atom 'string _) (rename! t real-id) #t]
               [(type-info 'atom 'var _) (rename! t real-id) #t]
               [#f (printf "STRING ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [type (printf "STRING ERROR: '~a' is ~a, except ~a, ~a.~%"
                             (id-name t) type 'bool (id-pos t))
                     #f]))]
          ['array-ref (check-array-ref t 'string 'STRING)]
          ['call (check-call-exp t 'string)]
          ['@ (list-and (map check-str (tree-content t)))]
          ['<- (and (check-str (car (tree-content t)))
                    (list-and (map check-exp (cdr (tree-content t)))))]
          [x (printf "STRING ERROR: '~a' can't be used for string, ~a.~%"
                     x (tree-pos t))
             #f]))
      
      (define (check-array-ref t expect exp-type)
        ; check index!!!
        (let* ([arr-t (first (tree-content t))]
               [real-id (get-real-id (id-name arr-t) env)]
               [index (second (tree-content t))])
          (define (type-match? ty-inf)
            (let ([elem-ty (type-info-value ty-inf)])
              (if (eq? (type-info-type ty-inf) 'array)
                  (if (list? expect)
                      (member elem-ty expect)
                      (or (eq? elem-ty expect)
                          (eq? elem-ty 'var)
                          (eq? expect 'exp)))
                  (begin
                    (printf "~a ERROR: ~a is not an array, ~a.~%"
                            exp-type real-id (id-pos arr-t))
                    #f))))
          (if real-id
              (match (st-lookup real-id 'type)
                [(? type-match?)
                 (cond [(check-int index)
                        (rename! arr-t real-id) #t]
                       [(printf "ARRAY-REF ERROR: invalid index, ~a.~%"
                                (tree-pos index))
                        #f])]
                [x
                 (printf "~a ERROR: the element of '~a' is ~a, expected ~a, ~a.~%"
                         exp-type real-id
                         (type-info-value (st-lookup real-id 'type))
                         expect (id-pos arr-t))
                 #f])
              (begin
                (printf "~a ERROR: Unknown array '~a', ~a.~%"
                        exp-type (id-name arr-t) (id-pos arr-t))
                #f))))
      
      (define (check-size t)
        (check-array (tree-content t)))
      
      (define (check-int t [positive? #f])
        (match (tree-type t)
          ['int
           (cond [(and positive?
                       (negative? (const-tree-value t)))
                  (printf "INT ERROR: expected positive integer, given ~a, ~a.~%"
                          (const-tree-value t) (id-pos t))
                  #f]
                 [else #t])]
          ['ident
           (let* ([real-id (get-real-id (id-name t) env)]
                  [type (get-type t env)])
             (cond [real-id
                    (cond [(and (eq? (type-info-type type) 'atom)
                                (eq? (type-info-value type) 'int))
                           (rename! t real-id)
                           #t]
                          [else
                           (printf "INT ERROR: expected int, given ~a, ~a.~%"
                                   (type-info->list type) (id-pos t))
                           #f])]
                   [else
                    (printf "INT ERROR: unknown id ~a, ~a.~%"
                            (id-name t) (id-pos t))
                    #f]))]
          ['size (check-size t)]
          ['array-ref (check-array-ref t 'int 'INT)]
          [x (error 'CHECK-INT "Unknown type '~a', ~a~%"
                    x (id-pos t))]))
      
      (define (check-array t)
        (match (tree-type t)
          ['const-array #t]
          ['new-array
           (cond [(check-int (second (tree-content t)) #t) #t]
                 [else
                  (printf "NEW-ARRAY ERROR: new array needs positive number, ~a.~%"
                          (id-pos t))
                  #f])]
          ['temp-array
           (let ([elems (tree-content t)])
             (if (null? elems)
                 #t
                 ; 所有元素类型一样,合法
                 ; 元素必须都是原子类型
                 ; 先找到第一个非 var 类型
                 (let ([elem-ty (type-info 'atom 'var #f)]
                       [result #t])
                   (for-each
                    (lambda (elem)
                      (cond [(check-element elem)
                             (match (get-type elem env)
                               [(type-info 'null _ _) #t]
                               [(type-info 'atom 'var _) #t]
                               [(type-info 'atom _ _)
                                (if (eq? (type-info-value elem-ty) 'var)
                                    (set! elem-ty (get-type elem env))
                                    (when (not (eq? (type-info-value elem-ty)
                                                    (type-info-value (get-type elem env))))
                                      (printf "TEMP-ARRAY ERROR: ~a array can't contain ~a, ~a.~%"
                                              (type-info-value elem-ty)
                                              (type-info-value (get-type elem env))
                                              (tree-pos elem))
                                      (set! result #f)))]
                               [x (printf
                                   "TEMP-ARRAY ERROR: array excepted atom, given ~a, ~a.~%"
                                   (type-info->list (get-type elem env))
                                   (tree-pos elem))
                                  (set! result #f)])]
                            [else
                             (printf "TEMP-ARRAY ERROR: invalid element, ~a.~%"
                                     (id-pos (car elems)))
                             (set! result #f)]))
                    elems)
                   result)))]
          ['ident
           (let* ([real-id (get-real-id (id-name t) env)]
                  [type (st-lookup real-id 'type)])
             (cond [real-id
                    (cond [(eq? (type-info-type type) 'array)
                           (rename! t real-id) #t]
                          [else
                           (printf "ARRAY ERROR: expected array, given ~a, ~a.~%"
                                   (type-info->list type) (id-pos t))
                           #f])]
                   [else
                    (printf "ARRAY ERROR: Unknown id '~a', ~a.~%"
                            (id-name t) (id-pos t))
                    #f]))]
          [x (error 'CHECK-ARRAY "Unknown type '~a', ~a" x (id-pos t))]))
      
      (define (check-element t)
        (match (tree-type t)
          ['ident
           (if (get-type t env)
               (let* ([id (tree-content t)]
                      [real-id (get-real-id id env)])
                 (cond [(eq? (type-info-type (get-type t env)) 'atom)
                        (rename! t real-id) #t]
                       [else
                        (printf "ELEMENT ERROR: expected atom, given ~a, ~a.~%"
                                (type-info-type (get-type t env)) (id-pos t))
                        #f]))
               (begin (printf "ELEMENT ERROR: Unknown id '~a', ~a.~%"
                              (id-name t) (id-pos t))
                      #f))]
          [(? array?)
           (printf "ELEMENT ERROR: expected atom, given ~a, ~a.~%"
                   (type-info-type (get-type t env)) (id-pos t))
           #f]
          [_ (check-exp t)]))
      
      (define (check-exp t)
        (match (tree-type t)
          ['ident
           (if (get-type t env)
               (let* ([id (tree-content t)]
                      [real-id (get-real-id id env)])
                 (rename! t real-id)
                 #t)
               (begin (printf "EXP ERROR: Unknown id '~a', ~a.~%"
                              (id-name t) (id-pos t))
                      #f))]
          ['call (check-call-exp t 'exp)]
          ['size (check-size t)]
          ['array-ref (check-array-ref t 'exp 'EXP)]
          [(? array?) (check-array t)]
          [(? const?) #t]
          [(? arith-op?) (check-arith t)]
          [(? cond-op?) (check-condition t)]
          [(? str-op?) (check-str t)]))
      
      (define (check-call-exp t exp-type)
        (and (check-call-env t env)
             (check-ret-type-in-exp t exp-type)))
      
      (define (check-ret-type-in-exp t exp-type)
        ; 这个函数只在 exp 中的 call 使用,可以特殊对待!
        (let* ([id-tree (car (tree-content t))]
               [real-id (get-real-id (id-name id-tree) env)]
               [proc-type (st-lookup real-id 'type)])
          ; call in expression should only return 1 value
          (if (not (= (length (second (type-info-value proc-type)))
                      1))
              (begin
                (printf "~a ERROR: ~a in expr should 1 value but ~a, ~a.~%"
                        exp-type real-id
                        (if (null? (second (type-info-value proc-type)))
                            '()
                            (type-info->list
                             (second (type-info-value proc-type))))
                        (id-pos id-tree))
                #f)
              (let* ([type (caadr (type-info-value proc-type))]
                     [ret-type (type-info-value type)])
                ; [ret-type] is [type-info]
                (cond [(eq? (type-info-type type) 'atom)
                       (match exp-type
                         ['exp (rename! id-tree real-id) #t]
                         ['string
                          (case ret-type
                            [(string var)
                             (rename! id-tree real-id) #t]
                            [else
                             (printf "STRING ERROR: expected string, but ~a return a ~a, ~a.~%"
                                     real-id ret-type (id-pos id-tree))
                             #f])]
                         ['arith
                          (case ret-type
                            [(int real var)
                             (rename! id-tree real-id) #t]
                            [else
                             (printf "ARITH ERROR: expected int or real, but ~a return a ~a, ~a.~%"
                                     real-id ret-type (id-pos id-tree))
                             #f])]
                         ['cond
                          (case ret-type
                            [(bool var)
                             (rename! id-tree real-id) #t]
                            [else
                             (printf "CONDITION ERROR: expected bool, but ~a return a ~a, ~a.~%"
                                     real-id ret-type (id-pos id-tree))
                             #f])])]
                       [else
                        (printf "~a ERROR: expression expected atom type, given ~a, ~a.~%"
                                exp-type (type-info-type type) (id-pos id-tree))
                        #f])))))
      
      
      (define (dispatch m)
        (match m
          ['exp check-exp]
          ['arith check-arith]
          ['cond check-condition]
          ['string check-str]
          ['int check-int]
          ['array check-array]
          ['size check-size]
          [x (error 'checker "Unknown type '~a'" x)]))
      dispatch)
    ; ----------------------------- checker end -------------------------------
    
    (define (check-dup-id x env)
      ; check if there are duplicate id at the same environment
      ; analyze cannot continue when find duplicate identities
      (let ([id (tree-content x)]
            [pos (tree-pos x)])
        (when (env-lookup id env)
          (error 'duplicate-id
                 "~a:~a is already exist as a ~a, ~a"
                 (env-name env) id
                 (st-lookup (decorate-name id env) 'type)
                 pos))))
    
    ; get functional type (input and output) of current procedure
    (define (curr-proc-type env)
      (type-info-value
       (st-lookup (env-name env) 'type)))
    
    (define (check-block t env)
      ; check if the proc return any value when it decl as returner
      (define (has-return? t)
        (match (tree-type t)
          ['return #t]
          ['begin
           (call/cc
            (lambda (ret)
              (for-each
               (lambda (stmt) (when (has-return? stmt) (ret #t)))
               (tree-content t))
              #f))]
          ['if (has-return? (second (tree-content t)))]
          ['while (has-return? (second (tree-content t)))]
          ['foreach (has-return? (third (tree-content t)))]
          [_ #f]))
      ; modify syntax-tree here!
      ; modify id to decorate name of variables, constants and procedures
      (add-const (first (tree-content t)) env)
      (add-var (second (tree-content t)) env)
      (add-proc (third (tree-content t)) env)
      (list-and
       (list ; 防止短路求值
        (list-and (map (lambda (p) (check-proc p env))
                       (third (tree-content t))))
        ; check if the procedure return any value
        (when (not (top-env? env))
          (let ([proc-ret (second (curr-proc-type env))])
            (when (not (null? proc-ret))
              (if (has-return? (fourth (tree-content t)))
                  #t
                  (begin
                    (printf "PROCEDURE ERROR: '~a' must return ~a.~%"
                            (env-name env) proc-ret)
                    #f)))))
        (check-statement (fourth (tree-content t)) env))))
    
    (define (tree-to-type-info type [const? #f])
      (type-info (first (tree-content type))
                 (second (tree-content type))
                 const?))
    
    (define (add-const const-tree env)
      (when (not (null? const-tree))
        (for-each
         (lambda (x)
           (let* ([id (tree-content (car x))]
                  [dec-id (decorate-name id env)]
                  [val (const-tree-value (second x))])
             (check-dup-id (car x) env)
             (env-insert! id env)
             (rename! (car x) dec-id)
             (st-insert! dec-id 'type (const-type val))
             (st-insert! dec-id 'value val)))
         (tree-content const-tree))))
    
    (define (add-var t env)
      (when (not (null? t))
        (add-var-list (tree-content t) env)))
    
    (define (add-var-list l env)
      (for-each
       (lambda (x)
         (if (tree? x)
             (let* ([id (tree-content x)]
                    [dec-id (decorate-name id env)])
               (check-dup-id x env)
               (env-insert! id env)
               (rename! x dec-id)
               (st-insert! dec-id 'type (make-type-info 'atom 'var #f)))
             (let* ([id (tree-content (car x))]
                    [dec-id (decorate-name id env)])
               (check-dup-id (car x) env)
               (env-insert! id env)
               (rename! (car x) dec-id)
               ; check type and value of initial expression
               (if (eq? (tree-type (second x)) 'type)
                   (st-insert! dec-id 'type (tree-to-type-info (second x)))
                   (let ([check-exp ((make-exp-checker env) 'exp)])
                     (when (check-exp (second x))
                       (let ([var-ty (get-type (second x) env)])
                         (set-type-info-const?! var-ty #f)
                         (st-insert! dec-id 'type var-ty))))))))
       l))
    
    (define (arg-list l)
      (map
       (lambda (x)
         (tree-to-type-info (second x)))
       l))
    
    (define (add-proc tlist env)
      ; 只是添加 proc 到环境中
      (when (not (null? tlist))
        (for-each
         (lambda (x)
           (let* (; 不统一语法树造成的恶果=_= ↓
                  [proc-name (tree-content (car (tree-content x)))]
                  [dec-id (decorate-name proc-name env)])
             (check-dup-id (car (tree-content x)) env)
             (env-insert! proc-name env)
             ; type contains input args and output args
             (st-insert!
              dec-id 'type
              (make-type-info 'proc
                              (list (arg-list (caadr (tree-content x)))
                                    (map tree-to-type-info (cadadr (tree-content x))))
                              #t))))
         tlist)))
    
    (define (check-proc t env)
      ; proc should create a new environment
      (let* ([proc-name (tree-content (car (tree-content t)))]
             [new-env (make-new-env proc-name env)])
        ;(set-tree-content! t
        ;                   (cons (decorate-name proc-name env)
        ;                         (rest (tree-content t))))
        (rename! (car (tree-content t))
                 (decorate-name proc-name env))
        ; add formal arguments
        (add-var-list (caadr (tree-content t)) new-env)
        (check-block (third (tree-content t)) new-env)))
    
    
    (define (get-real-id id env)
      (cond [(decorated? id) id]
            [(not env)
             (printf "Unknown id '~a'" id) #f]
            [(env-lookup id env) (decorate-name id env)]
            [else (get-real-id id (env-parent env))]))
    
    (define (rename! t new-name)
      (when (not (decorated? (id-name t)))
        (set-tree-content! t new-name)))
    
    ; ========================== expression list checker =======================
    (define (make-elist-checker env)
      (define check-exp ((make-exp-checker env) 'exp))
      (define (check-call t) (check-call-env t env))
      
      (define (check-given-list exp-list)
        ; this function will rename the exp of list
        (let ([result #t])
          (for-each
           (lambda (e)
             (match (tree-type e)
               ['call (when (not (check-call e))
                        (set! result #t))]
               [_ (when (not (check-exp e))
                    (set! result #f))]))
           exp-list)
          result))
      
      (define (get-type-list exp-list)
        (flatten
         (map
          (lambda (exp)
            (if (eq? (tree-type exp) 'call)
                (let ([type (st-lookup
                             (id-name (car (tree-content exp)))
                             'type)])
                  (if type (second (type-info-value type)) #f))
                (if (check-exp exp)
                    (get-type exp env)
                    #f)))
          exp-list)))
      
      (define (match-exp-type? ty1 ty2)
        ; compare is directed, ty1 is expected, ty2 is given
        (let ([lt (type-info-type ty1)]
              [rt (type-info-type ty2)]
              [lv (type-info-value ty1)]
              [rv (type-info-value ty2)])
          (cond [(eq? lt rt)
                 (cond [(eq? lv rv) #t]
                       [(or (eq? lv 'var) (eq? rv 'var)) #t]
                       [(or (eq? lv 'null) (eq? rv 'null)) #t]
                       [(or (and (eq? lv 'int) (eq? rv 'real))
                            (and (eq? lv 'real) (eq? rv 'int))) #t]
                       [else #f])]
                [(eq? rt 'null) #t]
                [else #f])))
      
      (define (check-elist expected given stmt pos [name ""])
        (if (check-given-list given)
            (let ([given-ty (get-type-list given)]
                  [result #t])
              (if (not (= (length expected)
                          (length given-ty)))
                  (begin
                    (printf "~a ERROR: ~a expected ~a args, given ~a, ~a.~%"
                            stmt name (length expected) (length given-ty) pos)
                    #f)
                  (begin
                    (for-each
                     (lambda (ty1 ty2)
                       (when (not (match-exp-type? ty1 ty2))
                         (printf "~a ERROR: expected ~a type, given ~a, ~a.~%"
                                 stmt
                                 (type-info->list ty1)
                                 (type-info->list ty2)
                                 pos)
                         (set! result #f)))
                     expected given-ty)
                    (if result #t
                        (begin
                          (printf "~a ERROR: expected ~a type, given ~a, ~a.~%"
                                  stmt
                                  (map type-info->list expected)
                                  (map type-info->list given-ty)
                                  pos)
                          #f)))))
            (begin
              (printf "~a ERROR: invalid expression(s), ~a.~%"
                      stmt pos)
              #f)))
      
      check-elist)
    
    
    ; ============================ check call ==================================
    ; the call will be used in both "statement" and "expression"
    ; so I extract it from 'check-statement'
    
    (define (check-call-env t env)
      (define check-elist (make-elist-checker env))
      
      (let* ([id-tree (car (tree-content t))]
             [real-id (get-real-id (id-name id-tree) env)]
             [result #t])
        (cond [(not real-id)
               (printf "CALL ERROR: Unknwon id '~a', ~a.~%"
                       (id-name id-tree) (id-pos id-tree))
               (set! result #f)]
              [(not (eq? 'proc (type-info-type (st-lookup real-id 'type))))
               (printf "CALL ERROR: '~a' is not a procedure, ~a.~%"
                       real-id (id-pos id-tree))
               (set! result #f)]
              [else
               (let ([args (second (tree-content t))]
                     [proc-arg (first
                                (type-info-value
                                 (st-lookup real-id 'type)))])
                 (if (check-elist proc-arg args 'CALL
                                  (id-pos id-tree) real-id)
                     (rename! (first (tree-content t)) real-id)
                     (set! result #f)))])
        result))
    
    ; =========================== check statement =============================
    (define (check-statement t env)      
      
      ; ================== check expression ===============
      
      (define checker (make-exp-checker env))
      (define check-exp (checker 'exp))
      (define check-array (checker 'array))
      (define check-int (checker 'int))
      (define check-condition (checker 'cond))
      
      (define (check-begin t)
        (list-and (map (lambda (x)
                         (check-statement x env))
                       (tree-content t))))
      
      (define check-elist (make-elist-checker env))
      
      (define (check-assign t)
        (define (check-var t)
          (match (tree-type t)
            ['ident
             (let* ([id (id-name t)]
                    [real-id (get-real-id id env)])
               (cond [(not real-id) ; is identity already declared?
                      (printf "ASSIGN ERROR: Unknwon id '~a', ~a.~%"
                              id (id-pos t))
                      #f] ; is identity a constant?
                     [(type-info-const? (st-lookup real-id 'type))
                      (printf "ASSIGN ERROR: '~a' is constant and can't be assigned, ~a.~%"
                              id (id-pos t))
                      #f]
                     [else (rename! t real-id) #t]))]
            ['array-ref
             (cond [(and (check-array (first (tree-content t)))
                         (check-int (second (tree-content t))))
                    (if (type-info-const?
                         (get-type (first (tree-content t)) env))
                        (begin
                          (printf "ASSIGN ERROR: ~a is constant array, can't be assigned, ~a.~%"
                                  (id-name (first (tree-content t)))
                                  (tree-pos (first (tree-content t))))
                          #f)
                        #t)]
                   [else
                    (printf "ASSIGN ERROR: invalid array reference, ~a.~%"
                            (tree-pos t))
                    #f])]
            [x (error 'CHECK-VAR "Unknown left-value type ~a, ~a.~%"
                      (tree-type t) (tree-pos t))]))
        
        (let ([result #t]
              [var-type-list (map (lambda (x) (get-type x env))
                                  (car (tree-content t)))])
          ; 检查左值是否合法
          (for-each
           (lambda (x)
             (when (not (check-var x))
               (set! result #f)))
           (car (tree-content t)))
          ; 检查右值与左值匹不匹配
          (when (not (check-elist var-type-list
                                  (second (tree-content t))
                                  'ASSIGN
                                  (id-pos t)))
            (set! result #f))
          result))
      
      
      (define (check-call t)
        (check-call-env t env))
      
      (define (check-return t)
        (let ([proc-name (env-name env)])
          (if (string=? proc-name "") ; 顶层环境不检查返回值
              #t
              (let ([proc-ret (second (curr-proc-type env))])
                (check-elist proc-ret
                             (tree-content t)
                             'RETURN
                             (tree-pos t)
                             proc-name)))))
      
      (define (check-read t)
        (match (tree-type (tree-content t))
          ['ident
           (let* ([id-tree (tree-content t)]
                  [real-id (get-real-id (id-name id-tree) env)])
             (cond [(not real-id)
                    (printf "READ ERROR: Unknwon id '~a', ~a.~%"
                            (id-name id-tree) (id-pos id-tree))
                    #f]
                   [(type-info-const? (st-lookup real-id 'type))
                    (printf "READ ERROR: '~a' is a constant, ~a.~%"
                            (id-name id-tree) (id-pos id-tree))
                    #f]
                   [else
                    (rename! (tree-content t) real-id)
                    #t]))]
          ['array-ref
           (let ([ref-t (tree-content t)])
             (cond [(and (check-array (first (tree-content ref-t)))
                         (check-int (second (tree-content ref-t))))
                    (if (type-info-const?
                         (get-type (first (tree-content ref-t)) env))
                        (begin
                          (printf "READ ERROR: ~a is constant array, can't be assigned, ~a.~%"
                                  (id-name (first (tree-content ref-t)))
                                  (tree-pos (first (tree-content ref-t))))
                          #f)
                        #t)]
                   [else
                    (printf "READ ERROR: invalid array reference, ~a.~%"
                            (tree-pos ref-t))
                    #f]))]))
      
      
      (define (check-print t)
        (list-and
         (map
          (lambda (e)
            (if (eq? (tree-type e) 'call)
                (check-call e)
                (check-exp e)))
          (tree-content t))))
      
      (define (check-while t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      (define (check-foreach t)
        (define (check-type)
          (let ([var-ty (get-type (first (tree-content t)) env)]
                [arr-ty (get-type (second (tree-content t)) env)])
            (cond [(type-info-const? var-ty)
                   (printf "FOREACH ERROR: can't assign constant '~a', ~a.~%"
                           (id-name (first (tree-content t)))
                           (id-pos (first (tree-content t))))
                   #f]
                  [(or (eq? (type-info-value var-ty) 'var)
                       (eq? (type-info-value arr-ty) 'var))
                   #t]
                  [(eq? (type-info-value var-ty) (type-info-value arr-ty))
                   #t]
                  [else
                   (printf "FOREACH ERROR: var type(~a) not match array type(~a), ~a.~%"
                           (type-info-value var-ty) (type-info-value arr-ty)
                           (id-pos (first (tree-content t))))
                   #f])))
        (if (check-exp (first (tree-content t)))
            (if (check-array (second (tree-content t)))
                (when (check-type)
                  (check-statement (third (tree-content t)) env))
                (begin
                  (printf "FOREACH ERROR: invalid array, ~a.~%"
                          (tree-pos (second (tree-content t))))
                  #f))
            (begin
              (printf "FOREACH ERROR: invalid var, ~a.~%"
                      (tree-pos (first (tree-content t))))
              #f)))
      
      (define (check-if t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      (match (tree-type t)
        ['begin (check-begin t)]
        ['assign (check-assign t)]
        ['call (check-call t)]
        ['return (check-return t)]
        ['read (check-read t)]
        ['print (check-print t)]
        ['while (check-while t)]
        ['foreach (check-foreach t)]
        ['if (check-if t)]
        [_ (error "不可能!")]))
    
    ;==========================================================================
    (cond [debug?
           (check-block (tree-content syntax-tree) env)
           (print-st symbol-table)]
          [(check-block (tree-content syntax-tree) env)
           symbol-table]
          [else #f])))

(define (type-info->list t)
  (list (type-info-type t)
        (type-info-value t)))

(define (print-env env)
  (when env
    (printf "~a~%" (env-name env))
    (hash-for-each (env-st env)
                   (lambda (k v)
                     (printf "~a: ~a~%" k v)))
    (print-env (env-parent env))))

(define (print-st st)
  (define (type->string t)
    (format "(~a ~a ~a)"
            (type-info-type t)
            (type-info-value t)
            (if (type-info-const? t) "const" "var")))
  
  (for-each
   (lambda (x)
     (printf "(\"~a\" ~a ~a)~%"
             (first x) (second x) (if (type-info? (third x))
                                      (type->string (third x))
                                      (third x))))
   (table->list st)))

; ============================ for test =======================================
(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
         "PLT-parser.rkt"
         "define.rkt"
         "util/table.rkt")

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (parse [filename "../sample/test_array.pl"])
  (PL/T-parser
   (PL/T-scanner
    (read-string-from-file filename))))

(define (analyze [filename "../sample/test_array.pl"])
  (PL/T-analyzer (parse filename)))

(define t (parse))
(define st (PL/T-analyzer t))

(newline)
(print-tree t)
(newline)
;(print-st st)
