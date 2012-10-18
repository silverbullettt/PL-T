#lang racket
(require "define.rkt" "util/table.rkt" "util/utility.rkt")
(provide PL/T-analyzer print-env print-st)

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
    
    (define (const-tok-value tok)
      (match (tree-type tok)
        [(or 'int 'real) (string->number (tree-content tok))]
        ['bool (string=? (tree-content tok) "#t")]
        ['string (tree-content tok)]
        ['null null]
        [x (error 'const-tok-value "Unknown type '~a'" x)]))
    (define (const-type x)
      (cond [(integer? x) 'int]
            [(real? x) 'real]
            [(boolean? x) 'bool]
            [(string? x) 'string]
            [(null? x) 'null]
            [else (error 'const-type
                         "Type of '~a' is unknown" x)])) 
    (define const? (member-tester '(int real bool string null)))
    
    (define (get-type exp env) ; return symbol directly
      (define (get-arith-type t)
        ; may int or real
        (call/cc
         (lambda (return)
           (let ([unknown? #f])
             (for-each
              (lambda (x)
                (match (get-type x env)
                  ['real (return 'real)]
                  ['var (set! unknown? #t)]
                  [_ (void)]))
              (tree-content t))
             (if unknown? 'var 'int)))))
      ; get the type of expression tree
      (if (tree? exp)
          (match (tree-type exp)
            ['ident
             (let* ([id (tree-content exp)]
                    [real-id (if (decorated? id)
                                 id
                                 (get-real-id id env))])
               (if (not real-id)
                   #f
                   (type-info-value (st-lookup real-id 'type))))]
            ; the exp must be checked before pass in 'get-type'
            ['call (caadr
                    (type-info-value
                     (st-lookup (id-name (car (tree-content exp))) 'type)))]
            [(? const?) (tree-type exp)]
            [(? arith-op?) (get-arith-type exp)]
            [(? cond-op?) 'bool]
            [(? str-op?) 'string]
            [_ (error 'get-type "unknown type -- ~a" (tree-type exp))])
          #f))
    
    ; ------------------------------ exp checker ------------------------------
    (define (make-exp-checker env)
      
      (define (check-arith t)
        (match (tree-type t)
          [(or 'int 'real) #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               [(or 'int 'real) (rename! t real-id) #t]
               ['var (rename! t real-id) #t]
               [#f (printf "ARITH ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [x (printf "ARITH ERROR: '~a' is ~a, except ~a, ~a.~%"
                          (id-name t) x 'number (id-pos t))
                  #f]))]
          ['call (check-call-exp t 'arith)]
          ['null (printf "ARITH ERROR: NULL can not be used for computing, ~a.~%"
                         (tree-pos t))]
          [(? arith-op?) 
           (list-and
            (map check-arith (tree-content t)))]
          [x (printf "ARITH ERROR: Unexcepted type ~a, ~a~%" x (tree-pos t))
             #f]))
      
      (define (check-condition t)
        (match (tree-type t)
          ['bool #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               ['bool (rename! t real-id) #t]
               ['var (rename! t real-id) #t]
               [#f (printf "CONDITION ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [type (printf "CONDITION ERROR: '~a' is ~a, except ~a, ~a.~%"
                             (id-name t) type 'bool (id-pos t))
                     #f]))]
          ['call (check-call-exp t 'cond)]
          ['null (printf "CONDITION ERROR: NULL can not be used for condition, ~a.~%"
                         (tree-pos t))]
          ['not (check-exp (tree-content t))]
          [(? logic-op?) (list-and (map check-exp (tree-content t)))]
          [(? comp-op?)
           (let ([left-type (get-type (first (tree-content t)) env)]
                 [right-type (get-type (second (tree-content t)) env)])
             (when (and (check-exp (first (tree-content t)))
                        (check-exp (second (tree-content t))))
               (cond [(eq? left-type right-type) #t]
                     [(or (eq? left-type 'var) (eq? right-type 'var))
                      #t]
                     [(or (eq? left-type 'null) (eq? right-type 'null))
                      #t]
                     [(or (and (eq? left-type 'int) (eq? right-type 'real))
                          (and (eq? left-type 'real) (eq? right-type 'int)))
                      #t]
                     [else (printf "COMPARE ERROR: '~a' can not compare with '~a',  ~a.~%"
                                   left-type right-type (tree-pos t))
                           #f])))]))
      
      (define (check-str t)
        (match (tree-type t)
          ['string #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t env)
               ['string (rename! t real-id) #t]
               ['var (rename! t real-id) #t]
               [#f (printf "STRING ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [type (printf "STRING ERROR: '~a' is ~a, except ~a, ~a.~%"
                             (id-name t) type 'bool (id-pos t))
                     #f]))]
          ['call (check-call-exp t 'str)]
          ['@ (list-and (map check-str (tree-content t)))]
          ['<- (and (check-str (car (tree-content t)))
                    (list-and (map check-exp (cdr (tree-content t)))))]))
      
      (define (check-exp t)
        ; exp 是右值,意味着必须经过初始化
        ; 通过类型是否是 unknown 恰好可知它是否以初始化
        ; 若没有,则不是合法右值
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
          [(? const?) #t]
          [(? arith-op?) (check-arith t)]
          [(? cond-op?) (check-condition t)]
          [(? str-op?) (check-str t)]))
      
      (define (check-call-exp t type)
        (and (check-call-env t env)
             (check-ret-type t type)))
      
      (define (check-ret-type t type)
        (let* ([id-tree (car (tree-content t))]
               [real-id (get-real-id (id-name id-tree) env)]
               [proc-type (st-lookup real-id 'type)])
          (cond [(not real-id)
                 (printf "~a ERROR: Unknown procedure '~a', ~a.~%"
                         type (id-name id-tree) (id-pos id-tree))
                 #f]
                [(not (eq? (type-info-type proc-type) 'proc))
                 (printf "~a ERROR: ~a is not a procedure, ~a.~%"
                         type (id-name id-tree) (id-pos id-tree))
                 #f]
                [else
                 ; call in expression should only return 1 value
                 (if (not (= (length (second (type-info-value proc-type)))
                             1))
                     (begin
                       (printf "~a ERROR: ~a in expr should 1 value but ~a, ~a.~%"
                               type real-id
                               (second (type-info-value proc-type))
                               (id-pos id-tree))
                       #f)
                     (let ([ret-type (caadr (type-info-value proc-type))])
                       (match type
                         ['exp (rename! id-tree real-id) #t]
                         ['str
                          (case ret-type
                            [(str var)
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
                             (printf "ARITHMETIC ERROR: expected int or real, but ~a return a ~a, ~a.~%"
                                     real-id ret-type (id-pos id-tree))
                             #f])]
                         ['cond
                          (case ret-type
                            [(bool var)
                             (rename! id-tree real-id) #t]
                            [else
                             (printf "CONDITION ERROR: expected bool, but ~a return a ~a, ~a.~%"
                                     real-id ret-type (id-pos id-tree))])])))])))
      
      
      (define (dispatch m)
        (match m
          ['exp check-exp]
          ['arith check-arith]
          ['cond check-condition]
          ['str check-str]
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
    
    (define (add-const const-tree env)
      (when (not (null? const-tree))
        (for-each
         (lambda (x)
           (let* ([id (tree-content (car x))]
                  [dec-id (decorate-name id env)]
                  [val (const-tok-value (second x))])
             (check-dup-id (car x) env)
             (env-insert! id env)
             (rename! (car x) dec-id)
             (st-insert! dec-id 'type
                         (make-type-info 'atom (const-type val) #t))
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
               (if (tree? (second x)) ; it's initial expression
                   (let ([check-exp ((make-exp-checker env) 'exp)])
                     (when (check-exp (second x))
                       (st-insert! dec-id 'type
                                   (make-type-info
                                    'atom
                                    (get-type (second x) env) #f))))
                   (st-insert! dec-id 'type
                               (make-type-info 'atom
                                               (second x) #f))))))
       l))
    
    (define (arg-list l)
      (map
       (lambda (x)
         (if (tree? x) 'var (second x)))
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
                                    (cadadr (tree-content x)))
                              #t))))
         tlist)))
    
    (define (check-proc t env)
      ; proc should create a new environment
      (let* ([proc-name (tree-content (car (tree-content t)))]
             [new-env (make-new-env proc-name env)])
        (set-tree-content! t
                           (cons (decorate-name proc-name env)
                                 (rest (tree-content t))))
        ; add formal arguments
        (add-var-list (caadr (tree-content t)) new-env)
        (check-block (third (tree-content t)) new-env)))
    
    
    (define (get-real-id id env)
      (cond [(decorated? id) id]
            [(not env) #f]
            [(env-lookup id env) (decorate-name id env)]
            [else (get-real-id id (env-parent env))]))
    
    (define (rename! t new-name)
      (set-tree-content! t new-name))
    
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
        (cond [(eq? ty1 ty2) #t]
              [(eq? ty2 'null) #t]
              [(or (eq? ty1 'var) (eq? ty2 'var)) #t]
              [(or (and (eq? ty1 'int) (eq? ty2 'real))
                   (and (eq? ty1 'real) (eq? ty2 'int))) #t]
              [else #f]))
      
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
                         (set! result #f)))
                     expected given-ty)
                    (if result #t
                        (begin
                          (printf "~a ERROR: expected ~a type, given ~a, ~a.~%"
                                  stmt expected given-ty pos)
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
      
      (define (check-begin t)
        (list-and (map (lambda (x)
                         (check-statement x env))
                       (tree-content t))))
      
      (define check-elist (make-elist-checker env))
      
      (define (check-assign t)
        (define (check-var t)
          (let* ([id (id-name t)]
                 [real-id (get-real-id id env)])
            (cond [(not real-id) ; is identity already declared?
                   (printf "ASSIGN ERROR: Unknwon id '~a', ~a.~%"
                           id (id-pos t))
                   #f] ; is identity a constant?
                  [(type-info-const? (st-lookup real-id 'type))
                   (printf "ASSIGN ERROR: '~a' is constant and cannot be assigned, ~a.~%"
                           id
                           (type-info-type (st-lookup real-id 'type))
                           (id-pos t))
                   #f]
                  [else (rename! t real-id) #t])))
        
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
                 #t])))
      
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
      
      (define (check-if t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      ; ================== check expression ===============
      
      (define checker (make-exp-checker env))
      (define check-exp (checker 'exp))
      (define check-condition (checker 'cond))
      
      (match (tree-type t)
        ['begin (check-begin t)]
        ['assign (check-assign t)]
        ['call (check-call t)]
        ['return (check-return t)]
        ['read (check-read t)]
        ['print (check-print t)]
        ['while (check-while t)]
        ['if (check-if t)]
        [_ (error "不可能!")]))
    
    ;==========================================================================
    ;(check-block (tree-content syntax-tree) env)
    ;symbol-table))
    (if (check-block (tree-content syntax-tree) env)
        symbol-table
        #f)))

(define (print-env env)
  (when env
    (printf "~a~%" (env-name env))
    (hash-for-each (env-st env)
                   (lambda (k v)
                     (printf "~a: ~a~%" k v)))
    (print-env (env-parent env))))

(define (print-st st)
  (define (type->string t)
    (format "(~a ~a)"
            (type-info-type t) (type-info-value t)))
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

(define (parse [filename "../sample/test_null.pl"])
  (PL/T-parser
   (PL/T-scanner
    (read-string-from-file filename))))

(define (analyze [filename "../sample/test_null.pl"])
  (PL/T-analyzer (parse filename)))

(define t (parse))
(define st (PL/T-analyzer t))

(print-st st)
(newline)
(print-tree t)
