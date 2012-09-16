#lang racket
(require "define.rkt" "util/table.rkt" "util/utility.rkt")
(provide PL/T-analyzer)

(define (PL/T-analyzer syntax-tree)
  (let ([symbol-table (make-table 2)]
        [env (make-env "" (set) #f)])
    
    (define st-lookup (symbol-table 'lookup))
    (define st-insert! (symbol-table 'insert!))
    (define (env-lookup id env) (set-member? (env-st env) id))
    (define (env-insert! id env) (set-env-st! env (set-add (env-st env) id)))
    
    (define (decorate-name name env) (string-append (env-name env) "@" name))
    (define (make-new-env name parent)
      (make-env (decorate-name name parent) (set) parent))
    
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
    
    (define (check-block t env)
      ; modify syntax-tree here!
      ; modify id to decorate name of variables, constants and procedures
      (add-const (first (tree-content t)) env)
      (add-var (second (tree-content t)) env)
      (add-proc (third (tree-content t)) env)
      (list-and
       (list ; 防止短路求值
        (list-and (map (lambda (p) (check-proc p env))
                       (third (tree-content t))))
        (check-statement (fourth (tree-content t)) env))))
    
    (define (add-const const-tree env)
      (when (not (null? const-tree))
        (for-each
         (lambda (x)
           (let* ([id (tree-content (car x))]
                  [dec-id (decorate-name id env)])
             (check-dup-id (car x) env)
             (env-insert! id env)
             (rename! (car x) dec-id)
             (st-insert! dec-id 'type (make-type-info 'const 'number))
             (st-insert! dec-id 'value (string->number (second x)))))
        (tree-content const-tree))))
    
    (define (add-var t env)
      (when (not (null? t))
        (for-each
         (lambda (x)
           (let* ([id (tree-content x)]
                  [dec-id (decorate-name id env)])
             (check-dup-id x env)
             (env-insert! id env)
             (rename! x dec-id)
             (st-insert! dec-id 'type (make-type-info 'var 'unknown))))
         (tree-content t))))
    
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
             (st-insert! dec-id 'type (make-type-info 'const 'proc))))
         tlist)))
    
    (define (check-proc t env)
      ; proc should create a new environment
      (let* ([proc-name (tree-content (car (tree-content t)))]
             [new-env (make-new-env proc-name env)])
        (set-tree-content! t
                           (cons (decorate-name proc-name env)
                                 (rest (tree-content t))))
        ;(st-insert! (decorate-name proc-name env) 'env env)))
        (check-block (second (tree-content t)) new-env)))
    
    
    (define (get-real-id id env)
      (cond [(not env) #f]
            [(env-lookup id env) (decorate-name id env)]
            [else (get-real-id id (env-parent env))]))
    
    (define (rename! t new-name)
      (set-tree-content! t new-name))
    
    ; =========================================================================
    
    (define (check-statement t env)
      
      (define (get-type exp)
        ; get the type of expression tree
        ; must use original id to get type
        (if (tree? exp)
            (match (tree-type exp)
              ['true 'bool]
              ['false 'bool]
              ['number 'number]
              ['ident
               (let* ([id (tree-content exp)]
                      [real-id (if (string-contain? id #\@)
                                   id
                                   (get-real-id id env))])
                 (if (not real-id)
                     #f
                     (type-info-value (st-lookup real-id 'type))))]
              [(? arith-op?) 'number]
              [(? cond?) 'bool]
              [_ (error 'get-type "unknown type -- ~a" (tree-type exp))])
            #f))
      
      (define (check-begin t)
        (list-and (map (lambda (x)
                         (check-statement x env))
                       (tree-content t))))
      
      (define (check-assign t)
        (define (check var-exp)
          (let* ([id-tree (first var-exp)]
                 [exp-tree (second var-exp)]
                 [real-id (get-real-id (id-name id-tree) env)])
            (cond [(not real-id) ; is identity already declared?
                   (printf "ASSIGN ERROR: Unknwon id '~a', ~a.~%"
                           (id-name id-tree) (id-pos id-tree))
                   #f] ; is identity a variable?
                  [(not (eq? 'var (type-info-type (st-lookup real-id 'type))))
                   (printf "ASSIGN ERROR: '~a' is ~a and cannot be assigned, ~a.~%"
                           (id-name id-tree)
                           (type-info-type (st-lookup real-id 'type))
                           (id-pos id-tree))
                   #f] ; is the expression valid?
                  [(not (check-exp exp-tree))
                   (printf "ASSIGN ERROR: not an invalid expression, ~a.~%"
                           (id-pos id-tree))
                   #f]
                  [else
                   (let ([exp-type (get-type exp-tree)]
                         [id-type (type-info-value (st-lookup real-id 'type))])
                     (if (or (eq? id-type 'unknown) (eq? exp-type id-type))
                         (begin (rename! id-tree real-id) #t)
                         (begin (printf "ASSIGN ERROR: Cannot assign ~a to ~a '~a', ~a.~%"
                                        exp-type id-type (id-name id-tree)
                                        (id-pos id-tree))
                                #f)))])))
        (list-and (map check (tree-content t))))
      
      (define (check-call t)
        (let* ([id-tree (tree-content t)]
               [real-id (get-real-id (id-name id-tree) env)])
          (cond [(not real-id)
                 (printf "CALL ERROR: Unknwon id '~a', ~a.~%"
                         (id-name id-tree) (id-pos id-tree))
                 #f]
                [(not (eq? 'proc (type-info-value (st-lookup real-id 'type))))
                 (printf "CALL ERROR: '~a' is not a procedure, ~a.~%"
                         (id-name id-tree) (id-pos id-tree))
                 #f]
                [else
                 (rename! (tree-content t) real-id)
                 #t])))
      
      (define (check-read t)
        (let* ([id-tree (tree-content t)]
               [real-id (get-real-id (id-name id-tree) env)])
          (cond [(not real-id)
                 (printf "READ ERROR: Unknwon id '~a', ~a.~%"
                         (id-name id-tree) (id-pos id-tree))
                 #f]
                [(not (eq? 'var (type-info-type (st-lookup real-id 'type))))
                 (printf "READ ERROR: '~a' is not a variable, ~a.~%"
                         (id-name id-tree) (id-pos id-tree))
                 #f]
                [else
                 (rename! (tree-content t) real-id)
                 #t])))
      
      (define (check-print t)
        (for-each check-exp (tree-content t)))
      
      (define (check-while t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      (define (check-if t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      ; ================== check expression ===============
      
      (define (check-arith t)
        (match (tree-type t)
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t)
               ['number (rename! t real-id) #t]
               ['unknown (rename! t real-id) #t]
               [#f (printf "ARITH ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [x (printf "ARITHMETIC ERROR: '~a' is ~a, except ~a, ~a.~%"
                          (id-name t) x 'number (id-pos t))
                     #f]))]
          ['number #t]
          [(? arith-op?) 
           (list-and
            (map check-arith (tree-content t)))]
          [x (printf "ARITHMETIC ERROR: Unexcepted type ~a, ~a~%" x (tree-pos t))
             #f]))
      
      (define (check-condition t)
        (match (tree-type t)
          ['true #t]
          ['false #t]
          ['ident
           (let ([real-id (get-real-id (id-name t) env)])
             (match (get-type t)
               ['bool (rename! t real-id) #t]
               ['unknown (rename! t real-id) #t]
               [#f (printf "CONDITION ERROR: Unknown id '~a', ~a.~%"
                           (id-name t) (id-pos t))
                   #f]
               [type (printf "CONDITION ERROR: '~a' is ~a, except ~a, ~a.~%"
                             (id-name t) type 'bool (id-pos t))
                     #f]))]
          ['not (check-exp (tree-content t))]
          [(? logic-op?) (list-and (map check-exp (tree-content t)))]
          [(? cond-op?) (list-and (list (check-arith (first (tree-content t)))
                                        (check-arith (second (tree-content t)))))]))
      
      (define (check-exp t)
        ; exp 是右值，意味着必须经过初始化
        ; 通过类型是否是 unknown 恰好可知它是否以初始化
        ; 若没有，则不是合法右值
        (match (tree-type t)
          ['ident
           (if (get-type t)
               (let* ([id (tree-content t)]
                      [real-id (get-real-id id env)])
                 (rename! t real-id)
                 #t)
               #f)]
           [(? arith-op?) (check-arith t)]
           [(? cond?) (check-condition t)]))
 
      (match (tree-type t)
        ['begin (check-begin t)]
        ['assign (check-assign t)]
        ['call (check-call t)]
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
    (set-for-each (env-st env) (lambda (x) (printf "  ~a~%" x)))
    (print-env (env-parent env))))
