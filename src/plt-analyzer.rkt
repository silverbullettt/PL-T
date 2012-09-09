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
      (let ([id (first (tree-content x))]
            [pos (second (tree-content x))])
        (when (env-lookup id env)
          (error 'duplicate-id
                 "~a:~a is already exist as a ~a, L~a:~a"
                 (env-name env) id
                 (st-lookup (decorate-name id env) 'type)
                 (car pos) (cdr pos)))))
    
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
           (let* ([id (car (tree-content (car x)))]
                  [dec-id (decorate-name id env)])
             (check-dup-id (car x) env)
             (env-insert! id env)
             (set-tree-content! (car x)
                                (cons
                                 dec-id
                                 (rest (tree-content (car x)))))
             (st-insert! dec-id 'type 'const)
             (st-insert! dec-id 'value (string->number (second x)))))
        (tree-content const-tree))))
    
    (define (add-var t env)
      (when (not (null? t))
        (for-each
         (lambda (x)
           (let* ([id (car (tree-content x))]
                  [dec-id (decorate-name id env)])
             (check-dup-id x env)
             (env-insert! id env)
             (set-tree-content! x (cons dec-id (rest (tree-content x))))
             (st-insert! dec-id 'type 'var)))
         (tree-content t))))
    
    (define (add-proc tlist env)
      (when (not (null? tlist))
        (for-each
         (lambda (x)
           (let* (; 不统一语法树造成的恶果=_= ↓
                  [proc-name (car (tree-content (car (tree-content x))))]
                  [dec-id (decorate-name proc-name env)])
             (check-dup-id (car (tree-content x)) env)
             (env-insert! proc-name env)
             (st-insert! dec-id 'type 'proc)))
         tlist)))
    
    (define (check-proc t env)
      ; proc should create a new environment
      (let* ([proc-name (car (tree-content (car (tree-content t))))]
             [new-env (make-new-env proc-name env)])
        (set-tree-content! t
                           (cons (decorate-name proc-name env)
                                 (rest (tree-content t))))
        (check-block (second (tree-content t)) new-env)))
    
    
    (define (get-real-id id env)
      (cond [(not env) #f]
            [(env-lookup id env) (decorate-name id env)]
            [else (get-real-id id (env-parent env))]))
    
    (define (rename! t new-name)
      (set-tree-content! t (cons new-name (rest (tree-content t)))))
    
    ; =========================================================================
    
    (define (check-statement t env)
      (define (check-begin t)
        (list-and (map (lambda (x)
                         (check-statement x env))
                       (tree-content t))))
      
      (define (check-assign t)
        (let* ([id-info (tree-content (first (tree-content t)))]
               [exp-tree (second (tree-content t))]
               [real-id (get-real-id (first id-info) env)])
          (cond [(not real-id)
                 (printf "ASSIGN ERROR: Unknwon id '~a', L~a:~a.~%"
                         (first id-info) (car (second id-info)) (cdr (second id-info)))
                 #f]
                [(not (eq? 'var (st-lookup real-id 'type)))
                 (printf "ASSIGN ERROR: '~a' is ~a and cannot be assigned, L~a:~a.~%"
                         (first id-info)
                         (st-lookup real-id 'type)
                         (car (second id-info)) (cdr (second id-info)))
                 #f]
                [(not (check-exp exp-tree))
                 (printf "ASSIGN ERROR: not an invalid expression, L~a:~a.~%"
                         (car (second id-info)) (cdr (second id-info)))
                 #f]
                [else 
                 (rename! (first (tree-content t)) real-id)
                 #t])))
      
      (define (check-call t)
        (let* ([id-info (tree-content (tree-content t))]
               [real-id (get-real-id (first id-info) env)])
          (cond [(not real-id)
                 (printf "CALL ERROR: Unknwon id '~a', L~a:~a.~%"
                         (first id-info) (car (second id-info)) (cdr (second id-info)))
                 #f]
                [(not (eq? 'proc (st-lookup real-id 'type)))
                 (printf "CALL ERROR: '~a' is not a procedure, L~a:~a.~%"
                         (first id-info)
                         (car (second id-info)) (cdr (second id-info)))
                 #f]
                [else
                 (rename! (tree-content t) real-id)
                 #t])))
      
      (define (check-print t)
        (check-exp (tree-content t)))
      
      (define (check-while t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      (define (check-if t)
        (and
         (check-condition (first (tree-content t)))
         (check-statement (second (tree-content t)) env)))
      
      
      (define (check-condition t)
        (if (eq? (tree-type t) 'odd)
            (check-exp (tree-content t))
            (and
             (check-exp (first (tree-content t)))
             (check-exp (second (tree-content t))))))
      
      (define (check-exp t)
        (list-and
         (map (lambda (x)
                (if (tree? x) (check-term x) #t))
              (tree-content t))))
      
      (define (check-term t)
        (list-and
         (map (lambda (x)
                (if (tree? x) (check-factor x) #t))
              (tree-content t))))
      
      (define (check-factor t)
        (match (tree-type t)
          ['ident
           (let* ([id-info (tree-content t)]
                  [real-id (get-real-id (first id-info) env)])
             (match (st-lookup real-id 'type)
               ['var (rename! t real-id) #t]
               ['const (rename! t real-id) #t]
               ['proc
                (printf "EXPRESSION ERROR: procedure '~a' cannot be reference in expression, L~a:~a.~%"
                        (first id-info)
                        (car (second id-info)) (cdr (second id-info)))
                #f]
               [#f
                (printf "EXPRESSION ERROR: Unknwon id '~a', L~a:~a.~%"
                        (first id-info) (car (second id-info)) (cdr (second id-info)))
                #f]
               [_ (error "不可能!")]))]
          ['number #t]
          ['exp (check-exp t)]
          [_ (error "不可能!")]))
      
      (match (tree-type t)
        ['begin (check-begin t)]
        ['assign (check-assign t)]
        ['call (check-call t)]
        ['print (check-print t)]
        ['while (check-while t)]
        ['if (check-if t)]
        [_ (error "不可能!")]))
    
    ;==========================================================================
    
    (if (check-block (tree-content syntax-tree) env)
        symbol-table
        #f)))

(define (print-env env)
  (when env
    (printf "~a~%" (env-name env))
    (set-for-each (env-st env) (lambda (x) (printf "  ~a~%" x)))
    (print-env (env-parent env))))
