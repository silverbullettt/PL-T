#lang racket
(require "util/table.rkt" "define.rkt" "comparer.rkt")
(provide PL/T-machine)

; instruction set:
; (decl var type val)  -> declare a new variable
; (set e var)          -> set the value of var to e
; (call entry)         -> call dest entry, and push pc + 1 to stack
; (return)             -> return the addr which in the top of stack
; (if-false e addr)    -> if e is #f, go to addr
; (goto addr)          -> go to addr directly
; (arth-op e1 e2 var)  -> set the value of e1 arth-op e2 to var
; (cond-op e1 e2 var)  -> set the value of e1 cond-op e2 to var
; (logic-op e1 e2 var) -> set the value of e1 logic-op e2 to var
; (str-op )
; (push e)
; (pop var)
; (print e)            -> print the value of e to screen

; 一些操作直接对应类型信息
; arth-op  => int | real
; cond-op  => bool
; logic-op => bool
; set      => type of var

(define (PL/T-machine code-list entry)
  (define (exec pc stack env)
    ; this environment is a little different with
    ; the environment in PL/T-analyzer, this env is a memory model
        
    (define (get-entity name [env env])
      (cond [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" name)]
            [(hash-has-key? (env-st env) name)
             (hash-ref (env-st env) name)]
            [else (get-entity name (env-parent env))]))
    
    (define (get-value e [env env])
      (cond [(or (number? e) (boolean? e) (string? e)) e]
            [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" e)]
            [(tree? e) (entity-value (get-entity (id-name e) env))]))
    
    (define (get-entity-type e)
      (type-info-value (entity-type e)))
    
    (define (get-type e [env env])
      (match e
        [(? integer?) 'int]
        [(? real?) 'real]
        [(? boolean?) 'bool]
        [(? string?) 'string] ; to do...
        [(? tree?)
         (get-entity-type (get-entity (id-name e) env))]))
    
    (define (set-type! name type [env env])
      (if (hash-has-key? (env-st env) name)
          (let* ([ent (hash-ref (env-st env) name)]
                 [ty (entity-type ent)])
            (set-type-info-value! ty type))
          (set-type! name type (env-parent env))))
    
    (define (set-value! var val [env env])
      (if (hash-has-key? (env-st env) (id-name var))
          (let ([var-ent (hash-ref (env-st env) (id-name var))]
                [left-type (get-type var)]
                [right-type (get-type val)])
            ; check type information dynamically
            (cond [(or (eq? right-type 'unknown) (null? val))
                   (error 'ASSIGN-ERROR "Unknown value, L~a:~a.~%"
                          (car (id-pos var) (cdr (id-pos var))))]
                  [(eq? left-type 'unknown)
                   (set-type! (id-name var) right-type)
                   (set-entity-value! var-ent val)]
                  [(eq? left-type right-type)
                   (set-entity-value! var-ent val)]
                  ; convert from real to int
                  [(and (eq? left-type 'int) (eq? right-type 'real))
                   (set-entity-value! var-ent (truncate val))]
                  ; convert from int to real
                  [(and (eq? left-type 'real) (eq? right-type 'int))
                   (set-entity-value! var-ent (+ val 0.0))]
                  [else
                   (error 'ASSIGN-ERROR "~a '~a' cannot be assigned by ~a, L~a:~a.~%"
                          left-type (id-name var) right-type
                          (car (id-pos var)) (cdr (id-pos var)))]))
          (set-value! var val (env-parent env))))
    
    (define (declare name [type (make-type-info #f 'unknown #f)] [value '()])
      (hash-set! (env-st env)
                 name
                 (make-entity name type value)))
    ; 注意 set 指令和 set-value! 函数的 src 和 dest 是相反的= =
    (define (set src dest) (set-value! dest (get-value src)))
    
    (define (read-value var)
      (let ([val (read)])
        (set-value! var val)))
    (define (print e)  (printf "~a" (get-value e)))
    
    (define (arith op e1 e2 var)
      (define (check-type e)
        (let ([ty (get-type e)])
          (if (or (eq? ty 'int) (eq? ty 'real))
              #t
              (error 'ARITH-ERROR "expect (real/int) type, given (~a) '~a', ~a."
                     ty (id-name e) (id-pos e)))))
      (when (and (check-type e1) (check-type e2))
        (set-value! var
                    (eval (list op (get-value e1) (get-value e2))
                          (make-base-namespace)))))
    
    (define (comp-op op e1 e2 var)
      (define (check-type e1 e2)
        (let ([ty1 (get-type e1)] [ty2 (get-type e2)])
          (cond [(eq? ty1 'unknown)
                 (error 'TYPE-ERROR "Unknown value: '~a', ~a~%"
                        (id-name e1) (id-pos e1))]
                [(eq? ty2 'unknown)
                 (error 'TYPE-ERROR "Unknown value: '~a', ~a~%"
                        (id-name e2) (id-pos e2))]
                [(eq? ty1 ty2) ty1]
                [(or (and (eq? ty1 'int) (eq? ty2 'real))
                     (and (eq? ty1 'real) (eq? ty2 'int)))
                 ty1]
                [else (error 'COMP-OP "Can not compare '~a' with '~a', ~a~%"
                             ty1 ty2 (if (tree? e1) (id-pos e1) (id-pos e2)))])))
      (let* ([v1 (get-value e1)]
             [v2 (get-value e2)]
             [ty (check-type e1 e2)]
             [comparer (get-comparer ty op)])
        (set-value! var (comparer v1 v2))))
    
    (define (logic-op op e1 e2 var)
      (let ([result (if (eq? op 'not)
                        (not (get-value e1))
                        (eval (list op (get-value e1) (get-value e2))
                              (make-base-namespace)))])
        (set-value! var (if result #t #f))))
    
    (define (str-op op args var)
      (match op
        ['@
         ; check if value is string
         (for-each
          (lambda (x)
            (when (not (eq? (get-type x) 'string))
              (error 'STR-OP "'~a' is not a string, ~a~%"
                     (id-name x) (id-pos x))))
          args)
         (set-value! var (apply string-append
                                (map get-value args)))]
        ['<-
         (for-each
          (lambda (x)
            (when (eq? (get-type x) 'unknown)
              (error 'STR-OP "Unknown value '~a', ~a~%"
                     (id-name x) (id-pos x))))
          args)
         (set-value! var (apply format
                                (map get-value args)))]))
    
    (when (< pc (vector-length code-list))
      (call/cc
       (lambda (ret)
         (let ([inst (vector-ref code-list pc)])
           (match (car inst)
             ; allocate instructions
             ['decl (apply declare (cdr inst))]
             ['set (set (second inst) (third inst))]
             ['read (read-value (second inst))]
             ['print (print (second inst))]
             ; jump instructions
             ['goto (ret (exec (second inst) stack env))]
             ['if-false (when (not (get-value (second inst)))
                          (ret (exec (third inst) stack env)))]
             ; each CALL will create a new env
             ['call (ret (exec
                          (second inst)
                          (cons (add1 pc) stack)
                          (make-env "" (make-hash) env)))]
             ; each return will delete a env
             ['return (ret (exec (car stack)
                                 (cdr stack)
                                 (env-parent env)))]
             ;['push]
             ;['pop]
             ; computing instructions
             [(? arith-op?) (apply arith inst)]
             [(? comp-op?) (apply comp-op inst)]
             [(? logic-op?) (apply logic-op inst)]
             [(? str-op?) (apply str-op inst)]
             [_ (error "我次奥 -- unknown instruction")])
           (exec (add1 pc) stack env))))))
  (exec entry '() (make-env "" (make-hash) #f)))
