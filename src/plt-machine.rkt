#lang racket
(require "util/table.rkt" "define.rkt")
(provide PL/T-machine)

; instruction set:
; (decl var)           -> declare a new variable
; (set e var)          -> set the value of var to e
; (call entry)         -> call dest entry, and push pc + 1 to stack
; (return)             -> return the addr which in the top of stack
; (if-false e addr)    -> if e is #f, go to addr
; (goto addr)          -> go to addr directly
; (arth-op e1 e2 var)  -> set the value of e1 arth-op e2 to var
; (cond-op e1 e2 var)  -> set the value of e1 cond-op e2 to var
; (logic-op e1 e2 var) -> set the value of e1 logic-op e2 to var
; (print e)            -> print the value of e to screen

; 一些操作直接对应类型信息
; arth-op  => integer
; cond-op  => bool
; logic-op => bool
; set      => type of var

(define (PL/T-machine code-list entry)
  (define (exec pc stack env)
    ; this environment is a little different with
    ; the environment in PL/T-analyzer, this env is a memory model
        
    (define (get-entity name [env env])
      (cond [(not env) (error "我次奥 -- unknown variable '~a'" name)]
            [(hash-has-key? (env-st env) name)
             (hash-ref (env-st env) name)]
            [else (get-entity name (env-parent env))]))
    
    (define (get-value e [env env])
      (cond [(or (number? e) (boolean? e) (string? e)) e]
            [(not env) (error "我次奥 -- unknown variable '~a'" e)]
            [(tree? e) (entity-value (get-entity (id-name e) env))]))
    
    (define (get-entity-type e)
      (type-info-value (entity-type e)))
    
    (define (get-type e [env env])
      (match e
        [(? boolean?) 'bool]
        [(? number?) 'number]        
        [(? string?) 'string] ; to do...
        [(? tree?)
         (get-entity-type (get-entity (id-name e) env))]))
    ; !!!!!!!
    (define (set-type! name type [env env])
      (if (hash-has-key? (env-st env) name)
          (let* ([ent (hash-ref (env-st env) name)]
                 [ty (entity-type ent)])
            (set-type-info-value! ty type)
            (set-entity-type! ent ty))
          (set-type! name type (env-parent env))))
    ; !!!!!!!
    (define (set-value! var val [env env])
      (if (hash-has-key? (env-st env) (id-name var))
          (let ([var-ent (hash-ref (env-st env) (id-name var))]
                [left-type (get-type var)]
                [right-type (get-type val)])
            ; check type information dynamically
            (cond [(or (eq? right-type 'unknown) (null? val))
                   (error "ASSIGN ERROR: Unknown value, L~a:~a.~%"
                          (car (id-pos var) (cdr (id-pos var))))]
                  [(eq? left-type 'unknown)
                   (set-type! (id-name var) right-type)
                   (set-entity-value! var-ent val)]
                  [(eq? left-type right-type)
                   (set-entity-value! var-ent val)]
                  [else
                   (error "ASSIGN ERROR: ~a '~a' cannot be assigned by ~a, L~a:~a.~%"
                          left-type (id-name var)
                          (car (id-pos var)) (cdr (id-pos var)))]))
          (set-value! var val (env-parent env))))
    
    (define (declare name [type 'unknown] [value '()])
      (hash-set! (env-st env)
                 name
                 (make-entity name
                              (make-type-info 'var type)
                              value)))
    ; 注意 set 指令和 set-value! 函数的 src 和 dest 是相反的= =
    (define (set src dest) (set-value! dest (get-value src)))
    
    (define (read var)
      (let ([val (read)])
        (set-value! var val)))
    (define (print e)  (printf "~a" (get-value e)))
    
    (define (arith op e1 e2 var)
      (let ([e1-type (get-type e1)] [e1-val (get-value e1)]
            [e2-type (get-type e2)] [e2-val (get-value e2)]
            [var-type (get-type var)])
        (cond [(or (null? e1-val) (null? e2-val))
               (error "ARITHMETIC ERROR: Unknown value, ~a" (id-pos var))]
              [(or (not (eq? e1-type 'number))
                   (not (eq? e2-type 'number)))
               (error "ARITHMETIC ERROR: Type '~a' ~a '~a', ~a"
                      e1-type op e2-type (id-pos var))]
              [(and (not (eq? var-type 'unknown))
                    (not (eq? var-type 'number)))
               (error "ARITHMETIC ERROR: ~a '~a' should be a '~a', ~a"
                      var-type (id-name var) 'number (id-pos var))]
              [else
               (when (eq? var-type 'unknown)
                 (set-type! (id-name var) 'number))
               (set-value! var
                           (eval (list op e1-val e2-val)
                                 (make-base-namespace)))])))
    
    (define (cond-op op e1 e2 var)
      (if (eq? op '\#)
          (set-value! var (not (= (get-value e1)
                                  (get-value e2))))
          (set-value! var (eval (list op
                                      (get-value e1)
                                      (get-value e2))
                                (make-base-namespace)))))
    
    (define (logic-op op e1 e2 var)
      (let ([result (if (eq? op 'not)
                        (not (get-value e1))
                        (eval (list op (get-value e1) (get-value e2))
                              (make-base-namespace)))])
        (set-value! var (if result #t #f))))
    
    (when (< pc (vector-length code-list))
      (call/cc
       (lambda (ret)
         (let ([inst (vector-ref code-list pc)])
           (match (car inst)
             ; allocate instructions
             ['decl (apply declare (cdr inst))]
             ['set (set (second inst) (third inst))]
             ['read (read (second inst))]
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
             ; computing instructions
             [(? arith-op?) (apply arith inst)]
             [(? cond-op?) (apply cond-op inst)]
             [(or 'not (? logic-op?)) (apply logic-op inst)]
             [_ (error "我次奥 -- unknown instruction")])
           (exec (add1 pc) stack env))))))
  (exec entry '() (make-env "" (make-hash) #f)))
