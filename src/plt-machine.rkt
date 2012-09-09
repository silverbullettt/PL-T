#lang racket
(require "define.rkt")
(provide PL/T-machine)

; instruction set:
; (decl var)          -> declare a new variable
; (set e var)         -> set the value of var to e
; (call entry)        -> call dest entry, and push pc + 1 to stack
; (return)            -> return the addr which in the top of stack
; (if-false e addr)   -> if e is #f, go to addr
; (goto addr)         -> go to addr directly
; (arth-op e1 e2 var) -> set the value of e1 arth-op e2 to var
; (cond-op e1 e2 var) -> set the value of e1 cond-op e2 to var
; (odd e var)         -> if e is odd, set var to #t, or #f
; (print e)           -> print the value of e to screen

(define (PL/T-machine code-list entry)
  (define (exec pc stack env)
    ; this environment is a little different with
    ; the environment in PL/T-analyzer, this env is a memory model
    (define (get-value e [env env])
      (cond [(number? e) e]
            [(not env) (error "我次奥 -- unknown variable")]
            [(hash-has-key? (env-st env) e)
             (hash-ref (env-st env) e)]
            [else (get-value e (env-parent env))]))
    (define (set-value! var val [env env])
      (if (hash-has-key? (env-st env) var)
          (hash-set! (env-st env) var val)
          (set-value! var val (env-parent env))))
    (define (declare var) (hash-set! (env-st env) var #f))
    (define (set src dest) (set-value! dest (get-value src)))
    (define (print e)  (printf "~a~%" (get-value e)))
    (define (arth-op? op) (member op '(+ - * /)))
    (define (cond-op? op) (member op *cond-op*))
    (define (arth op e1 e2 var)
      (set-value! var
                  (eval 
                   (list op (get-value e1) (get-value e2))
                   (make-base-namespace))))
    (define (odd e var)
      (set-value! var
                  (and (integer? (get-value e))
                       (odd? (get-value e)))))
    (define (cond-op op e1 e2 var)
      (if (eq? op '\#)
          (set-value! var (not (= (get-value e1)
                                  (get-value e2))))
          (set-value! var (eval (list op
                                      (get-value e1)
                                      (get-value e2))
                                (make-base-namespace)))))
    
    (when (< pc (vector-length code-list))
      (call/cc
       (lambda (ret)
         (let ([inst (vector-ref code-list pc)])
           (match (car inst)
             ; allocate instructions
             ['create (void)]
             ['decl (declare (second inst))]
             ['set (set (second inst) (third inst))]
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
             [(? arth-op?) (apply arth inst)]
             ['odd (odd (second inst) (third inst))]
             [(? cond-op?) (apply cond-op inst)]
             [_ (error "我次奥 -- unknown instruction")])
           (exec (add1 pc) stack env))))))
  (exec entry '() (make-env "" (make-hash) #f)))
