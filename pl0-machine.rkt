#lang racket
(require "define.rkt")
(provide PL/0-machine)

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

(define (PL/0-machine code-list entry)
  (define (exec pc stack memory)
    (define (get-value e)
      (if (number? e)
          e
          (hash-ref memory e)))
    (define (declare var)
      (hash-set! memory var #f))
    (define (set src dest)
      (hash-set! memory dest (get-value src)))
    (define (print e)
      (printf "~a~%" (get-value e)))
    (define (arth-op? op)
      (member op '(+ - * /)))
    (define (cond-op? op)
      (member op *cond-op*))
    (define (arth op e1 e2 var)
      (hash-set! memory
                 var
                 (eval 
                  (list op (get-value e1) (get-value e2))
                  (make-base-namespace))))
    (define (odd e var)
      (hash-set! memory var
                 (and (integer? (get-value e))
                      (odd? (get-value e)))))
    (define (cond-op op e1 e2 var)
      (if (eq? op '\#)
          (hash-set! memory var (not (= (get-value e1)
                                        (get-value e2))))
          (hash-set! memory var (eval (list op
                                            (get-value e1)
                                            (get-value e2))
                                      (make-base-namespace)))))
    
    (when (< pc (vector-length code-list))
      (call/cc
       (lambda (ret)
         (let ([inst (vector-ref code-list pc)])
           (match (car inst)
             ; allocate instructions
             ['decl (declare (second inst))]
             ['set (set (second inst) (third inst))]
             ['print (print (second inst))]
             ; jump instructions
             ['goto (ret (exec (second inst) stack memory))]
             ['if-false (when (not (get-value (second inst)))
                          (ret (exec (third inst) stack memory)))]
             ['call (ret (exec (second inst) (cons (add1 pc) stack) memory))]
             ['return (ret (exec (car stack) (cdr stack) memory))]
             ; computing instructions
             [(? arth-op?) (apply arth inst)]
             ['odd (odd (second inst) (third inst))]
             [(? cond-op?) (apply cond-op inst)]
             [_ (error "我次奥")])
           (exec (add1 pc) stack memory))))))
  (exec entry '() (make-hash)))
