#lang racket
(require "util/table.rkt" "util/utility.rkt"
         "define.rkt" "comparer.rkt")
(provide PL/T-machine)

; instruction set:
; (decl var type val)  -> declare a new variable
; (set e var)          -> set the value of var to e
; (call entry)         -> call dest entry, and push pc + 1 to stack
; (return)             -> return the addr which in the top of stack
; (push e)
; (pop var)
; (if-false e addr)    -> if e is #f, go to addr
; (goto addr)          -> go to addr directly
; (arth-op args var)   -> set the value of e1 arth-op e2 to var
; (comp-op args var)   -> set the value of e1 cond-op e2 to var
; (logic-op e1 e2 var) -> set the value of e1 logic-op e2 to var
; (str-op args var)    -> set...
; (print e)            -> print the value of e to screen

; 一些操作直接对应类型信息
; arth-op  => int | real
; cond-op  => bool
; logic-op => bool
; set      => type of var

(define (PL/T-machine code-list entry)
  (define (exec pc      ; program counter
                add-stk ; address stack
                arg-stk ; argument stack
                env)
    
    ; this environment is a little different with
    ; the environment in PL/T-analyzer, this env is a memory model
    
    (define (get-entity name [env env])
      (cond [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" name)]
            [(hash-has-key? (env-st env) name)
             (hash-ref (env-st env) name)]
            [else (get-entity name (env-parent env))]))
    
    (define (get-value e [env env])
      (cond [(or (number? e) (boolean? e) (string? e)) e]
            [(eq? e 'null) 'null]
            [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" e)]
            [(tree? e) (entity-value (get-entity (id-name e) env))]))
    
    (define (get-entity-type e)
      (type-info-value (entity-type e)))
    
    (define (get-type e [env env])
      ;(printf "*** get-type: ~a\n" e)
      (match e
        [(? integer?) 'int]
        [(? real?) 'real]
        [(? boolean?) 'bool]
        [(? string?) 'string]
        ['null 'null] ; null is just a value, it doesn't have type
        [(? tree?)
         (get-entity-type (get-entity (id-name e) env))]
        [x (dump) (error 'get-type "Unknown type ~a" x)]))
    
    (define (dump)
      ; for debug
      (define (print-env env)
        (when env
          (printf "proc-name: ~a~%" (env-name env))
          (hash-for-each
           (env-st env)
           (lambda (k v)
             (printf "~a: ~a ~a~%"
                     k
                     (type-info-value (entity-type v))
                     (entity-value v))))
          (print-env (env-parent env))))
      (printf "--------------- dump start ---------------\n")
      (printf "pc: ~a\n" pc)
      (printf "address stack: ~a\n" add-stk)
      (printf "arguments stack: ~a\n" arg-stk)
      (print-env env)
      (printf "---------------- dump end ----------------\n"))
    
    (define (set-type! name type [env env])
      (if (hash-has-key? (env-st env) name)
          (let* ([ent (hash-ref (env-st env) name)]
                 [ty (entity-type ent)])
            (set-type-info-value! ty type))
          (set-type! name type (env-parent env))))
    
    (define (set-value! var val [env env])
      ;(printf "### var: ")
      ;(print-tree var)
      ;(printf ", val: ~a ###\n" val)
      (if (hash-has-key? (env-st env) (id-name var))
          (let ([var-ent (hash-ref (env-st env) (id-name var))]
                [left-type (get-type var)]
                [right-type (get-type val)])
            ; check type information dynamically
            (cond ;[(and (eq? right-type 'var) (not (null? val)))
                  ; (error 'ASSIGN-ERROR "Unknown value, L~a:~a.~%"
                  ;        (car (id-pos var) (cdr (id-pos var))))]
                  [(eq? left-type 'var)
                   (when (not (eq? right-type 'null))
                     (set-type! (id-name var) right-type))
                   (set-entity-value! var-ent val)]
                  [(eq? left-type right-type)
                   (set-entity-value! var-ent val)]
                  [(eq? right-type 'null) ; null value can be set to any type
                   (set-entity-value! var-ent 'null)]
                  ; convert from real to int
                  [(and (eq? left-type 'int) (eq? right-type 'real))
                   (set-entity-value! var-ent (exact-truncate val))]
                  ; convert from int to real
                  [(and (eq? left-type 'real) (eq? right-type 'int))
                   (set-entity-value! var-ent (+ val 0.0))]
                  [else
                   (error 'ASSIGN-ERROR "~a '~a' cannot be assigned by ~a, L~a:~a.~%"
                          left-type (id-name var) right-type
                          (car (id-pos var)) (cdr (id-pos var)))]))
          (set-value! var val (env-parent env))))
    
    (define (declare name [type (make-type-info 'atom 'var #f)] [value 'null])
      (hash-set! (env-st env)
                 name
                 (make-entity name type value)))
    
    (define (set dest src) (set-value! dest (get-value src)))
    
    (define (read-value var)
      (let ([val (read)])
        (set-value! var val)))
    (define (print e)  (printf "~a" (get-value e)))
    
    (define (arith op args var)
      (define (check-type e)
        (let ([ty (get-type e)])
          (if (or (eq? ty 'int) (eq? ty 'real))
              #t
              (error 'ARITH-ERROR "expect (real/int) type, given (~a) '~a', ~a."
                     ty (id-name e) (id-pos e)))))
      (when (list-and (map check-type args))
        (set-value! var
                    (eval (cons op (map get-value args))
                          (make-base-namespace)))))
    
    (define (comp-op op e1 e2 var)
      (define (check-type e1 e2)
        (let ([ty1 (get-type e1)] [ty2 (get-type e2)])
          (cond [(or (eq? ty1 'null) (eq? ty2 'null))
                 'null]
                [(eq? ty1 'var)
                 (error 'TYPE-ERROR "Unknown value: '~a', ~a~%"
                        (id-name e1) (id-pos e1))]
                [(eq? ty2 'var)
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
             [ty (check-type e1 e2)])
        (if (eq? ty 'null)
            (begin
              ;(printf "   $$$ v1=~a, v2=~a $$$\n" v1 v2)
              (match op
                ['= (set-value! var (eq? v1 v2))]
                ['\# (set-value! var (not (eq? v1 v2)))]
                [x (error 'COMP-OP
                          "x can not compare NULL value, ~a.~%" (id-pos e1))]))
            (set-value! var ((get-comparer ty op) v1 v2)))))
    
    (define (logic-op op args var)
      (let ([result
             (match op
               ['not (not (get-value args))]
               ['and (list-and (map get-value args))]
               ['or (list-or (map get-value args))])])
        (set-value! var result)))
    
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
            (when (eq? (get-type x) 'var)
              (error 'STR-OP "Unknown value '~a', ~a~%"
                     (id-name x) (id-pos x))))
          args)
         (set-value! var (apply format
                                (map get-value args)))]))
    
    (define (push exp)
      (set! arg-stk (cons (get-value exp) arg-stk)))
    
    (define (pop var)
      (set-value! var (car arg-stk))
      (set! arg-stk (cdr arg-stk)))
    
    (when (< pc (vector-length code-list))
      ;(printf "pc: ~a, arguments stack: ~a\n" pc arg-stk)
      (call/cc
       (lambda (ret)
         (let ([inst (vector-ref code-list pc)])
           (match (car inst)
             ; allocate instructions
             ['decl (apply declare (cdr inst))]
             ['set (set (second inst) (third inst))]
             ['read (read-value (second inst))]
             ['print (print (get-value (second inst)))]
             ; jump instructions
             ['goto (ret (exec (second inst) add-stk arg-stk env))]
             ['if-false (when (not (get-value (second inst)))
                          (ret (exec (third inst) add-stk arg-stk env)))]
             ; each CALL will create a new env
             ['call (ret (exec
                          (second inst)
                          (cons (add1 pc) add-stk)
                          arg-stk
                          (make-env "" (make-hash) env)))]
             ; each return will delete a env
             ['return (ret (exec (car add-stk)
                                 (cdr add-stk)
                                 arg-stk
                                 (env-parent env)))]
             ; argument instructions
             ['push (apply push (cdr inst))]
             ['pop (apply pop (cdr inst))]
             ; computing instructions
             [(? arith-op?) (apply arith inst)]
             [(? comp-op?) (apply comp-op inst)]
             [(? logic-op?) (apply logic-op inst)]
             [(? str-op?) (apply str-op inst)]
             [_ (error "我次奥 -- unknown instruction")])
           (exec (add1 pc) add-stk arg-stk env))))))
  (exec entry '() '() (make-env "" (make-hash) #f)))

; ============================ for test =======================================
(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
         "PLT-parser.rkt"
         "PLT-analyzer.rkt"
         "PLT-generator.rkt"
         "define.rkt"
         "util/table.rkt")

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (exec [filename "../sample/test_null.pl"])
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (let ([code-ent (PL/T-generator t st)])
          (PL/T-machine (first code-ent)
                        (second code-ent)))
        (error 'exec "Something wrong!"))))

(exec)
