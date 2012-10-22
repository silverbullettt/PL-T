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

; (array v-list dest)
; (array-ref arr index dest)
; (array-set arr-t src)
; (array-read arr-t)
; (size arr)

; 一些操作直接对应类型信息
; arth-op  => int | real
; cond-op  => bool
; logic-op => bool
; set      => type of var

(define debug? #f)

(define (PL/T-machine code-list entry)
  (define (exec pc      ; program counter
                add-stk ; address stack
                arg-stk ; argument stack
                env)
    
    (define (temp? t)
      (and (tree? t) (string-startswith? (id-name t) "%")))
    
    ; this environment is a little different with
    ; the environment in PL/T-analyzer, this env is a memory model
    
    (define (get-entity name [env env])
      (cond [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" name)]
            [(hash-has-key? (env-st env) name)
             (hash-ref (env-st env) name)]
            [else (get-entity name (env-parent env))]))
    
    (define (get-value e [env env])
      (cond [(or (number? e) (boolean? e) (string? e)) e]
            [(vector? e) e]
            [(eq? e 'null) 'null]
            [(not env) (error 'gen-entity "我次奥 -- unknown variable '~a'" e)]
            [(tree? e) (entity-value (get-entity (id-name e) env))]))
    
    (define (get-entity-type e)
      (type-info-value (entity-type e)))
    
    (define (get-type-info e [env env])
      (entity-type (get-entity (id-name e) env)))
    
    (define (get-type e [env env])
      (debugger "*** get-type: ~a\n" e)
      (match e
        [(? exact-integer?) (type-info 'atom 'int #f)]
        [(? real?) (type-info 'atom 'real #f)]
        [(? boolean?) (type-info 'atom 'bool #f)]
        [(? string?) (type-info 'atom 'string #f)]
        [(? vector?) (get-array-type e)]
        ['null (type-info 'null 'null #f)] ; null is just a value, it doesn't have type
        [(? tree?) (get-type-info e env)]
        [x (dump) (error 'get-type "Unknown type ~a" x)]))
    
    (define (get-array-type array)
      (call/cc
       (lambda (ret)
         (let iter ([i 0])
           (cond [(= i (vector-length array))
                  (ret (type-info 'array 'null #f))]
                 [(eq? (vector-ref array i) 'null)
                  (iter (add1 i))]
                 [else
                  (ret (type-info 'array
                                  (type-info-value (get-type (vector-ref array i)))
                                  #f))])))))
    
    (define (type-info->string ty)
      (if (type-info? ty)
          (format "~a ~a" (type-info-type ty) (type-info-value ty))
          (format "~a" ty)))
    
    (define (dump)
      ; for debug
      (define (print-env env tab)
        (when env
          (printf "~aproc-name: ~a~%" tab (env-name env))
          (hash-for-each
           (env-st env)
           (lambda (k v)
             (printf "~a~a: [~a] ~a~%"
                     tab k
                     (type-info->string (entity-type v))
                     (entity-value v))))
          (print-env (env-parent env)
                     (format "  ~a" tab))))
      (printf "--------------- dump start ---------------\n")
      (printf "pc: ~a\n" pc)
      (printf "address stack: ~a\n" add-stk)
      (printf "arguments stack: ~a\n" arg-stk)
      (print-env env "")
      (printf "---------------- dump end ----------------\n"))
    
    (define (debugger . args)
      (when debug? (apply printf args)))
    
    (define (set-type! name type [env env])
      (if (hash-has-key? (env-st env) name)
          (let* ([ent (hash-ref (env-st env) name)]
                 [ty (entity-type ent)])
            (set-type-info-value! ty type))
          (set-type! name type (env-parent env))))
    
    (define (set-value! var val [env env])
      (when debug?
        (printf "### var: ")
        (print-tree var)
        (printf ", val: ~a ###\n" val))
      (if (hash-has-key? (env-st env) (id-name var))
          (let ([var-ent (hash-ref (env-st env) (id-name var))]
                [left-type (get-type var)]
                [right-type (get-type val)])
            ; check type information dynamically
            (if (eq? val 'null) ; null value can be set to any type
                (set-entity-value! var-ent 'null)
                (cond [(eq? (type-info-type left-type) 'array)
                       (cond [(eq? (type-info-value left-type)
                                   (type-info-value right-type))
                              (set-entity-value! var-ent val)]
                             [(eq? (type-info-value left-type) 'var)
                              (set-type! (id-name var)
                                         (type-info-value right-type))
                              (set-entity-value! var-ent val)]
                             [(eq? (type-info-value right-type) 'null)
                              (set-entity-value! var-ent val)]
                             [else
                              (error 'SET-VALUE!
                                     "~a array -> ~a array '~a', ~a"
                                     (type-info-value right-type)
                                     (type-info-value left-type)
                                     (id-name var)
                                     (id-pos var))])]
                      [(eq? (type-info-type left-type) 'atom)
                       (let ([lv (type-info-value left-type)]
                             [rv (type-info-value right-type)])
                         (cond [(eq? lv 'var)
                                (set-type! (id-name var) rv)
                                (set-entity-value! var-ent val)]
                               [(eq? lv rv)
                                (set-entity-value! var-ent val)]
                               ; convert from real to int
                               [(and (eq? lv 'int) (eq? rv 'real))
                                (set-entity-value! var-ent (exact-truncate val))]
                               ; convert from int to real
                               [(and (eq? lv 'real) (eq? rv 'int))
                                (set-entity-value! var-ent (+ val 0.0))]
                               [else
                                (error 'ASSIGN-ERROR "~a '~a' cannot be assigned by ~a, ~a.~%"
                                       lv (id-name var) rv (id-pos var))]))]
                      [else
                       (error 'SET-VALUE!
                              "~a can't be assigned by ~a, ~a"
                              (type-info-type left-type)
                              (type-info-type right-type)
                              (tree-pos var))])))
          (set-value! var val (env-parent env))))
    
    (define (copy-type type)
      (type-info (type-info-type type)
                 (type-info-value type)
                 #f))
    
    (define (declare name [type (make-type-info 'atom 'var #f)] [value 'null])
      (debugger "%%% declare: \"~a\" with value: ~a(~a)\n"
                name (get-value value) value)
      (hash-set! (env-st env)
                 name
                 (make-entity name (copy-type type) (get-value value))))
    
    (define (set dest src) (set-value! dest (get-value src)))
    
    ; ======================= array instructions start ========================
    (define (check-array-index array index pos)
      (if (eq? array 'null)
          (begin
            (dump)
            (error 'ARRAY-ERROR
                   "null array can't be referenced, ~a."
                   pos))
          (if (< index 0)
              (if (index < (- (vector-length array)))
                  (error 'INDEX-ERROR
                         "array index out of range, index: ~a, valid range: [~a, ~a], vector: ~a, ~a"
                         index (- (vector-length array)) (- (vector-length array) 1)
                         array pos)
                  #t)
              (if (>= index (vector-length array))
                  (error 'INDEX-ERROR
                         "array index out of range, index: ~a, valid range: [~a, ~a], vector: ~a, ~a"
                         index (- (vector-length array)) (- (vector-length array) 1)
                         array pos)
                  #t))))
    
    (define (get-array-ref array index pos)
      (when (check-array-index array index pos)
        (if (< index 0)
            (vector-length array (+ (vector-length array) index))
            (vector-ref array index))))
    
    (define (get-array array [env env])
      (cond [(vector? array) array]
            [(eq? array 'null) 'null]
            [(not env) (error 'GET-ARRAY "我次奥 -- unknown array '~a'" (id-name array))]
            [(tree? array) (entity-value (get-entity (id-name array) env))]))
    
    (define (array v-list dest [env env])
      (let* ([elem-ty 'var]
             [arr (list->vector
                   (map
                    (lambda (e)
                      (let* ([val (get-value e)]
                             [type (type-info-value (get-type val))])
                        (cond [(eq? elem-ty type) val]
                              [(eq? elem-ty 'var) (set! elem-ty type) val]
                              [(eq? type 'null) val]
                              [else
                               ; 不相容的类型必然是 var
                               (error 'array "the elements' type are incompatible, ~a|~a, ~a"
                                      elem-ty type (tree-pos e))])))
                    v-list))])
        (when debug?
          (printf "<- array ->, array: ~a, dest: ~a, ~a\n"
                  arr (id-name dest) (id-pos dest)))
        (set-value! dest arr)))
    
    (define (array-ref array index dest)
      (let ([array (get-array array)]
            [index (get-value index)])
        (set-value! dest (get-array-ref array index (tree-pos dest)))))
    
    (define (array-set array-t index src)
      (let* ([array (get-array array-t)]
             [arr-type (type-info-value (get-type array-t))]
             [index (get-value index)]
             [val (get-value src)]
             [val-type (type-info-value (get-type val))])
        (define (check-type)
          (cond [(eq? arr-type val-type) #t]
                [(eq? val-type 'null) #t]
                [(eq? arr-type 'var) #t]
                [(or (and (eq? arr-type 'int) (eq? val-type 'real))
                     (and (eq? arr-type 'real) (eq? val-type 'int)))
                 #t]
                [else (error 'ARRAY-SET "can't set '~a' value to ~a array, ~a"
                             val-type arr-type (tree-pos array-t))]))
        (when (check-array-index array index (tree-pos array-t))
          (when (check-type)
            (let ([i (if (< index 0)
                         (+ index (vector-length array))
                         index)])
              (when (eq? arr-type 'var)
                (set-type! (id-name array-t) val-type))
              (cond [(and (eq? arr-type 'int) (eq? val-type 'real))
                     (vector-set! array i (exact-truncate val))]
                    [(and (eq? arr-type 'real) (eq? val-type 'int))
                     (vector-set! array i (+ val 0.0))]
                    [else (vector-set! array i val)]))))))
    
    (define (array-new type size dest)
      (let ([size (get-value size)])
        (cond [(or (eq? size 'null) (< size 0))
               (dump)
               (error 'NEW-ARRAY "Invalide size: ~a, size should >= 0, ~a."
                      size (tree-pos dest))]
              [else
               (debugger "&&& array-new, dest: ~a, ~a\n"
                         (id-name dest) (id-pos dest))
               (set-type! (id-name dest) (type-info-value type))
               (set-value! dest (make-vector size 'null))])))
    
    (define (array-read array index)
      (let ([val (read)])
        (array-set array index val)))
    
    (define (array-size array dest)
      (let ([arr (get-array array)])
        (set-value! dest
                    (if (eq? arr 'null)
                        'null
                        (vector-length arr)))))
    
    ; ======================= array instructions end ==========================
    ; read and computing instructions
    (define (read-value var)
      (let ([val (read)])
        (set-value! var val)))
    (define (print e)  (printf "~a" (get-value e)))
    
    (define (arith op args var)
      (define (check-type e)
        (match (get-type e)
          [(or (type-info 'atom 'int _) (type-info 'atom 'real _))
           #t]
          [x (error 'ARITH-ERROR "expect (real/int) type, given (~a) '~a', ~a."
                    (type-info-value x) (id-name e) (id-pos e))]))
      (when (list-and (map check-type args))
        (set-value! var
                    (eval (cons op (map get-value args))
                          (make-base-namespace)))))
    
    (define (comp-op op e1 e2 var)
      (define (check-type e1 e2)
        (let ([ty1 (get-type e1)] [ty2 (get-type e2)])
          (when debug?
            (printf "> comp-op <, left-type: ~a, right-type: ~a\n"
                    (type-info->string ty1) (type-info->string ty2)))
          (cond [(or (eq? (type-info-value ty1) 'null)
                     (eq? (type-info-value ty2) 'null))
                 'null]
                [(eq? (type-info-type ty1) (type-info-type ty2))
                 (cond [(eq? (type-info-value ty1) (type-info-value ty2))
                        ty1]
                       [(or (and (eq? (type-info-value ty1) 'int)
                                 (eq? (type-info-value ty2) 'real))
                            (and (eq? (type-info-value ty1) 'real)
                                 (eq? (type-info-value ty2) 'int)))
                        ty1]
                       [(eq? (type-info-value ty1) 'var)
                        (error 'TYPE-ERROR "Unknown value: '~a', ~a~%"
                               (id-name e1) (id-pos e1))]
                       [(eq? (type-info-value ty2) 'var)
                        (error 'TYPE-ERROR "Unknown value: '~a', ~a~%"
                               (id-name e2) (id-pos e2))]
                       [else (error 'COMP-OP "Can not compare '~a' with '~a', ~a~%"
                                    (type-info-value ty1) (type-info-value ty2)
                                    (if (tree? e1) (id-pos e1) (id-pos e2)))])]
                [else (error 'COMP-OP "Can not compare '~a' with '~a', ~a~%"
                             (type-info-type ty1) (type-info-type ty2)
                             (if (tree? e1) (id-pos e1) (id-pos e2)))])))
      (let* ([v1 (get-value e1)]
             [v2 (get-value e2)]
             [ty (check-type e1 e2)])
        (if (or (eq? v1 'null) (eq? v2 'null))
            (match op
              ['= (set-value! var (equal? v1 v2))]
              ['\# (set-value! var (not (equal? v1 v2)))]
              [_ (set-value! var #f)])
            (set-value! var ((get-comparer (type-info-type ty)
                                           (type-info-value ty)
                                           op)
                             v1 
                             v2)))))
    
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
            (match (get-type x)
              [(type-info 'atom 'string _) #t]
              [x (error 'STR-OP "'~a' is not a string, ~a~%"
                        (id-name x) (id-pos x))]))
          args)
         (set-value! var (apply string-append
                                (map get-value args)))]
        ['<-
         (for-each
          (lambda (x)
            (match (get-type x)
              [(type-info _ 'var _)
               (error 'STR-OP "Unknown value '~a', ~a~%"
                      (id-name x) (id-pos x))]
              [_ (void)]))
          args)
         (set-value! var (apply format
                                (map get-value args)))]))
    
    (define (push exp)
      (set! arg-stk (cons (get-value exp) arg-stk))
      (when debug?
        (printf "argument stack: ~a, pc=~a\n" arg-stk pc)))
    
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
             ; array relatively instructions
             ['array (apply array (cdr inst))]
             ['array-new (apply array-new (cdr inst))]
             ['array-ref (apply array-ref (cdr inst))]
             ['array-set (apply array-set (cdr inst))]
             ['array-read (apply array-read (cdr inst))]
             ['array-size (apply array-size (cdr inst))]
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

(define (exec [filename "../sample/test_array.pl"])
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (let ([code-ent (PL/T-generator t st)])
          (PL/T-machine (first code-ent)
                        (second code-ent)))
        (error 'exec "Something wrong!"))))

(display "\n**************\n")
(exec)
