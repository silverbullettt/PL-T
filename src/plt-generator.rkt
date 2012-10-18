#lang racket

; Code generator of PL/0
(require "util/table.rkt" "util/utility.rkt" "define.rkt")
(provide PL/T-generator print-code)

(define (PL/T-generator syntax-tree symbol-table)
  (let ([temp-counter 0] [pc -1] [code-list #()] [call-list '()])
    
    (define lookup (symbol-table 'lookup))
    (define insert! (symbol-table 'insert!))
    
    (define (add-code! code)
      (set! code-list (vector-append code-list (vector code)))
      (set! pc (add1 pc))
      pc)
    (define (set-code! pos code)
      (vector-set! code-list pos code))
    
    (define (get-pos id-tree)
      (second (tree-content id-tree)))
    
    (define (new-temp [pos #f])
      ; all temple variables start with '%'
      (let ([result (make-tree 'temp
                               (string-append "%tmp"
                                               (number->string temp-counter))
                               pos)])
        (set! temp-counter (add1 temp-counter))
        (add-code! (list 'decl (id-name result)))
        result))
    
    (define (temp? t)
      (and (tree? t) (eq? (tree-type t) 'temp)))
    
    (define const? (member-tester '(int real bool string null)))
    (define (const-tok-value tok)
      (match (tree-type tok)
        [(or 'int 'real) (string->number (tree-content tok))]
        ['bool (let ([val (tree-content tok)])
                 (if (boolean? val)
                     val
                     (string=? "#t" val)))]
        ['string (tree-content tok)]
        ['null 'null]
        [x (error 'const-tok-value "Unknown type '~a'" x)]))
    
    (define (gen-exp-list exp-list)
      (flatten (map
                (lambda (x)
                  (if (eq? (tree-type x) 'call)
                      (get-call-retv x)
                      (gen-exp x)))
                exp-list)))
    
    (define (get-ret-type-by-name name)
      (second (type-info-value (lookup name 'type))))
    
    (define (get-call-retv t)
      (gen-call t)
      (map
       (lambda (retv)
         (let ([temp (new-temp)])
           (add-code! (list 'pop temp))
           temp))
       (get-ret-type-by-name (id-name (first (tree-content t))))))
    
    (define (gen-exp t)
      (if (temp? t) t
          (match (tree-type t)
            [(? const?) (const-tok-value t)]
            ['ident t] ; position
            ['call (car (get-call-retv t))]
            [(? arith-op?)
             (let ([temp (new-temp)])
               (add-code!
                (list (tree-type t)
                      (map gen-exp (tree-content t))
                      temp))
               temp)]
            [(or (? comp-op?) (? logic-op?)) (gen-cond t)]
            [(? str-op?) (gen-str t)]
            [type (error 'exp "Unknown type '~a'~%" type)])))
    
    (define (gen-cond t)
      (match (tree-type t)
        ['bool (const-tok-value t)]
        ['ident t]
        ['call (car (get-call-retv t))]
        [(? comp-op?)
             (let* ([left (gen-exp (first (tree-content t)))]
                    [right (gen-exp (second (tree-content t)))]
                    [temp (new-temp)])
               (add-code!
                (list (tree-type t) left right temp))
               temp)]
        ['not
         (let ([temp (new-temp)])
           (add-code!
            (list 'not (gen-exp (tree-content t)) temp))
           temp)]
        [(? logic-op?)
         (let ([temp (new-temp)])
           (add-code!
            (list (tree-type t)
                  (map gen-exp (tree-content t))
                  temp))
           temp)]
        [type (error 'cond "Unknown type '~a'~%" type)]))
    
    (define (gen-str t)
      (match (tree-type t)
        ['string (const-tok-value t)]
        ['ident t]
        ['call (car (get-call-retv t))]
        ['@
         (let ([temp (new-temp)])
           (add-code!
            (list '@ (map gen-str (tree-content t)) temp))
           temp)]
        ['<-
         (let ([temp (new-temp)]
               [str (gen-str (car (tree-content t)))])
           (add-code!
            (list '<- 
                  (cons str
                        (map gen-exp (cdr (tree-content t))))
                  temp))
           temp)]
        [type (error 'str "Unknown type '~a'~%" type)]))
    
    ; each gen-xxx statement return the end index of itself
    (define (gen-statement t)
      (when (tree? t)
        (match (tree-type t)
          ['begin (gen-begin t)]
          ['assign (gen-assign t)]        
          ['call (gen-call t)]
          ['return (gen-return t)]
          ['read (gen-read t)]
          ['print (gen-print t)]
          ['if (gen-if t)]
          ['while (gen-while t)]
          [x (error "'~a' 还没实现呢" x)])))
    
    (define (gen-begin t)
      (for-each gen-statement
                (tree-content t))
      pc)
    
    (define (gen-call t)
      (for-each
       (lambda (arg)
         (add-code! (list 'push arg)))
       (reverse (gen-exp-list (second (tree-content t)))))
      (set! call-list
            (cons (add-code!
                   (list 'call
                         (tree-content (first (tree-content t)))))
                  call-list)))
    
    (define (gen-return t)
      (for-each
       (lambda (tmp)
         (add-code! (list 'push tmp)))
       (reverse (gen-exp-list (tree-content t))))
      (add-code! (list 'return)))
    
    (define (gen-read t)
      (add-code! (list 'read (tree-content t))))
    
    (define (gen-print t)
      (for-each (lambda (e)
                  (add-code! (list 'print e))
                  (add-code! (list 'print " ")))
                (gen-exp-list (tree-content t)))
      (add-code! (list 'print "\n")))
    
    (define (gen-assign t)
      (for-each
       (lambda (var exp)
         (add-code! (list 'set var exp)))
       (first (tree-content t))
       (gen-exp-list (second (tree-content t)))))
    
    (define (gen-if t)
      (let ([cond-res (gen-cond (first (tree-content t)))]
            [if-pos (add-code! '())])
        (gen-statement (second (tree-content t)))
        (set-code! if-pos (list 'if-false cond-res (add1 pc)))
        pc))
    
    (define (gen-while t)
      (let ([start-pos (add1 pc)]
            [cond-res (gen-cond (first (tree-content t)))]
            [while-pos (add-code! '())])
        (gen-statement (second (tree-content t)))
        (add-code! (list 'goto start-pos))
        (set-code! while-pos (list 'if-false cond-res (add1 pc)))
        pc))
    
    (define (gen-proc t)
      (let ([entry (gen-block (third (tree-content t))
                              (caadr (tree-content t)))])
        (insert! (first (tree-content t)) 'entry entry)
        (when (null? (cadadr (tree-content t)))
          (add-code! (list 'return)))
        ;(printf "~a, ~a\n" (first (tree-content t)) entry)
        entry))
    
    (define (gen-block t args)
      (define (construct)
        ; initial const and variable
        (define (gen-const t)
          (let* ([id (tree-content (car t))]
                 [ty (lookup id 'type)]
                 [val (lookup id 'value)])
            (add-code! (list 'decl id ty val))))
        (define (gen-var x)
          (cond [(tree? x) ; var x;
                 (add-code! (list 'decl (id-name x)))]
                [(tree? (second x)) ; var x:exp;
                 (add-code! (list 'decl (tree-content (car x))
                                  (lookup (tree-content (car x)) 'type)
                                  (gen-exp (second x))))]
                [else ; var x:type;
                 (add-code! (list 'decl (tree-content (car x))
                                  (lookup (tree-content (car x)) 'type)))]))
        
        (for-each gen-var args)
        (for-each (lambda (x)
                    (add-code!
                     (list 'pop (if (tree? x) x (car x)))))
                  args)
        (for-each gen-const
                  (if (tree? (first (tree-content t)))
                      (tree-content (first (tree-content t)))
                      '()))
        (for-each gen-var
                  (if (tree? (second (tree-content t)))
                      (tree-content (second (tree-content t)))
                      '())))
      (let ([entry #f])
        (for-each gen-proc (if (null? (third (tree-content t)))
                               '()
                               (third (tree-content t))))
        (set! entry (add1 pc))
        (construct)
        (gen-statement (fourth (tree-content t)))
        entry))
    (let ([entry (gen-block (tree-content syntax-tree) '())])
      ; 设置未知地址的 call
      (for-each
       (lambda (i)
         (let ([inst (vector-ref code-list i)])
           (vector-set! code-list
                        i
                        (list 'call (lookup (second inst) 'entry)))))
       call-list)
      (list code-list entry))))


(define (print-code code-list)
  (define (tree->string t)
    (if (tree? t)
        (format "~a" (tree-content t))
        t))
  (let f ([index 0])
    (when (< index (vector-length code-list))
      (printf "~a: ~a~%" index (map
                                tree->string
                                (vector-ref code-list index)))
      (f (add1 index)))))

; ============================ for test =======================================

(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
         "PLT-parser.rkt"
         "PLT-analyzer.rkt"
         "define.rkt"
         "util/table.rkt")

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (parse [filename "../sample/test.pl"])
  (PL/T-parser
   (PL/T-scanner
    (read-string-from-file filename))))

(define (analyse [filename "../sample/test.pl"])
  (PL/T-analyzer
   (PL/T-parser
    (PL/T-scanner
     (read-string-from-file filename)))))

(print-tree (parse "../sample/test_null.pl"))

(define t (parse "../sample/test_null.pl"))
(print-tree t)
(define st (PL/T-analyzer t))

(define code (PL/T-generator t st))
(print-code (car code))
