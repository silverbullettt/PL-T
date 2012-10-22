#lang racket
; Code generator of PL/0
(require "util/table.rkt" "util/utility.rkt" "define.rkt")
(provide PL/T-generator print-code)

(define debug? #f)

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
    
    (define (new-temp [type (make-type-info 'atom 'var #f)]
                      [value 'null]
                      [pos #f])
      ; all temple variables start with '%'
      (let ([result (make-tree 'temp
                               (string-append "%tmp"
                                              (number->string temp-counter))
                               pos)])
        (set! temp-counter (add1 temp-counter))
        (add-code! (list 'decl (id-name result) type value))
        result))
    
    (define (temp? t)
      (and (tree? t) (eq? (tree-type t) 'temp)))
    
    (define const? (member-tester '(int real bool string null const-array)))
    (define (const-tree-value t)
      (define (const-array t)
        (apply vector
               (map const-tree-value (second (tree-content t)))))
      (match (tree-type t)
        [(or 'int 'real)
         (when debug?
           (printf "[[[ ~a -> ~a, ~a ]]]\n"
                   (tree-content t)
                   (string->number (tree-content t))
                   (tree-pos t)))
         (string->number (tree-content t))]
        ['bool (string=? (tree-content t) "#t")]
        ['string (tree-content t)]
        ['null 'null]
        ['const-array (const-array t)]
        [x (error 'const-tree-value "Unknown type '~a', ~a.~%"
                  x (tree-pos t))]))
    
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
         (let ([temp (new-temp retv)]) ; TODO!!!
           (add-code! (list 'pop temp))
           temp))
       (get-ret-type-by-name (id-name (first (tree-content t))))))
    
    (define (gen-size t)
      (let ([arr (gen-array (tree-content t))]
            [size (new-temp (type-info 'atom 'int #f)
                            'null (tree-pos t))])
        (add-code! (list 'array-size arr size))
        size))
    
    (define (gen-array t)
      (when debug?
        (display t)
        (newline))
      (match (tree-type t)
        ['ident t]
        ['temp t]
        ['new-array
         (let* ([type (type-info 'array (first (tree-content t)) #f)]
                [arr (new-temp type 'null (tree-pos t))]
                [size (gen-exp (second (tree-content t)))])
           (add-code! (list 'array-new type size arr))
           arr)]
        ['temp-array
         (let* ([arr (new-temp (type-info 'array 'var #f)
                               'null (tree-pos t))]
                [elems (gen-exp-list (tree-content t))])
           (add-code! (list 'array elems arr))
           arr)]))
    
    (define (gen-array-ref t)
      (let ([index (gen-exp (second (tree-content t)))]
            [temp (new-temp (type-info 'atom 'var #f)
                            'null (tree-pos t))])
        (add-code! (list 'array-ref
                         (first (tree-content t)) index temp))
        temp))
    
    (define (gen-exp t)
      (if (temp? t) t
          (match (tree-type t)
            [(? const?) (const-tree-value t)]
            ['ident t] ; position
            ['call (car (get-call-retv t))]
            ['size (gen-size t)]
            ['array-ref (gen-array-ref t)]
            [(? array?) (gen-array t)]
            [(? arith-op?)
             (let ([temp (new-temp (type-info 'atom 'var #f)
                                   'null
                                   (tree-pos t))])
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
        ['bool (const-tree-value t)]
        ['ident t]
        ['array-ref (gen-array-ref t)]
        ['call (car (get-call-retv t))]
        [(? comp-op?)
         (let* ([left (gen-exp (first (tree-content t)))]
                [right (gen-exp (second (tree-content t)))]
                [temp (new-temp (type-info 'atom 'bool #f)
                                'null (tree-pos t))])
           (add-code!
            (list (tree-type t) left right temp))
           temp)]
        ['not
         (let ([temp (new-temp (type-info 'atom 'bool #f)
                                'null (tree-pos t))])
           (add-code!
            (list 'not (gen-exp (tree-content t)) temp))
           temp)]
        [(? logic-op?)
         (let ([temp (new-temp (type-info 'atom 'bool #f)
                               'null (tree-pos t))])
           (add-code!
            (list (tree-type t)
                  (map gen-exp (tree-content t))
                  temp))
           temp)]
        [type (error 'cond "Unknown type '~a'~%" type)]))
    
    (define (gen-str t)
      (match (tree-type t)
        ['string (const-tree-value t)]
        ['ident t]
        ['array-ref (gen-array-ref t)]
        ['call (car (get-call-retv t))]
        ['@
         (let ([temp (new-temp (type-info 'atom 'string #f)
                               'null (tree-pos t))])
           (add-code!
            (list '@ (map gen-str (tree-content t)) temp))
           temp)]
        ['<-
         (let ([temp (new-temp (type-info 'atom 'string #f)
                               'null (tree-pos t))]
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
          ['foreach (gen-foreach t)]
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
      (match (tree-type (tree-content t))
        ['ident (add-code! (list 'read (tree-content t)))]
        ['array-ref
         (let ([array (first (tree-content (tree-content t)))]
               [index (gen-exp (second (tree-content (tree-content t))))])
           (add-code! (list 'array-read array index)))]))
    
    (define (gen-print t)
      (define (iter elist)
        (cond [(null? elist) (void)]
              [(null? (cdr elist))
               (add-code! (list 'print (car elist)))]
              [else
               (add-code! (list 'print (car elist)))
               (add-code! (list 'print " "))
               (iter (cdr elist))]))
      (iter (gen-exp-list (tree-content t)))
      (add-code! (list 'print "\n")))
    
    (define (gen-assign t)
      (for-each
       (lambda (var exp)
         (match (tree-type var)
           ['ident (add-code! (list 'set var exp))]
           ['array-ref
            (let ([index (gen-exp (second (tree-content var)))])
              (add-code! (list 'array-set
                               (first (tree-content var)) index exp)))]))
       (first (tree-content t))
       (gen-exp-list (second (tree-content t)))))
    
    (define (gen-if t)
      (let ([cond-res (gen-cond (first (tree-content t)))]
            [if-pos (add-code! '())])
        (gen-statement (second (tree-content t)))
        (set-code! if-pos (list 'if-false cond-res (add1 pc)))
        pc))
    
    (define (gen-while t)
      (let* ([start-pos (add1 pc)]
             [cond-res (gen-cond (first (tree-content t)))]
             [while-pos (add-code! '())])
        (gen-statement (second (tree-content t)))
        (add-code! (list 'goto start-pos))
        (set-code! while-pos (list 'if-false cond-res (add1 pc)))
        pc))
    
    (define (gen-foreach t)
      (when debug?
        (newline)
        (print-tree (first (tree-content t)))
        (newline)
        (print-tree (second (tree-content t))))
      (let* ([arr (gen-array (second (tree-content t)))]
             [index (new-temp (type-info 'atom 'int #f) 0)]
             [inc (new-temp (type-info 'atom 'int #t) 1)]
             [size (gen-size (tree 'size arr #f))]
             [comp-res (new-temp (type-info 'atom 'bool #f))]
             [start-pos (add1 pc)]
             [comp (add-code! (list '< index size comp-res))]
             [for-pos (add-code! '())])
        (add-code! (list 'set
                         (first (tree-content t))
                         (gen-array-ref
                          (tree 'array-ref (list arr index) #f))))
        (gen-statement (third (tree-content t)))
        (add-code! (list '+ (list index inc) index))
        (add-code! (list 'goto start-pos))
        (set-code! for-pos (list 'if-false comp-res (add1 pc)))
        pc))
    
    (define (gen-proc t)
      (let ([entry (gen-block (third (tree-content t))
                              (caadr (tree-content t)))])
        (insert! (id-name (first (tree-content t))) 'entry entry)
        (when (null? (cadadr (tree-content t)))
          (add-code! (list 'return)))
        (when debug?
          (printf "~a, ~a\n" (first (tree-content t)) entry))
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
                [(eq? (tree-type (second x)) 'type) ; var x:type
                 (add-code! (list 'decl (tree-content (first x))
                                  (lookup (tree-content (first x)) 'type)))]
                [else ; var x(exp)
                 (add-code! (list 'decl (tree-content (first x))
                                  (lookup (tree-content (first x)) 'type)
                                  (gen-exp (second x))))]))
        
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
      (when debug? (print-st symbol-table))
      (list code-list entry))))


(define (print-code code-list)
  (define (to-string t)
    (cond [(tree? t) (format "[~a]" (to-string (tree-content t)))]
          [(type-info? t) (format "<~a ~a>"
                                  (type-info-type t)
                                  (type-info-value t))]
          [(list? t) (format "(~a)" (string-join (map to-string t)))]
          [(string? t)
           (if (string-endswith? t (string #\newline))
               (format "~a~a"
                       (substring t 0 (- (string-length t) 1))
                       "#newline")
               t)]
          [else (format "~a" t)]))
  (let f ([index 0])
    (when (< index (vector-length code-list))
      (printf "~a: ~a~%" index (map
                                to-string
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

(define (gen [filename "../sample/test_array.pl"])
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (PL/T-generator t st)
        #f)))

(print-tree (parse "../sample/test_array.pl"))

(define t (parse "../sample/test_array.pl"))
(print-tree t)
(define st (PL/T-analyzer t))

(define code (PL/T-generator t st))
(print-code (car code))
