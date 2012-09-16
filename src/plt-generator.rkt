#lang racket

; Code generator of PL/0
(require "util/table.rkt" "define.rkt")
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
    
    (define (new-temp)
      ; all temple variables start with '%'
      (let ([result (make-tree 'temp
                               (list
                                (string-append "%tmp"
                                               (number->string temp-counter))))])
        (set! temp-counter (add1 temp-counter))
        (add-code! (list 'decl (id-name result)))
        result))
    
    (define (temp? t)
      (and (tree? t) (eq? (tree-type t) 'temp)))
    
    ; gen-exp and gen-cond return the name of temp variable
    (define (iter ls op)
      (if (= (length ls) 1)
          (gen-exp (car ls))
          (let* ([left (gen-exp (first ls))]
                 [right (gen-exp (second ls))]
                 [temp (new-temp)])
            (add-code! (list op left right temp))
            (iter (cons temp (cddr ls)) op))))
    
    (define (gen-exp t)
      (define (primitive x) (match x ['- 0] ['/ 1]))      
      (if (temp? t) t
          (match (tree-type t)
            [(or 'true 'false) (tree-content t)]
            ['number (string->number (tree-content t))]
            ['ident t] ; position
            [(or '- '/)
             (if (= (length (tree-content t)) 1)
                 (let ([temp (new-temp)]
                       [op (tree-type t)])
                   (add-code!
                    (list op (primitive op)
                          (gen-exp (car (tree-content t)))
                          temp))
                   temp)
                 (iter (tree-content t) (tree-type t)))]
            [(or 'not (? cond-op?) (? logic-op?))
             (gen-cond t)]
            [(? arith-op?)
             (iter (tree-content t) (tree-type t))]
            [type (error 'exp "Unknown type '~a'~%" type)])))
    
    (define (gen-cond t)
      (match (tree-type t)
        [(or 'true 'false) (tree-content t)]
        ['ident t]
        ['not
         (let ([temp (new-temp)])
           (add-code!
            (list 'not (gen-exp (tree-content t)) '() temp))
           temp)]
        [(? cond-op?)
             (let* ([left (gen-exp (first (tree-content t)))]
                    [right (gen-exp (second (tree-content t)))]
                    [temp (new-temp)])
               (add-code!
                (list (tree-type t) left right temp))
               temp)]
        [(? logic-op?)
         (iter (tree-content t) (tree-type t))]))
         
    ; each gen-xxx statement return the end index of itself
    (define (gen-statement t)
      (when (tree? t)
        (match (tree-type t)
          ['begin (gen-begin t)]
          ['assign (gen-assign t)]        
          ['call (gen-call t)]
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
      (set! call-list
            (cons (add-code!
                   (list 'call
                         (car (tree-content (tree-content t)))))
                  call-list)))
    
    (define (gen-read t)
      (add-code! (list 'read (tree-content t))))
    
    (define (gen-print t)
      (add-code! (list 'print (gen-exp (tree-content t))))
      (add-code! (list 'print "\n")))
    
    (define (gen-assign t)
      (let ([id (car (tree-content t))]
            [e (gen-exp (second (tree-content t)))])
        (add-code! (list 'set e id))))
    
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
      (let ([entry (gen-block (second (tree-content t)))])
        (insert! (first (tree-content t)) 'entry entry)
        (add-code! (list 'return))
        entry))
    
    (define (gen-block t)
      (define (construct)
        ; initial const and variable
        (for-each (lambda (x)
                    (let ([id (car (tree-content (car x)))])
                      (add-code! (list 'decl id))
                      (add-code! (list 'set (lookup id 'value) (make-tree 'const (list id))))))
                  (if (tree? (first (tree-content t)))
                      (tree-content (first (tree-content t)))
                      '()))
        (for-each (lambda (x)
                    (let ([id (car (tree-content x))])
                      (add-code! (list 'decl id))))
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
    (let ([entry (gen-block (tree-content syntax-tree))])
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
        (format "~a" (if (eq? (tree-type t) 'temp)
                         (tree-content t)
                         (car (tree-content t))))
        t))
  (let f ([index 0])
    (when (< index (vector-length code-list))
      (printf "~a: ~a~%" index (map
                                tree->string
                                (vector-ref code-list index)))
      (f (add1 index)))))
