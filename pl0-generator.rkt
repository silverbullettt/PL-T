#lang racket

; Code generator of PL/0
(require "pl0-analyzer.rkt"
         "define.rkt")
(provide PL/0-generator show-code)

(define (PL/0-generator syntax-tree symbol-table)
  (let ([temp-counter 0] [pc -1] [code-list #()])
    
    (define lookup (symbol-table 'lookup))
    (define insert! (symbol-table 'insert!))
    
    (define (add-code! code)
      (set! code-list (vector-append code-list (vector code)))
      (set! pc (add1 pc))
      pc)
    
    (define (set-code! pos code)
      (vector-set! code-list pos code))
    
    (define (new-temp)
      ; all temple variables start with '@'
      (let ([result (string-append "@tmp"
                                   (number->string temp-counter))])
        (set! temp-counter (add1 temp-counter))
        (add-code! (list 'decl result))
        result))
    
    (define (temp? t)
      (and (string? t) (char=? (string-ref t 0) #\@)))
    
    ; gen-exp and gen-cond return the name of temp variable
    (define (gen-exp t)
      (define (iter ls)
        (if (= (length ls) 1)
            (gen-term (car ls))
            (let* ([left (gen-term (first ls))]
                   [op (second ls)]
                   [right (gen-term (third ls))]
                   [temp (new-temp)])
              (add-code! (list op left right temp))
              (iter (cons temp (cdddr ls))))))
      (iter (tree-content t)))
    
    (define (gen-term t)
      (define (iter ls)
        (if (= (length ls) 1)
            (gen-factor (car ls))
            (let* ([left (gen-factor (first ls))]
                   [op (second ls)]
                   [right (gen-factor (third ls))]
                   [temp (new-temp)])
              (add-code! (list op left right temp))
              (iter (cons temp (cdddr ls))))))
      (if (temp? t)
          t
          (iter (tree-content t))))
    
    (define (gen-factor t)
      (if (temp? t)
          t
          (match (tree-type t)
            ['ident (first (tree-content t))]
            ['number (string->number (tree-content t))]
            ['exp (gen-exp t)]
            [_ (error "不可能！")])))
    
    ; each gen-xxx statement return the end index of itself
    (define (gen-statement t)
      (when (tree? t)
        (match (tree-type t)
          ['begin (gen-begin t)]
          ['assign (gen-assign t)]        
          ['call (gen-call t)]
          ['print (gen-print t)]
          ['if (gen-if t)]
          ['while (gen-while t)]
          [_ (error "还没实现呢")])))
    
    (define (gen-begin t)
      (for-each gen-statement
                (tree-content t))
      pc)
    
    (define (gen-call t)
      (add-code! (list 'call
                       (lookup (first (tree-content (tree-content t))) 'entry))))
    
    (define (gen-print t)
      (add-code! (list 'print (gen-exp (tree-content t)))))
    
    (define (gen-assign t)
      (let ([id (car (tree-content (car (tree-content t))))]
            [e (gen-exp (second (tree-content t)))])
        (add-code! (list 'set e id))))
    
    (define (gen-cond t)
      (if (eq? (tree-type t) 'odd)
          (let ([e (gen-exp (tree-content t))]
                [tmp (new-temp)])
            (add-code! (list 'odd e tmp))
            tmp)
          (let ([l-exp (gen-exp (first (tree-content t)))]
                [r-exp (gen-exp (second (tree-content t)))]
                [tmp (new-temp)])
            (add-code! (list (tree-type t) l-exp r-exp tmp))
            tmp)))
    
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
      (let ([entry (add1 pc)])
        (insert! (first (tree-content t)) 'entry entry)
        (gen-block (second (tree-content t)))
        (add-code! (list 'return))
        entry))
    
    (define (gen-block t)
      (define (construct)
        ; initial const and variable
        (for-each (lambda (x)
                    (let ([id (car (tree-content (car x)))])
                      (add-code! (list 'decl id))
                      (add-code! (list 'set (lookup id 'value) id))))
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
      (list code-list entry))))

(define (show-code code-list)
  (let f ([index 0])
    (when (< index (vector-length code-list))
      (printf "~a: ~a~%" index (vector-ref code-list index))
      (f (add1 index)))))
