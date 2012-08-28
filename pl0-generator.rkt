#lang racket

; Code generator of PL/0
(require "pl0-scanner.rkt"
         "pl0-parser.rkt"
         "pl0-analyzer.rkt"
         "define.rkt" "util/utility.rkt")
(provide PL/0-generator)

(define (exp-parser tokens)
  
  (define (get-type tok) (first tok))
  (define (get-token-type) (if (null? tokens)
                               #f
                               (get-type (first tokens))))
  (define (get-token)
    (if (null? tokens) #f  (first tokens)))
  (define (get-token!)
    (let ([tok (get-token)])
      (begin (set! tokens (cdr tokens)) tok)))
  
  (define (match? tok-type)
    (eq? tok-type (first (get-token))))
  (define (match! tok-type)
    (let ([tok (get-token)])
      (if (match? tok-type)
          (get-token!)
          (error 'PL/0-parser "Error token: ~a, excepted: ~a, given: ~a -- L~a:~a"
                 (second tok) tok-type (first tok) (third tok) (fourth tok)))))
 
  (define (get-id-inf tok)
    (list (second tok) (cons (third tok) (fourth tok))))
  (define (make-id tok)
    (make-tree 'ident (get-id-inf tok)))
  (define (report-error type expected [tok (get-token)])
    (error 'PL/0-parser
           "syntax error in (~a), excepted: ~a, given: ~a -- L~a:~a"
           type expected (second tok) (third tok) (fourth tok)))

  (define (condition)
    (if (eq? (get-token-type) 'odd)
        (make-tree (get-type (match! 'odd)) (exp))
        (let* ([l-exp (exp)] [cond-op (get-type (get-token!))] [r-exp (exp)])
          (make-tree cond-op (list l-exp r-exp)))))
  
  (define (term-op)
    (if (or (eq? (get-token-type) *plus*)
            (eq? (get-token-type) *minus*))
        (get-type (get-token!))
        '()))
  
  (define (exp)
    (define (iter)
      (if (get-token)
          (let ([op (term-op)])
            (if (null? op)
                '()
                (cons op (cons (term) (iter)))))
          '()))
    (let* ([first-op (term-op)] [l-term (term)])
      (make-tree 'exp
                 (if (null? first-op)
                     (cons l-term (iter))
                     (cons first-op (cons l-term (iter)))))))
  
  (define (term)
    (define (iter)
      (if (get-token)
          (if (or (eq? (get-token-type) *times*)
                  (eq? (get-token-type) *over*))
              (cons (get-type (get-token!))
                    (cons (factor) (iter)))
              '())
          '()))
    (make-tree 'term (cons (factor) (iter))))
  
  (define (factor)
    (match (get-token-type)
      ['number (make-tree 'number (second (get-token!)))]
      ['ident (make-id (match! 'ident))]
      ['\( (let ([expr null])
             (match! '\() (set! expr (exp)) (match! '\))
             expr)]
      [#f '()]
      [_ (report-error 'factor '(number ident \())]))

  (exp))

(print-tree (exp-parser (PL/0-scanner "1+2+3+4")))
(newline)

(define (PL/0-generator syntax-tree)
  (let (;[symbol-table (PL/0-analyzer syntax-tree)]
        [temp-counter 0]
        [code-list '()])
    
    (define (new-temp)
      ; all temple variables start with '@'
      (let ([result (string-append "@t"
                                   (number->string temp-counter))])
        (set! temp-counter (add1 temp-counter))
        result))
    
    (define (temp? t)
      (and (string? t) (char=? (string-ref t 0) #\@)))
    
    (define (gen-exp t)
      (define (iter ls)
        (if (= (length ls) 1)
            (gen-term (car ls))
            (let* ([left (gen-term (first ls))]
                   [op (second ls)]
                   [right (gen-term (third ls))]
                   [temp (new-temp)])
              (set! code-list
                    (append code-list (list (list op left right temp))))
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
              (set! code-list
                    (append code-list (list (list op left right temp))))
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
    
    (list (gen-exp syntax-tree) code-list)))

(first
 (PL/0-generator (exp-parser (PL/0-scanner "1+2*3+4/(5-6)"))))

(second
 (PL/0-generator (exp-parser (PL/0-scanner "1+2*3+4/(5-6)"))))
