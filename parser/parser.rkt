#lang racket
(require "../scanner/pl0-scanner.rkt" "const.rkt")
(provide PL/0-parser print-tree tree-content)

(define-struct tree (type content) #:mutable)


(define (PL/0-parser tokens)
  
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
          (error 'PL/0-parser "Error token: ~a, excepted: ~a, given: ~a line ~a:~a"
                 (second tok) tok-type (first tok) (third tok) (fourth tok)))))
 
  
  (define (report-error type)
    (error 'PL/0-parser
           "syntax error in (~a), token: ~a in L~a" type (get-token) (third (get-token))))

  (define (program)
    (let ([blk (block)])
      (begin
        (match! '\.)
        (if (null? tokens)
            (make-tree 'program blk)
            (report-error 'program)))))
  
  (define (block)
    (define (iter) ; for procedure
      (if (eq? (get-token-type) 'proc)
          (cons (procedure) (iter))
          '()))
    (let ([const null] [var null] [proc null])
      (begin (if (eq? (get-token-type) 'const) (set! const (const-init)) '())
             (if (eq? (get-token-type) 'var) (set! var (var-decl)) '())
             (if (eq? (get-token-type) 'proc) (set! proc (iter)) '())
             (make-tree 'block (list const var proc (statement))))))
  
  (define (const-init)
    (define (iter)
      (let* ([id (match! 'ident)] [init (match! *init*)] [num (match! 'number)]
             [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (list (second id) (second num)) (iter))]
          ['\; (list (list (second id) (second num)))]
          [_ (report-error 'const)])))
    (let* ([con (match! 'const)] [content (iter)])
      (make-tree 'const content)))
 
  (define (var-decl)
    (define (iter)
      (let* ([id (match! 'ident)] [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (second id) (iter))]
          ['\; (list (second id))]
          [_ (report-error 'var)])))
    (let* ([con (match! 'var)] [content (iter)])
      (make-tree 'var content)))
  
   (define (procedure)
    (let* ([proc-tok (match! 'proc)] [id-tok (match! 'ident)]
           [semi1 (match! '\;)] [blk (block)] [semi2 (match! '\;)])
      (make-tree 'proc (list (second id-tok) blk))))
  
  (define (statement)
    (match (get-token-type)
      ['begin (statement-begin)]
      ['call (statement-call)]
      ['print (statement-print)]
      ['if (statement-if)]
      ['while (statement-while)]
      ['ident (statement-assign)]
      ['\. (make-tree 'statement '())]
      [_ (report-error 'statement)]))
  
  (define (statement-begin)
    (define (iter)
      (match (get-token-type)
        ['\; (begin (match! '\;)
                    (cons (statement) (iter)))]
        ['end '()]
        [_ (report-error 'statement-begin)]))
    (let ([stmts null])
      (begin (match! 'begin)
             (set! stmts (cons (statement) (iter)))
             (match! 'end)
             (make-tree 'begin stmts))))  
  
  (define (statement-call)
    (let* ([call-tok (match! 'call)] [id (match! 'ident)])
      (make-tree 'call (second id))))
  
  (define (statement-print)
    (let* ([print-tok (match! 'print)] [expr (exp)])
      (make-tree 'print expr)))
    
  (define (statement-if)
    (let* ([if-tok (match! 'if)] [condi (condition)]
           [then-tok (match! 'then)] [stmt (statement)])
      (make-tree 'if (list condi stmt))))
  
  (define (statement-while)
    (let* ([while-tok (match! 'while)] [condi (condition)]
           [do-tok (match! 'do)] [stmt (statement)])
      (make-tree 'while (list condi stmt))))
  
  (define (statement-assign)
    (let* ([lhs (match! 'ident)]
           [ass-tok (match! 'assign)]
           [expr (exp)])
      (make-tree 'assign (list (second lhs) expr))))
    
           
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
      ['ident (make-tree 'ident (second (get-token!)))]
      ['\( (let ([expr null])
             (begin
               (match! '\() (set! expr (exp)) (match! '\))
               expt))]
      [#f '()]
      [_ (report-error 'factor)]))
  
  (if (null? tokens)
      null
      (program)))

(define (print-tree t [depth 0])
  (define (list-or l)
    (cond [(null? l) #f]
          [(= (length l) 1) (car l)]
          [(car l) #t]
          [else (list-or (cdr l))]))
  (define (to-string x) (format "~a" x))
  (define (flat-tree? t)
    (cond [(tree? (tree-content t)) #f]
          [(list? (tree-content t))
           (not (list-or (map tree? (tree-content t))))]
          [else #t]))
  (begin
    (printf "~a[~a" (make-string depth #\space) (tree-type t))
    (cond [(flat-tree? t)
           (printf " ~a" (to-string (tree-content t)))]
          [(tree? (tree-content t))
           (begin
             (printf "~%~a" (make-string (+ depth 1) #\space))
             (print-tree (tree-content t) (+ depth 1)))]
          [(list? (tree-content t))
           (for-each (lambda (x)
                       (cond [(tree? x)
                              (begin
                                (printf "~%~a" (make-string (+ depth 1) #\space))
                                (print-tree x (+ depth 1)))]
                             [(null? x) '()]
                             [(list? x)
                              (for-each (lambda (tr)
                                          (begin 
                                            (printf "~%~a" (make-string (+ depth 1) #\space))
                                            (print-tree tr (+ depth 1)))) x)]
                             [else (printf " ~a" (to-string x))]))
                     (tree-content t))])
    (printf "]")))
