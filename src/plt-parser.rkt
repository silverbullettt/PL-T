#lang racket
(require "plt-scanner.rkt" "define.rkt")
(provide PL/T-parser print-tree)

; The syntax parser of PL/T

; program = block "." .
;
; block = [ "const" ident "=" number {"," ident "=" number} ";"]
;         [ "var" ident {"," ident} ";"]
;         { "procedure" ident ";" block ";" } statement .
;
; statement = [ ident ":=" expression | "call" ident |
;             "begin" statement {";" statement } "end" |
;             "if" condition "then" statement |
;             "while" condition "do" statement ].
;
; condition = "odd" expression |
;             expression ("="|"#"|"<"|"<="|">"|">=") expression .
;
; expression = [ "+"|"-"] term { ("+"|"-") term}.
;
; term = factor {("*"|"/") factor}.
;
; factor = ident | number | "(" expression ")".


(define (PL/T-parser tokens)
  
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

  (define (program)
    (let ([blk (block)])
      (match! '\.)
        (if (null? tokens)
            (make-tree 'program blk)
            (report-error 'program))))
  
  (define (block)
    (define (iter) ; for procedure
      (if (eq? (get-token-type) 'proc)
          (cons (procedure) (iter))
          '()))
    (let ([const null] [var null] [proc null])
      (if (eq? (get-token-type) 'const) (set! const (const-init)) '())
      (if (eq? (get-token-type) 'var) (set! var (var-decl)) '())
      (if (eq? (get-token-type) 'proc) (set! proc (iter)) '())
      (make-tree 'block (list const var proc (statement)))))
  
  (define (const-init)
    (define (iter)
      (let* ([id (match! 'ident)] [init (match! *init*)] [num (match! 'number)]
             [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (list (make-id id) (second num)) (iter))]
          ['\; (list (list (make-id id) (second num)))]
          [_ (report-error 'const '(\, \;) delim)])))
    (let* ([con (match! 'const)] [content (iter)])
      (make-tree 'const content)))
 
  (define (var-decl)
    (define (iter)
      (let* ([id (match! 'ident)] [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (make-id id) (iter))]
          ['\; (list (make-id id))]
          [_ (report-error 'var '(\, \;) delim)])))
    (let* ([con (match! 'var)] [content (iter)])
      (make-tree 'var content)))
  
   (define (procedure)
    (let* ([proc-tok (match! 'proc)] [id-tok (match! 'ident)]
           [semi1 (match! '\;)] [blk (block)] [semi2 (match! '\;)])
      (make-tree 'proc (list (make-id id-tok) blk))))
      ;(make-tree 'proc (list (second id-tok) blk))))
  
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
        [_ (report-error 'statement-begin '(\; end))]))
    (let ([stmts null])
      (match! 'begin)
      (set! stmts (cons (statement) (iter)))
      (match! 'end)
      (make-tree 'begin stmts)))
  
  (define (statement-call)
    (let* ([call-tok (match! 'call)] [id (match! 'ident)])
      (make-tree 'call (make-id id))))
  
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
      (make-tree 'assign (list (make-id lhs) expr))))
  
  
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
  
  (if (null? tokens)
      null
      (program)))


(define (print-tree t [depth 0])
  (define (list-or l)
    (cond [(null? l) #f]
          [(= (length l) 1) (car l)]
          [(car l) #t]
          [else (list-or (cdr l))]))
  (define (to-string x)
    (cond [(tree? x)
           (format "[~a ~a]" (tree-type x) (to-string (tree-content x)))]
          [(list? x)
           (map to-string x)]
          [else (format "~a" x)]))
  (define (flat-tree? t)
    (cond [(tree? (tree-content t)) #f]
          [(list? (tree-content t))
           (not (list-or (map tree? (tree-content t))))]
          [else #t]))
  (printf "~a[~a" (make-string depth #\space) (tree-type t))
  (cond [(flat-tree? t)
         (printf " ~a" (to-string (tree-content t)))]
        [(tree? (tree-content t))
         (printf "~%~a" (make-string (add1 depth) #\space))
         (print-tree (tree-content t) (add1 depth))]
        [(list? (tree-content t))
         (for-each (lambda (x)
                     (cond [(tree? x)
                            (printf "~%~a" (make-string (add1 depth) #\space))
                            (print-tree x (add1 depth))]
                           [(null? x) '()]
                           [(list? x)
                            (for-each (lambda (e)
                                        (printf "~%~a" (make-string (add1 depth) #\space))
                                        (if (tree? e)
                                            (print-tree e (add1 depth))
                                            (printf " ~a" (to-string e)))) x)]
                           [else (printf " ~a" (to-string x))]))
                   (tree-content t))])
  (printf "]"))
