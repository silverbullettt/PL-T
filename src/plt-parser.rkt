#lang racket
(require "plt-scanner.rkt" "define.rkt" "util/utility.rkt")
(provide PL/T-parser print-tree)

; The syntax parser of PL/T

; program = block ".".
;
; block = [ "const" ident "=" number { "," ident "=" number} ";" ]
;         [ "var" ident { "," ident } ";" ]
;         { "procedure" ident ";" block ";" } statement.
;
; statement = [ ident ":=" expression | "call" ident |
;             "begin" statement {";" statement } "end" |
;             "if" condition "then" statement |
;             "while" condition "do" statement |
;             "print" expression | "read" ident ].
;
; condition = ident | "true | "false " |
;             "(" ["="|"#"|"<"|"<="|">"|">="] arithmetic arithmetic ")" |
;             "(" "not" condition ")" | "(" ["and"|"or"] condition { condition } ")".
;
; arithmetic = ident | number |
;              "(" ["+"|"-"|"*"|"/"] arithmetic { arithmetic } ")".
;
; expression = ident | number | "true" | "false" |
;             "(" "not" condition ")" |
;             "(" ["and"|"or"] condition { condition } ")"
;             "(" ["="|"#"|"<"|"<="|">"|">="] arithmetic arithmetic ")
;             "(" ["+"|"-"|"*"|"/"] arithmetic { arithmetic } ")".


(define (PL/T-parser tokens)
  
  (define (get-type tok) (first tok))
  (define (get-token-type) (if (null? tokens)
                               #f
                               (get-type (first tokens))))
  ; get type of second token
  (define (get-token2-type) (if (null? (cdr tokens))
                                #f
                                (get-type (second tokens))))
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
      ['read (statement-read)]
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
  
  (define (statement-read)
    (let* ([read-tok (match! 'print)] [id (match! 'ident)])
      (make-tree 'read (make-id id))))
  
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
  
  (define logic-op? (member-tester *logic-op*))
  (define cond-op? (member-tester *cond-op*))
  
  (define (condition)
     (define (iter)
       (if (eq? (get-token-type) '\))
           (begin (match! '\)) '())
           (cons (exp) (iter))))
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      ['true (get-token!) (make-tree 'true #t)]
      ['false (get-token!) (make-tree 'false #f)]
      ['\(
       (match! '\()       
       (match (get-token-type)
         ['not
          (let ([result #f])
            (match! 'not)
            (set! result (make-tree 'not (exp)))
            (match! '\))
            result)]
         [(? logic-op?)
          (let ([op (get-type (get-token!))])
            (make-tree op (iter)))]
          [(? cond-op?)
           (let* ([op (get-type (get-token!))]
                  [l-arith (arithmetic)]
                  [r-arith (arithmetic)])
             (match! '\))
             (make-tree op (list l-arith r-arith)))]
          [_ (report-error 'condition (append *cond-op* *logic-op* '(not)))])]
       [_ (report-error 'condition '(true false ident \())]))
  
  (define (arithmetic)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (arithmetic) (iter))))
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      ['number (make-tree 'number (second (get-token!)))]
      ['\(
       (match! '\()
       (if (member (get-token-type) *arith-op*)
           (let ([op (get-type (get-token!))])
             (make-tree op (iter)))
           (report-error 'arithmetic *arith-op*))]
      [_ (report-error 'arithmetic '(number ident \())]))
  
  (define arith-op? (member-tester (append *arith-op* '(number))))
  (define cond? (member-tester
                 (append *logic-op* *cond-op* '(not true false))))
  
  (define (exp)
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      [(? arith-op?) (arithmetic)]
      [(? cond?) (condition)]
      ['\(
       (match (get-token2-type)
         [(? arith-op?) (arithmetic)]
         [(? cond?) (condition)]
         [_ (report-error 'exp '(arith-op condition-op))])]
      [_ (report-error 'exp '(number true false ident \())]))
  
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
