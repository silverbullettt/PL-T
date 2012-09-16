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
; statement = [ var-list ":=" exp-list | "call" ident |
;             "begin" statement {";" statement } "end" |
;             "if" condition "then" statement |
;             "while" condition "do" statement |
;             "print" exp-list | "read" ident ].
;
; var-list = ident { , ident }
;
; exp-list = expression { , expression }
;
; condition = ident | "true | "false " |
;             "(" ["="|"#"|"<"|"<="|">"|">="] arithmetic arithmetic ")" |
;             "(" "not" condition ")" | "(" ["and"|"or"] condition { condition } ")".
;
; arithmetic = ident | number |
;              "(" ["+"|"-"|"*"|"/"] arithmetic { arithmetic } ")".
;
; expression = condition | arithmetic.


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
  
  (define (new-tree type content [pos #f])
    (make-tree type content pos))
  (define (get-id-inf tok)
    (list (second tok) (cons (third tok) (fourth tok))))
  (define (make-id tok)
    (new-tree 'ident (second tok) (cons (third tok) (fourth tok))))
  (define (report-error type expected [tok (get-token)])
    (error 'PL/0-parser
           "syntax error in (~a), excepted: ~a, given: ~a -- L~a:~a"
           type expected (second tok) (third tok) (fourth tok)))
  
  
  (define (program)
    (let ([blk (block)])
      (match! '\.)
        (if (null? tokens)
            (new-tree 'program blk)
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
      (new-tree 'block (list const var proc (statement)))))
  
  (define (const-init)
    (define (iter)
      (let* ([id (match! 'ident)] [init (match! *init*)] [num (match! 'number)]
             [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (list (make-id id) (second num)) (iter))]
          ['\; (list (list (make-id id) (second num)))]
          [_ (report-error 'const '(\, \;) delim)])))
    (let* ([con (match! 'const)] [content (iter)])
      (new-tree 'const content)))
 
  (define (var-decl)
    (define (iter)
      (let* ([id (match! 'ident)] [delim (get-token!)])
        (match (get-type delim)
          ['\, (cons (make-id id) (iter))]
          ['\; (list (make-id id))]
          [_ (report-error 'var '(\, \;) delim)])))
    (let* ([con (match! 'var)] [content (iter)])
      (new-tree 'var content)))
  
   (define (procedure)
    (let* ([proc-tok (match! 'proc)] [id-tok (match! 'ident)]
           [semi1 (match! '\;)] [blk (block)] [semi2 (match! '\;)])
      (new-tree 'proc (list (make-id id-tok) blk))))
  
  ; ========================= statement ===================
  (define (statement)
    (match (get-token-type)
      ['begin (statement-begin)]
      ['call (statement-call)]
      ['read (statement-read)]
      ['print (statement-print)]
      ['if (statement-if)]
      ['while (statement-while)]
      ['ident (statement-assign)]
      ['\. (new-tree 'statement '())]
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
      (new-tree 'begin stmts)))
  
  (define (var-list)    
    (define (iter)
      (let ([id (make-id (match! 'ident))])
        (match (get-token-type)
          ['\, (match! '\,) (cons id (iter))]
          ['assign (list id)]
          [_ (report-error 'var '(\, assign) (get-token!))])))
    (iter))
    
  (define (exp-list)
    ; exp-list 只在三个地方用到:assign, print, call
    ; assign, print 以分号或 end 结束, call 以右括号结束
    (define (iter)
      (let ([expr (exp)])
        (match (get-token-type)
          ['\, (match! '\,) (cons expr (iter))]
          [(or '\; '\) 'end) (list expr)]
          [_ (report-error 'var '(\, \; \)) (get-token!))])))
    (iter))
  
  (define (statement-call)
    (let* ([call-tok (match! 'call)] [id (match! 'ident)])
      (new-tree 'call (make-id id))))
  
  (define (statement-read)
    (let* ([read-tok (match! 'read)] [id (match! 'ident)])
      (new-tree 'read (make-id id))))
  
  (define (statement-print)
    (let* ([print-tok (match! 'print)] [exp-list (exp-list)])
      (new-tree 'print exp-list)))
    
  (define (statement-if)
    (let* ([if-tok (match! 'if)] [condi (condition)]
           [then-tok (match! 'then)] [stmt (statement)])
      (new-tree 'if (list condi stmt))))
  
  (define (statement-while)
    (let* ([while-tok (match! 'while)] [condi (condition)]
           [do-tok (match! 'do)] [stmt (statement)])
      (new-tree 'while (list condi stmt))))
  
  (define (statement-assign)
    (let* ([vars (var-list)] [ass (match! 'assign)] [exps (exp-list)])
      (if (= (length vars) (length exps))
          (new-tree 'assign (map list vars exps))
          (error 'assign "var number (~a) is not equal exp number (~a), ~a"
                 (length vars) (length exps) (id-pos (car vars))))))
  
  ; ========================= expression ==================
  
  (define (condition)
     (define (iter)
       (if (eq? (get-token-type) '\))
           (begin (match! '\)) '())
           (cons (exp) (iter))))
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      [(or 'true 'false)
       (let ([tok (get-token!)])
         (new-tree 'true #t (cons (third tok) (fourth tok))))]
      ['\(
       (match! '\()       
       (match (get-token-type)
         ['not
          (let ([result #f])
            (match! 'not)
            (set! result (new-tree 'not (exp)))
            (match! '\))
            result)]
         [(? logic-op?)
          (let ([op (get-type (get-token!))])
            (new-tree op (iter)))]
          [(? cond-op?)
           (let* ([op (get-type (get-token!))]
                  [l-arith (arithmetic)]
                  [r-arith (arithmetic)])
             (match! '\))
             (new-tree op (list l-arith r-arith)))]
          [_ (report-error 'condition (append *cond-op* *logic-op* '(not)))])]
       [_ (report-error 'condition '(true false ident \())]))
  
  (define (arithmetic)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (arithmetic) (iter))))
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      ['number
       (let ([tok (get-token!)])
         (new-tree 'number (second tok) (cons (third tok) (fourth tok))))]
      ['\(
       (match! '\()
       (if (member (get-token-type) *arith-op*)
           (let ([op (get-type (get-token!))])
             (new-tree op (iter)))
           (report-error 'arithmetic *arith-op*))]
      [_ (report-error 'arithmetic '(number ident \())]))
  
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
