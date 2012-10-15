#lang racket
(require "define.rkt" "util/utility.rkt")
(provide PL/T-parser print-tree)

; The syntax parser of PL/T

; program = block ".".
;
; block = [ "const" ident "(" expression ")"  { "," ident "(" expression ")" } ";" ]
;         [ var-decl ]
;         { "procedure" ident "(" var-list ")" block ";" } statement.
;
; var-decl = "var" decl-list ";"
;
; decl-list = ident [":" type|"(" expression ")"] 
;             { "," ident [":" type|"(" expression ")"] }
;
; statement = [ var-list ":=" exp-list | 
;             "call" ident "(" exp-list ")" |
;             "begin" statement {";" statement } "end" |
;             "if" condition "then" statement |
;             "while" condition "do" statement |
;             "print" exp-list | "read" ident ].
;
; var-list = ident { , ident }
;
; exp-list = expression { , expression }
;
; condition = ident | bool
;             "(" comp-op arithmetic arithmetic ")" |
;             "(" "not" condition ")" | "(" ["and"|"or"] condition { condition } ")".
;
; arithmetic = ident | number |
;              "(" arith-op arithmetic { arithmetic } ")".
;
; str        = ident | string | "(" str-op string { string } ")".
;
; expression = condition | arithmetic | string.


(define (PL/T-parser tokens)
  
  (define (tok-type tok) (first tok))
  (define (tok-content tok) (second tok))
  (define (tok-pos tok) (cons (third tok) (fourth tok)))
  
  (define (get-token-type) (if (null? tokens)
                               #f
                               (tok-type (first tokens))))
  ; get type of second token
  (define (get-token2-type) (if (null? (cdr tokens))
                                #f
                                (tok-type (second tokens))))
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
    (list (tok-content tok) (tok-pos tok)))
  (define (make-id tok)
    (new-tree 'ident (tok-content tok) (tok-pos tok)))
  (define (make-const tok)
    (new-tree (tok-type tok) (tok-content tok) (tok-pos tok)))
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
      (let* ([id (match! 'ident)]
             [lbr (match! '\()]
             [val (const-value)]
             [rbr (match! '\))]
             [delim (get-token!)])
        (match (tok-type delim)
          ['\, (cons (list (make-id id) val) (iter))]
          ['\; (list (list (make-id id) val))]
          [_ (report-error 'const '(\, \;) delim)])))
    (let* ([con (match! 'const)] [content (iter)])
      (new-tree 'const content)))
  
  ; new declare list
  (define (var-decl)
    (define (decl)
      (let* ([id (match! 'ident)] [id-tree (make-id id)])
        (match (get-token-type)
          ['\( (match! '\()
               (let ([expr (exp)])
                 (match! '\))
                 (list id-tree expr))]
          [': (match! ':) (list id-tree (string->symbol
                                         (string-downcase
                                          (second (match! 'type)))))]
          [(or '\, '\;) id-tree]
          [_ (report-error 'var '(\, \( : \;) (get-token!))])))
    (define (iter)
      (let* ([id (decl)] [delim (get-token!)])
        (match (tok-type delim)
          ['\, (cons id (iter))]
          ['\; (list id)]
          [_ (report-error 'var '(\, \;) delim)])))
    (let* ([unused (match! 'var)] [content (iter)])
      (new-tree 'var content)))
  
  (define (procedure)
    (define (decl)
      (let* ([id (match! 'ident)] [id-tree (make-id id)])
        (match (get-token-type)
          [': (match! ':) (list id-tree (string->symbol
                                         (string-downcase
                                          (second (match! 'type)))))]
          [(or '\, '\)) id-tree]
          [_ (report-error 'procedure '(\, \) :) (get-token!))])))
    (define (iter)
      (let ([id (decl)])
        (match (get-token-type)
          ['\, (get-token!) (cons id (iter))]
          ['\) (list id)]
          [_ (report-error 'procedure '(\, \)) (get-token!))])))
    (let* ([proc-tok (match! 'proc)] [id-tok (match! 'ident)]
           [lbrac (match! '\()] [var-list (iter)] [rbrac (match! '\))]
           [blk (block)]
           [semi2 (match! '\;)])
      (new-tree 'proc (list (make-id id-tok) var-list blk))))
  
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
    (let* ([call-tok (match! 'call)]
           [id (match! 'ident)]
           [lbrac (match! '\()]
           [exp-list (exp-list)]
           [rbrac (match! '\))])
      (new-tree 'call (list (make-id id) exp-list))))
  
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
  
  (define (const-value)
    (if (member (get-token-type) '(int real bool string))
        (make-const (get-token!))
        (report-error 'const-value '(int real bool string))))
  
  (define (condition)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (exp) (iter))))
    (match (get-token-type)
      ['ident (make-id (get-token!))]
      ['bool (make-const (get-token!))]
      ['\(
       (match! '\()       
       (match (get-token-type)
         ['comp-op
          (let* ([op-tok (get-token!)]
                 [op (string->symbol (tok-content op-tok))]
                 [l-exp (exp)]
                 [r-exp (exp)])
            (match! '\))
            (new-tree op (list l-exp r-exp) (tok-pos op-tok)))]
         ['logic-op
          (let* ([op-tok (get-token!)]
                 [op (string->symbol
                      (string-downcase (tok-content op-tok)))])
            (if (eq? op 'not)
                (let ([result (new-tree 'not (exp))])
                  (match! '\))
                  result)
                (new-tree op (iter) (tok-pos op-tok))))]           
         [_ (report-error 'condition (append *comp-op* *logic-op* '(not)))])]
      [_ (report-error 'condition '(true false ident \())]))
  
  (define (arithmetic)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (arithmetic) (iter))))
    (match (get-token-type)
      ['ident (make-id (match! 'ident))]
      [(or 'int 'real) (make-const (get-token!))]
      ['\(
       (match! '\()
       (if (eq? (get-token-type) 'arith-op)
           (let* ([op-tok (get-token!)]
                  [op (string->symbol (tok-content op-tok))])
             (new-tree op (iter) (tok-pos op-tok)))
           (report-error 'arithmetic *arith-op*))]
      [_ (report-error 'arithmetic '(number ident \())]))
  
  (define (str)
    (define (@iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (str) (@iter))))
    (define (<-iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (exp) (<-iter))))
    (match (get-token-type)
      ['ident (make-id (get-token!))]
      ['string (make-const (get-token!))]
      ['\(
       (match! '\()
       (let* ([op-tok (get-token!)]
              [op (string->symbol (tok-content op-tok))])
         (match op
           ['@ (new-tree '@ (@iter) (tok-pos op-tok))]
           ['<-
            (if (or (eq? (get-token-type) 'string)
                    (eq? (get-token-type) 'ident))
                (let ([str (make-const (get-token!))])
                  (new-tree '<- (cons str (<-iter)) (tok-pos op-tok)))
                (report-error 'str '(string ident)))]
           [_ (report-error 'str '(@ <-))]))]
      [_ (report-error 'str *str-op*)]))
  
  (define (exp)
    (match (get-token-type)
      ['ident (make-id (get-token!))]
      [(or 'int 'real) (arithmetic)]
      ['bool (condition)]
      ['string (str)]
      ['\(
       (match (get-token2-type)
         ['arith-op (arithmetic)]
         [(or 'comp-op 'logic-op) (condition)]
         ['str-op (str)]
         [_ (report-error 'exp '(arith-op condition-op str-op))])]
      [_ (report-error 'exp '(int real bool string ident \())]))
  
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
