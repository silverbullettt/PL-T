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
; proc = "procedure" ident "(" decl-list ")" -> "(" type-list ")" block.
;
; type = ["int"|"real"|"bool"|"string"|"var"].
;
; type-list = type { "," type }.
;
; var-decl = "var" decl-list ";"
;
; decl-list = ident [":" type|"(" expression ")"] 
;             { "," ident [":" type|"(" expression ")"] }
;
; statement = [ var-list ":=" exp-list | 
;             "call" ident "(" exp-list ")" |
;             "return" exp-list |
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
;             "(" comp-op exp exp ")" |
;             "(" "not" condition ")" |
;             "(" ["and"|"or"] condition { condition } ")".
;
; arithmetic = ident | number |
;              "(" arith-op arithmetic { arithmetic } ")".
;
; str        = ident | string | "(" str-op string { string } ")".
;
; expression = condition | arithmetic | string | null |
;              "call" ident "(" exp-list ")".
;

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
  (define (get-token3-type) (if (null? (cddr tokens))
                                #f
                                (tok-type (third tokens))))
  
  (define (get-token)
    (if (null? tokens) #f  (first tokens)))
  (define (get-token2)
    (second tokens))
  (define (get-token!)
    (let ([tok (get-token)])
      (begin (set! tokens (cdr tokens)) tok)))
  
  (define (match? tok-type)
    (eq? tok-type (first (get-token))))
  (define (match! tok-type)
    (let ([tok (get-token)])
      (if (match? tok-type)
          (get-token!)
          (error 'PL/0-parser "Error token: (~a) excepted: (~a) given: (~a) -- L~a:~a"
                 (second tok) tok-type (first tok) (third tok) (fourth tok)))))
  
  (define (new-tree type content [pos #f])
    (make-tree type content pos))
  (define (get-id-inf tok)
    (list (tok-content tok) (tok-pos tok)))
  (define (make-id tok)
    (new-tree 'ident (tok-content tok) (tok-pos tok)))
  (define (make-const tok)
    ; 语法分析不对常量做任何转换
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
          (report-error 'program "Unknown error"))))
  
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
  
  
  (define (type)
    ; (printf "%%% type:~a%%%\n" (get-token))
    (match (get-token-type)
      [(or 'type 'var)
       (let* ([ty-tok (get-token!)]
              [ty (string->symbol
                   (string-downcase
                    (tok-content ty-tok)))])
         (if (eq? (get-token-type) '\[)
             (begin
               (match! '\[)
               (match! '\])
               (new-tree 'type (list 'array ty) (tok-pos ty-tok)))
             (new-tree 'type (list 'atom ty) (tok-pos ty-tok))))]
      [_ (report-error 'type '(int real bool string var))]))
  
  
  ; new declare list
  (define (var-decl)
    (define (decl)
      (let* ([id (match! 'ident)] [id-tree (make-id id)])
        (match (get-token-type)
          ['\( (match! '\()
               (let ([expr (exp)])
                 (match! '\))
                 (list id-tree expr))]
          [': (match! ':) (list id-tree (type))]
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
          [': (match! ':)
              (if (or (eq? (get-token-type) 'var)
                      (eq? (get-token-type) 'type))
                  (list id-tree (type))
                  (report-error 'procedure '(type var) (get-token!)))]
          [(or '\, '\)) id-tree]
          [_ (report-error 'procedure '(\, \) :) (get-token!))])))
    
    (define (var-iter)
      (if (eq? (get-token-type) '\))
          '()
          (let ([id (decl)])
            (match (get-token-type)
              ['\, (get-token!) (cons id (var-iter))]
              ['\) (list id)]
              [_ (report-error 'procedure '(\, \)) (get-token!))]))))
  
    (define (type-iter)
      (cond [(eq? (get-token-type) '\)) '()]
            [(or (eq? (get-token-type) 'var)
                 (eq? (get-token-type) 'type))
             (let ([type (type)])
               (match (get-token-type)
                 ['\, (get-token!) (cons type (type-iter))]
                 ['\) (list type)]
                 [_ (report-error 'procedure '(\, \)) (get-token!))]))]
            [else (report-error 'procedure '(type var) (get-token!))]))
    
    (let* ([proc-tok (match! 'proc)]
           [id-tok (match! 'ident)]
           [lb1 (match! '\()] [var-list (var-iter)] [rb1 (match! '\))]
           [arrow (match! '->)]
           [lb2 (match! '\()] [type-list (type-iter)] [rb2 (match! '\))]
           [blk (block)]
           [semi2 (match! '\;)])
      (new-tree 'proc (list (make-id id-tok) (list var-list type-list) blk))))
  
  ; ========================= statement ===================
  (define (statement)
    (match (get-token-type)
      ['begin (statement-begin)]
      ['call (statement-call)]
      ['return (statement-return)]
      ['read (statement-read)]
      ['print (statement-print)]
      ['if (statement-if)]
      ['while (statement-while)]
      ['foreach (statement-foreach)]
      ['ident (statement-assign)]
      ['\. (new-tree 'statement '())]
      [_ (report-error 'statement
                       '(begin end
                         call return
                         if then
                         while foreach do
                         ident
                         read print))]))
  
  (define (statement-begin)
    (define (iter)
      (match (get-token-type)
        ['\; (begin (match! '\;)
                    (cons (statement) (iter)))]
        ['end '()]
        [_ (report-error 'statement-begin '(\; end))]))
    (let* ([beg-tok (match! 'begin)]
           [stmts (cons (statement) (iter))]
           [end-tok (match! 'end)])
      (new-tree 'begin stmts (tok-pos beg-tok))))
  
  (define (var-list)
    (define (iter)
      (let ([id (if (eq? (get-token2-type) '\[)
                    (array-ref)
                    (make-id (match! 'ident)))])
        (match (get-token-type)
          ['\, (match! '\,) (cons id (iter))]
          ['assign (list id)]
          [_ (report-error 'var '(\, assign) (get-token!))])))
    (iter))
  
  (define (exp-list)
    ; exp-list 在四个地方用到:assign, print, call, return
    ; assign, print, return 以分号或 end 结束, call 以右括号结束
    ; !!! call 的返回值可能不止一个, 所以 exp-list 里面有 call 的时候
    ;     留给 analyzer 检查
    (define (iter)
      (let ([expr (exp)])
        (match (get-token-type)
          ['\, (match! '\,) (cons expr (iter))]
          [(or '\; '\) 'end) (list expr)]
          [_ (report-error 'var '(\, \; \)) (get-token!))])))
    (if (member (get-token-type) '(\; \) end))
        '()
        (iter)))
  
  (define (statement-call)
    (let* ([call-tok (match! 'call)]
           [id (match! 'ident)]
           [lbrac (match! '\()]
           [exp-list (exp-list)]
           [rbrac (match! '\))])
      (new-tree 'call
                (list (make-id id) exp-list)
                (tok-pos call-tok))))
  
  (define (statement-return)
    (let* ([ret-tok (match! 'return)]
           [expr-list (exp-list)])
      (new-tree 'return expr-list (tok-pos ret-tok))))
  
  (define (statement-read)
    (let* ([read-tok (match! 'read)])
      (new-tree 'read
                (if (and (eq? (get-token2-type) '\[)
                         (is-connect? (get-token) (get-token2)))
                    (array-ref)
                    (make-id (get-token!)))
                (tok-pos read-tok))))
  
  (define (statement-print)
    (let* ([print-tok (match! 'print)] [exps (exp-list)])
      (new-tree 'print exps (tok-pos print-tok))))
  
  (define (statement-if)
    (let* ([if-tok (match! 'if)]
           [condi (condition)]
           [then-tok (match! 'then)]
           [stmt (statement)])
      (new-tree 'if (list condi stmt) (tok-pos if-tok))))
  
  (define (statement-while)
    (let* ([while-tok (match! 'while)]
           [condi (condition)]
           [do-tok (match! 'do)]
           [stmt (statement)])
      (new-tree 'while (list condi stmt) (tok-pos while-tok))))
  
  (define (statement-foreach)
    (let* ([for-tok (match! 'foreach)]
           [lbrac (match! '\()]
           [var (make-id (match! 'ident))]
           [in (match! 'in)]
           [arr (array)]
           [rbrac (match! '\))]
           [do-tok (match! 'do)]
           [stmt (statement)])
      (new-tree 'foreach (list var arr stmt) (tok-pos for-tok))))
  
  ; 由于 call 的返回值可能不止1个,因此 assign 参数个数留给 analyzer 检查
  (define (statement-assign)
    (let* ([vars (var-list)] [ass (match! 'assign)] [exps (exp-list)])
      (new-tree 'assign (list vars exps) (tok-pos ass))))
  
  ; ========================= expression ==================
  
  (define (const-value)
    (match (get-token-type)
      [(or 'int 'real 'bool 'string) (make-const (get-token!))]
      ['\[ (const-array)]
      [_ (report-error 'const-value '(int real bool string))]))
  
  ; !!! 当作为表达式的一部分时, call 相应的函数只能返回一个值
  (define (condition)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (exp) (iter))))
    (match (get-token-type)
      ['ident (if (eq? (get-token2-type) '\[)
                  (array-ref)
                  (make-id (get-token!)))]
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
            ; null could only be used for equal compare
            (case op
              [(= \#) (new-tree op (list l-exp r-exp) (tok-pos op-tok))]
              [else
               (if (or (eq? (tree-type l-exp) 'null)
                       (eq? (tree-type r-exp) 'null))
                   (error
                    'compare
                    "NULL could only be used for compare equivanlence, ~a."
                    (tok-pos op-tok))
                   (new-tree op (list l-exp r-exp) (tok-pos op-tok)))]))]
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
      ['call (statement-call)]
      [_ (report-error 'condition '(true false ident \())]))
  
  (define (arithmetic)
    (define (iter)
      (if (eq? (get-token-type) '\))
          (begin (match! '\)) '())
          (cons (arithmetic) (iter))))
    (match (get-token-type)
      ['ident (if (eq? (get-token2-type) '\[)
                  (array-ref)
                  (make-id (get-token!)))]
      [(or 'int 'real) (make-const (get-token!))]
      ['size (size)]
      ['\(
       (match! '\()
       (if (eq? (get-token-type) 'arith-op)
           (let* ([op-tok (get-token!)]
                  [op (string->symbol (tok-content op-tok))])
             (new-tree op (iter) (tok-pos op-tok)))
           (report-error 'arithmetic *arith-op*))]
      ['call (statement-call)]
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
      ['ident (if (eq? (get-token2-type) '\[)
                  (array-ref)
                  (make-id (get-token!)))]
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
      ['call (statement-call)]
      [_ (report-error 'str *str-op*)]))
  
  
  (define (array)
    (match (get-token-type)
      ['ident (make-id (get-token!))]
      ['type (new-array)]
      ['\[ (temp-array)]
      ['null (make-const (get-token!))]
      [_ (report-error 'array '(ident type \[ null))]))
  
  (define (const-array)
    (define (iter type)
      (let ([elem (get-token!)])
        (if (eq? (tok-type elem) type)
            (match (get-token-type)
              ['\, (get-token!) (cons (make-const elem) (iter type))]
              ['\] (list (make-const elem))]
              [_ (report-error 'const-array '(\, \]))])
            (report-error 'const-array type elem))))
    (if (member (get-token2-type) '(int real bool string))
        (let* ([lbrac (get-token!)]
               [element-ty (get-token-type)]
               [elems (iter element-ty)]
               [rbrac (get-token!)])
          (new-tree 'const-array (list element-ty elems) (tok-pos lbrac)))
        (begin ; unknown array type
          (get-token!) ; left bracket
          (report-error 'const-array '(int real bool string)))))
  
  (define (temp-array)
    (define (iter)
      (if (eq? (get-token-type) '\])
          '()
          (let ([elem (exp)])
            (match (get-token-type)
              ['\, (get-token!) (cons elem (iter))]
              ['\] (list elem)]
              [_ (report-error 'temp-array '(\, \]))]))))
    (let* ([lbrac (get-token!)]
           [elems (iter)]
           [rbrac (get-token!)])
      (new-tree 'temp-array elems (tok-pos lbrac))))
  
  (define (new-array)
    (let* ([type-tok (get-token!)]
           [lbrac (match! '\[)]
           [size (int)]
           [rbrac (match! '\])])
      (new-tree 'new-array (list
                            (string->symbol
                             (string-downcase (tok-content type-tok)))
                            size)
                (tok-pos type-tok))))
  
  (define (size)
    (let* ([size-tok (match! 'size)]
           [lbrac (match! '\()]
           [arr (array)]
           [rbrac (match! '\))])
      (new-tree 'size arr (tok-pos size-tok))))
  

  (define (int)
    (match (get-token-type)
      ['int (make-const (get-token!))]
      ['ident (if (eq? (get-token2-type) '\[)
                  (array-ref)
                  (make-id (get-token!)))]
      ['size (size)]
      [_ (report-error 'integer '(int id))]))
  
  (define (array-ref)
    (let* ([id-tok (get-token!)]
           [lbrac (match! '\[)]
           [index (int)]
           [rbrac (match! '\])])
      (new-tree 'array-ref (list (make-id id-tok) index) (tok-pos id-tok))))
  
  (define (is-connect? t1 t2)
    (and (= (third t1) (third t2))
         (= (+ (fourth t1) (string-length (second t1)))
            (fourth t2))))
  
  (define (exp)
    (match (get-token-type)
      ['ident (if (and (eq? (get-token2-type) '\[)
                       (is-connect? (get-token) (get-token2)))
                  (array-ref)
                  (make-id (get-token!)))]
      [(or 'int 'real) (arithmetic)]
      ['bool (condition)]
      ['string (str)]
      ['type (new-array)]
      ['\[ (temp-array)]
      ['size (size)]
      ['null (make-const (get-token!))]
      ['\(
       (match (get-token2-type)
         ['arith-op (arithmetic)]
         [(or 'comp-op 'logic-op) (condition)]
         ['str-op (str)]
         [_ (report-error 'exp '(arith-op condition-op str-op))])]
      ['call (statement-call)]
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

; ============================ for test =======================================
(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
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

(print-tree (parse "../sample/test_array.pl"))
(newline)
