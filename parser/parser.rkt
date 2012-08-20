#lang racket
(require "../scanner/pl0-scanner.rkt" "const.rkt")
(provide PL/0-parser print-node)

(define-struct tree-node (type subtree sibling attr) #:mutable)
(define-struct attr (op name value lineno))

(define (make-node id-tok type)
  (make-tree-node type null null
                  (make-attr null (second id-tok)
                             null
                             (third id-tok))))
(define (make-idnode id-tok) (make-node id-tok 'ident))
(define (make-procnode proc-tok) (make-node proc-tok 'proc))
(define (make-numnode num-tok)
  (make-tree-node 'number null null
                  (make-attr null null
                             (string->number (second num-tok))
                             (third num-tok))))

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
  (define (get-node-lineno node) (attr-lineno (tree-node-attr node)))
  
  (define (match? tok-type)
    (eq? tok-type (first (get-token))))
  (define (match! tok-type)
    (let ([tok (get-token)])
      (if (match? tok-type)
          (get-token!)
          (error 'PL/0-parser "Error token: ~a, excepted: ~a, given: ~a~%line ~a:~a"
                 (second tok) tok-type (first tok) (third tok) (fourth tok)))))
 
  
  (define (report-error type)
    (error 'PL/0-parser "syntax error in ~a, token: ~a" type (get-token)))
  
  (define (program)
    (let ([blk (block)])
      (begin
        (match! 'period)
        (if (null? tokens) blk (report-error 'program)))))
  
  (define (block)
    (let ([const null] [var null] [proc null])
      (begin (if (eq? (get-token-type) 'const)
                 (begin (match! 'const) (set! const (const-init)) (match! '\;))
                 null)
             (if (eq? (get-token-type) 'var)
                 (begin (match! 'var) (set! var (var-decl)) (match! '\;))
                 null)
             (if (eq? (get-token-type) 'proc)
                 (set! proc (procedure))
                 null)
             (make-tree-node 'block
                             (list const var proc (statement))
                             null
                             null))))
  
  (define (const-init)
    (let* ([id-tok (match! 'ident)]
           [init (match! *init*)]
           [num-tok (match! 'number)])
      (make-tree-node 'init
                      (list (make-idnode id-tok)
                            (make-numnode num-tok))
                      (match (get-token-type)
                        ['\, (begin (match! '\,) (const-init))]
                        ['\; null]
                        [_ (report-error 'const-init)])
                      (make-attr 'const null null (third init)))))
  (define (var-decl)
    (let* ([id-tok (match! 'ident)])
      (make-tree-node 'init
                      (list (make-idnode id-tok))
                      (match (get-token-type)
                        ['\, (begin (match! '\,) (var-decl))]
                        ['\; null]
                        [_ (report-error 'var-decl)])
                      (make-attr 'var null null (third id-tok)))))
  
  (define (procedure)
    (let* ([proc-tok (match! 'proc)] [id-tok (match! 'ident)]
           [semi1 (match! '\;)] [blk (block)] [semi2 (match! '\;)])
      (make-tree-node 'proc
                      (list blk)
                      (if (eq? (get-token-type) 'proc)
                          (procedure)
                          null)
                      (make-attr 'proc (second id-tok) null (third proc-tok)))))
  
  (define (statement)
    (let ([stmt null])
      ; (printf "statement: ~a ~a~%" (get-token-type) (get-token))
      (match (get-token-type)
        ['begin 
         (begin
           (match! 'begin) (set! stmt (base-stmt #t)) (match! 'end)
           stmt)]
        [#f stmt]
        [_ (base-stmt #f)])))
  
  (define (base-stmt in-seq?)
    (define (get-sibling)
      (if (and in-seq? (not (eq? 'end (get-token-type))))
          (begin (match! '\;) (base-stmt #t))
          null))
    (match (get-token-type)
      ['ident
       (let* ([id-tok (match! 'ident)]
              [ass-tok (match! 'assign)]
              [expr (exp)])
         (make-tree-node 'statement
                         (list (make-idnode id-tok) expr)
                         (get-sibling)
                         (make-attr 'assign null null (third ass-tok))))]
      ['call
       (let* ([call-tok (match! 'call)]
              [id-tok (match! 'ident)])
         (make-tree-node 'statement
                         (list (make-idnode id-tok))
                         (get-sibling)
                         (make-attr 'call null null (third call-tok))))]
      ['if
       (let* ([if-tok (match! 'if)] [cond-node (condition)]
              [then-tok (match! 'then)] [stmt-node (statement)])
         (make-tree-node 'statement
                         (list cond-node stmt-node)
                         (get-sibling)
                         (make-attr 'if null null (third if-tok))))]
      ['while
       (let* ([while-tok (match! 'while)] [cond-node (condition)]
              [do-tok (match! 'do)] [stmt-node (statement)])
         (make-tree-node 'statement
                         (list cond-node stmt-node)
                         (get-sibling)
                         (make-attr 'while null null (third while-tok))))]
      ['begin (statement)]
      ['end null]
      [_ (report-error 'base-stmt)]))                 
  
  (define (condition)
    (cond [(member (get-type (second tokens)) *cond-op*)
           (let* ([left-child (exp)]
                  [tok (get-token)] [op (first tok)] [lineno (third tok)])
             (begin
               (match! op)
               (make-tree-node 'cond
                               (list left-child (exp))
                               null
                               (make-attr op null null lineno))))]
          [(eq? (get-token-type) 'odd)
           (let ([lineno (third (get-token))])
             (begin
               (match! 'odd)
               (make-tree-node 'cond
                               (list (exp))
                               null
                               (make-attr 'odd null null lineno))))]
          [else
           (error 'parser "CONDITION -- Unexcepted token: ~a" (get-token))]))
  
  (define (exp)
    (let ([left-child null])
      (begin
        (set! left-child (term))
        (if (and (get-token) (member (get-token-type) (list *plus* *minus*)))
            (let* ([tok (get-token)] [op (first tok)] [lineno (third tok)])
              (begin
                (match! op) ; may error
                (make-tree-node 'exp
                                (list left-child (exp))
                                null
                                (make-attr op null null lineno))))
            (make-tree-node 'exp ; default
                            (list left-child)
                            null
                            (make-attr null null null (get-node-lineno left-child)))))))
  (define (term)
    (let ([left-child null])
      (begin
        (set! left-child (factor))
        (if (and (get-token) (member (get-token-type) (list *times* *over*)))
            (let* ([tok (get-token)] [op (first tok)] [lineno (third tok)])
              (begin
                (match! op) ; may error
                (make-tree-node 'term
                                (list left-child (term))
                                null
                                (make-attr op null null lineno))))
            (make-tree-node 'term ; default
                            (list left-child)
                            null
                            (make-attr null null null (get-node-lineno left-child)))))))
  (define (factor)
    (match (get-token-type)
      ['number
       (make-tree-node 'number null null
                       (match-let ([(list _ val lineno _) (match! 'number)]) ; error
                         (make-attr null null (string->number val) lineno)))]
      ['ident
       (make-tree-node 'ident null null
                       (match-let ([(list _ val lineno _) (match! 'ident)]) ; error
                         (make-attr null val null lineno)))]
      ['\(
       (let ([e null])
         (begin
           (match! '\()
           (set! e (exp))
           (match! '\))
           e))]
      [_ (printf " --factor -> ~a~%" (get-token))])) ; report error

  (if (null? tokens)
      null
      (program)))

(define (print-node node)
  (define (to-string x)
    (if (null? x) "" (format " ~a" x)))
  (define (node->string node)
    (if (null? node) ""
        (format "~a ~a ~a"
                (attr->string (tree-node-attr node))
                (if (null? (tree-node-subtree node)) ""
                    (string-append "(" (node-list->string (tree-node-subtree node)) ")"))
                (if (null? (tree-node-sibling node)) ""
                    (string-append "[" (node->string (tree-node-sibling node)) "]")))))
  (define (node-list->string node-list)
    (string-join (map
                  (lambda (t)
                    (if (null? t) "" (node->string t)))
                  node-list)
                 " "))
  (define (attr->string attr)
    (if (null? attr) ""
        (format "~a~a~a"
                (to-string (attr-op attr))
                (to-string (attr-name attr))
                (to-string (attr-value attr)))))
    (printf "{~a}~%" (node->string node)))


(define t (PL/0-parser (PL/0-scanner
"CONST
  m =  7,
  n = 85;
 
VAR
  x, y, z, q, r;
 
PROCEDURE multiply;
VAR a, b;
 
BEGIN
  a := x;
  b := y;
  z := 0;
  WHILE b > 0 DO BEGIN
    IF ODD b THEN z := z + a;
    a := 2 * a;
    b := b / 2
  END
END;

CALL multiply .")))
;(print-node t)
