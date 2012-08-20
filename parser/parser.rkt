#lang racket
(require "../o-lex/pl0-lex-parser.rkt")
(provide PL/0-parser)


(define-struct tree-node (type subtree sibling attr))
(define-struct attr (op name value lineno))

(define (PL/0-parser tokens)
  
  (define (get-type tok) (first tok))
  (define (get-token-type) (if (null? tokens)
                               #f
                               (get-type (first tokens))))
  (define (get-token) (if (null? tokens) #f (first tokens)))
  (define (get-token!)
    (let ([tok (get-token)])
      (begin (set! tokens (cdr tokens)) tok)))
  (define (match? tok-type)
    (eq? tok-type (first (get-token))))
  (define (match! tok-type)
    (let ([tok (get-token)])
      (if (match? tok-type)
          (get-token!)
          (error 'PL/0-parser "Error token: ~a, excepted: ~a, given: ~a~%line ~a:~a"
                 (second tok) tok-type (first tok) (third tok) (fourth tok)))))
  (define (get-node-lineno node) (attr-lineno (tree-node-attr node)))
  
  (define (exp)
    (let ([left-child null] [right-child null])
      (begin
        (set! left-child (term))
        (if (and (get-token) (member (get-token-type) '(+ -)))
            (let* ([tok (get-token)] [op (first tok)] [lineno (third tok)])
              (begin
                (match! op) ; may error
                (make-tree-node 'op
                                (list left-child (exp))
                                null
                                (make-attr op null null lineno))))
            (make-tree-node 'exp ; default
                            (list left-child right-child)
                            null
                            (make-attr null null null (get-node-lineno left-child)))))))
  (define (term)
    (let ([left-child null] [right-child null])
      (begin
        (set! left-child (factor))
        (if (and (get-token) (member (get-token-type) '(* /)))
            (let* ([tok (get-token)] [op (first tok)] [lineno (third tok)])
              (begin
                (match! op) ; may error
                (make-tree-node 'op
                                (list left-child (term))
                                null
                                (make-attr op null null lineno))))
            (make-tree-node 'term ; default
                            (list left-child right-child)
                            null
                            (make-attr null null null (get-node-lineno left-child)))))))
  (define (factor)
    (match (get-token-type)
      ['number
       (make-tree-node 'number null null
                       (match-let ([(list _ val lineno _) (match! 'number)]) ; error
                         (make-attr null null val lineno)))]
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
      [_ #f])) ; report error

  (if (null? tokens)
      null
      (exp)))

(define (print-node node)
  (define (to-string x)
    (if (null? x) "" (format " ~a" x)))
  (define (node->string node)
    (format "~a ~a ~a"
            (attr->string (tree-node-attr node))
            (if (null? (tree-node-subtree node)) ""
                (string-append "(" (node-list->string (tree-node-subtree node)) ")"))
            (if (null? (tree-node-sibling node)) ""
                (string-append "<" (node-list->string (tree-node-sibling node)) ">"))))
  (define (node-list->string node-list)
    (string-join (map
                  (lambda (t)
                    (if (null? t) "" (node->string t)))
                  node-list)
                 " "))
    (define (attr->string attr)
      (format "~a~a~a"
              (to-string (attr-op attr))
              (to-string (attr-name attr))
              (to-string (attr-value attr))))
    (printf "[~a]~%" (node->string node)))

(define t (PL/0-parser (PL/0-lex-parser "4+5+6")))
;(print-node t)
(set! t (PL/0-parser (PL/0-lex-parser "123+x*(y+9)")))
(print-node t)
