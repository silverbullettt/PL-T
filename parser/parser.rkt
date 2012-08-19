#lang racket
(require "../o-lex/pl0-lex-parser.rkt")
(provide PL/0-parser)


(define-struct tree-node (type subtree sibling attr))

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
  
  (define (exp)
    (let ([left-child null] [right-child null] [type null])
      (begin
        (set! left-child (term))
        (if (get-token)
            (begin
              (set! type (get-token-type))
              (match! type) ; may error
              (set! right-child (exp)))
            '())
        (make-tree-node type (list left-child right-child) null null))))
  (define (term)
    (let ([left-child null] [right-child null] [type null])
      (begin
        (set! left-child (factor))
        (if (get-token)
            (begin
              (set! type (get-token-type))
              (match! type) ; may error
              (set! right-child (term)))
            '())
        (make-tree-node type (list left-child right-child) null null))))
  (define (factor)
    (match (get-token-type)
      ['number
       (make-tree-node 'number null null
                       (match-let ([(list _ val linenr _) (match! 'number)])
                         (list val linenr)))]
      ['ident
       (make-tree-node 'ident null null
                       (match-let ([(list _ val linenr _) (match! 'ident)])
                         (list val linenr)))]
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

(define t (PL/0-parser (PL/0-lex-parser "1+2+3")))