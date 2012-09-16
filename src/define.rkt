#lang racket
(require "util/utility.rkt")
(provide (all-defined-out))

(define-struct tree (type content) #:mutable)
(define-struct env (name st parent) #:mutable)
(define-struct type-info (type value) #:mutable)
; type : var, const, (array, struct)
; value: unknown, number, bool, (string, proc)
(define-struct entity (name type value) #:mutable)

(define *plus* '+)
(define *minus* '-)
(define *times* '*)
(define *over* '/)

(define *arith-op* '(+ - * /))
(define *logic-op* '(and or))
(define *cond-op* '(= \# < <= > >=))

(define arith-op? (member-tester (append *arith-op* '(number))))
(define logic-op? (member-tester *logic-op*))
(define cond-op? (member-tester *cond-op*))
(define cond? (member-tester
                 (append *logic-op* *cond-op* '(not true false))))

(define *init* '=)
