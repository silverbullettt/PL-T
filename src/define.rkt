#lang racket
(require "util/utility.rkt")
(provide (all-defined-out))

(define-struct tree (type content pos) #:mutable)
(define (id-name id) (tree-content id))
(define (id-pos id) (tree-pos id))

(define-struct env (name st parent) #:mutable)
(define-struct type-info (type value const?) #:mutable)
; type:  atom, procedure, (array, struct)
; value: int, real, bool, string <- atom
;                      type-info <- array
;     (list-of-args list-of-ret) <- procedure
(define-struct entity (name type value) #:mutable)

(define *plus* '+)
(define *minus* '-)
(define *times* '*)
(define *over* '/)

(define *arith-op* '(+ - * /))
(define *logic-op* '(and or not))
(define *comp-op* '(= \# < <= > >=))
(define *str-op* '(@ <-))
(define *array* '(const-array new-array temp-array))

(define arith-op? (member-tester *arith-op*))
(define logic-op? (member-tester *logic-op*))
(define comp-op? (member-tester *comp-op*))
(define cond-op? (member-tester (append *logic-op* *comp-op*)))
(define str-op? (member-tester *str-op*))
(define array? (member-tester *array*))
