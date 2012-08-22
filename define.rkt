#lang racket

(provide (all-defined-out))

(define-struct tree (type content) #:mutable)

(define *plus* '+)
(define *minus* '-)
(define *times* '*)
(define *over* '/)

(define *cond-op* '(= \# < <= > >=))
(define *init* '=)
