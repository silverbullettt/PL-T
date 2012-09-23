#lang racket
(require "util/table.rkt" "define.rkt")
(provide get-comparer)

(define (make-not pred)
  (lambda (x . y) (not (apply pred (cons x y)))))

(define comp-map (make-table 2))

(define num-comp
  (list = (make-not =) < <= > >=))
(for-each
 (lambda (op comp)
   ((comp-map 'insert!) 'int op comp)
   ((comp-map 'insert!) 'real op comp))
 *comp-op*
 num-comp)

(define (bool->int b) (if b 1 0))
(define bool-comp  
  (list boolean=?
        (make-not boolean=?)
        (lambda (b1 b2)
          (< (bool->int b1) (bool->int b2)))
        (lambda (b1 b2)
          (<= (bool->int b1) (bool->int b2)))
        (lambda (b1 b2)
          (> (bool->int b1) (bool->int b2)))
        (lambda (b1 b2)
          (>= (bool->int b1) (bool->int b2)))))
(for-each
 (lambda (op comp)
   ((comp-map 'insert!) 'bool op comp))
 *comp-op*
 bool-comp)

(define str-comp
  (list string=?
        (make-not string=?)
        string<? string<=? string>? string>=?))
(for-each
 (lambda (op comp)
   ((comp-map 'insert!) 'string op comp))
 *comp-op*
 str-comp)

(define (get-comparer type op)
  ((comp-map 'lookup) type op))
