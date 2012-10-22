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

(define (make-vector<? bigger smaller)
  (define (comp lv rv)
    (define (iter l r)
      (match (cons l r)
        [(cons '() '()) #f]
        [(cons '() (list _ ...)) #t]
        [(cons (list _ ...) '()) #f]
        [(cons (list lh lr ...) (list rh rr ...))
         (cond [(bigger lh rh) #f]
               [(smaller lh rh) #t]
               [else (iter lr rr)])]))
    (iter (vector->list lv) (vector->list rv)))
  comp)

(define (make-vector<=? bigger smaller)
  (define (comp lv rv)
    (define (iter l r)
      (match (cons l r)
        [(cons '() '()) #t]
        [(cons '() (list _ ...)) #t]
        [(cons (list _ ...) '()) #f]
        [(cons (list lh lr ...) (list rh rr ...))
         (cond [(bigger lh rh) #f]
               [(smaller lh rh) #t]
               [else (iter lr rr)])]))
    (iter (vector->list lv) (vector->list rv)))
  comp)

(define (make-vector>? bigger smaller)
  (define (comp lv rv)
    (define (iter l r)
      (match (cons l r)
        [(cons '() '()) #f]
        [(cons '() (list _ ...)) #f]
        [(cons (list _ ...) '()) #t]
        [(cons (list lh lr ...) (list rh rr ...))
         (cond [(bigger lh rh) #t]
               [(smaller lh rh) #f]
               [else (iter lr rr)])]))
    (iter (vector->list lv) (vector->list rv)))
  comp)

(define (make-vector>=? bigger smaller)
  (define (comp lv rv)
    (define (iter l r)
      (match (cons l r)
        [(cons '() '()) #t]
        [(cons '() (list _ ...)) #f]
        [(cons (list _ ...) '()) #t]
        [(cons (list lh lr ...) (list rh rr ...))
         (cond [(bigger lh rh) #t]
               [(smaller lh rh) #f]
               [else (iter lr rr)])]))
    (iter (vector->list lv) (vector->list rv)))
  comp)

(define (make-array-comparer type op)
  (let ([bigger ((comp-map 'lookup) type '>)]
        [smaller ((comp-map 'lookup) type '<)])
    (match op
      ['= equal?]
      ['\# (make-not equal?)]
      ['< (make-vector<? bigger smaller)]
      ['<= (make-vector<=? bigger smaller)]
      ['> (make-vector>? bigger smaller)]
      ['>= (make-vector>=? bigger smaller)])))

(define (get-comparer array/atom type op)
  (match array/atom
    ['atom ((comp-map 'lookup) type op)]
    ['array (make-array-comparer type op)]))
