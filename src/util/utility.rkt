#lang racket

(provide range accumulate union-append union-append*
         string-find-first string-find-last
         string-empty? string-split-list
         reverse-pair list-intersect? list-join member-tester
         list-and)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (range . params)
  (define (check-params start end step)
    (cond [(and (positive? step) (< end start))
           (error "range: step > 0 && start > end")]
          [(and (negative? step) (> end start))
           (error "range: step < 0 && start < end")]
          [(zero? step)
           (error "range: step cannot be zero!")]
          [else 'ok]))
  (define (range-iter start end step lst)
    (if (or (and (> step 0) (>= start end))
            (and (< step 0) (<= start end)))
        (reverse lst)
        (range-iter (+ start step) end step (cons start lst))))
  (define (check-and-iter start end step)
    (if (eq? (check-params start end step) 'ok)
        (range-iter start end step '())
        #f))
  
  (case (length params)
    ['1 (check-and-iter 0 (first params) 1)]
    ['2 (check-and-iter (first params) (second params) 1)]
    ['3 (check-and-iter (first params) (second params) (third params))]
    [else (error "range: params must be (range end) or (range start end [step])")]))

(define (union-append . lists)
  (remove-duplicates (accumulate append '() lists)))

(define (union-append* . lists)
  (remove-duplicates (accumulate append* '() lists)))

(define (string-find-first str char)
  (let ([str-len (string-length str)])
    (define (find-iter k)
      (cond [(= k str-len) -1]
            [(char=? (string-ref str k) char) k]
            [else (find-iter (add1 k))]))
    (find-iter 0)))

(define (string-find-last str char)
  (let ([str-len (string-length str)])
    (define (find-iter k)
      (cond [(= k -1) -1]
            [(char=? (string-ref str k) char) k]
            [else (find-iter (sub1 k))]))
    (find-iter (- str-len 1))))

(define (string-empty? str)
  (= (string-length str) 0))

(define (string-split-list str delim)
  (let ([i (string-find-first str delim)])
    (if (= i -1)
        (list str)
        (cons (substring str 0 i)
              (string-split-list (substring str (+ i 1)) delim)))))

(define (reverse-pair p)
  (cons (cdr p) (car p)))

(define (list-intersect? lst1 lst2)
  (not (set-empty? (set-intersect (list->set lst1) (list->set lst2)))))

(define (list-join join? proc lst)
  (cond [(null? lst) '()]
        [(= (length lst) 1) lst]
        [(and (join? (first lst))
              (join? (second lst)))
         (cons (proc (first lst) (second lst)) (cddr lst))]
        [else (cons (car lst)
                    (list-join join? proc (cdr lst)))]))

(define (member-tester set)
  (lambda (x) (if (member x set) #t #f)))

(define (list-and ls)
  (call/cc
   (lambda (return)
     (let f ([ls ls])
       (cond [(null? ls) #t]
             [(not (car ls)) (return #f)]
             [(f (cdr ls))])))))