#lang racket
(require "define.rkt" "util/table.rkt" "util/utility.rkt")
(provide PL/0-analyzer)

(define (make-symbol-table syntax-tree)
  (let ([symbol-table (make-table 2)])
    ; one is id, others are attributes
    (define insert! (symbol-table 'insert!))
    
    (define (add-block blk-tree)
      (let ([content (tree-content blk-tree)])
        (add-const (first content))
        (add-var (second content))
        (for-each add-proc (third content))))
    
    (define (add-const const-tree)
      (when (not (null? const-tree))
        (for-each
         (lambda (x)
           (let ([id (first x)])
             (insert! (car (tree-content id)) 'type 'const)
             (insert! (car (tree-content id)) 'value (second x))))
        (tree-content const-tree))))
    
    (define (add-var var-tree)
      (when (not (null? var-tree))
        (for-each
         (lambda (x)
           (insert! (car (tree-content x)) 'type 'var))
         (tree-content var-tree))))
    
    (define (add-proc proc-tree)
      (insert! (first (tree-content proc-tree)) 'type 'proc)
      (insert! (first (tree-content proc-tree)) 'value proc-tree)
      (add-block (second (tree-content proc-tree))))
    
    (add-block (tree-content syntax-tree))
    symbol-table))

(define (PL/0-analyzer syntax-tree)
  (let ([symbol-table (make-symbol-table syntax-tree)])
    
    (define lookup (symbol-table 'lookup))
    (define insert! (symbol-table 'insert!))
    
    (define (check-block t)
      (check-statement (fourth (tree-content t))))
    
    (define (check-proc t)
      (check-block (second (tree-content t))))
    
    (define (check-statement t)
      (match (tree-type t)
        ['begin (check-begin t)]
        ['assign (check-assign t)]
        ['call (check-call t)]
        ['print (check-print t)]
        ['while (check-while t)]
        ['if (check-if t)]
        [_ (error "不可能！")]))
    
    (define (check-begin t)
      (list-and (map check-statement (tree-content t))))
    
    (define (check-assign t)
      (let ([id-info (tree-content (first (tree-content t)))]
            [exp-tree (second (tree-content t))])
        (cond [(not (lookup (first id-info) 'type))
               (printf "ASSIGN ERROR: Unknwon id '~a', L~a:~a.~%"
                       (first id-info) (car (second id-info)) (cdr (second id-info)))
               #f]
              [(not (eq? 'var (lookup (first id-info) 'type)))
               (printf "ASSIGN ERROR: '~a' is ~a and cannot be assigned, L~a:~a.~%"
                       (first id-info)
                       (lookup (first id-info) 'type)
                       (car (second id-info)) (cdr (second id-info)))
               #f]
              [(not (check-exp exp-tree))
               (printf "ASSIGN ERROR: not an invalid expression, L~a:~a.~%"
                       (car (second id-info)) (cdr (second id-info)))
               #f]
              [else
               (insert! (first id-info) 'value #t)
               #t])))
    
    (define (check-call t)
      (let ([id-info (tree-content (tree-content t))])
        (cond [(not (lookup (first id-info) 'type))
               (printf "CALL ERROR: Unknwon id '~a', L~a:~a.~%"
                       (first id-info) (car (second id-info)) (cdr (second id-info)))
               #f]
              [(not (eq? 'proc (lookup (first id-info) 'type)))
               (printf "CALL ERROR: '~a' is not a procedure, L~a:~a.~%"
                       (first id-info)
                       (car (second id-info)) (cdr (second id-info)))
               #f]
              [else (check-proc (lookup (first id-info) 'value))])))
    
    (define (check-print t)
      (check-exp (tree-content t)))
    
    (define (check-while t)
      (and
       (check-condition (first (tree-content t)))
       (check-statement (second (tree-content t)))))
    
    (define (check-if t)
      (and
       (check-condition (first (tree-content t)))
       (check-statement (second (tree-content t)))))
    
    
    (define (check-condition t)
      (if (eq? (tree-type t) 'odd)
          (check-exp (tree-content t))
          (and
           (check-exp (first (tree-content t)))
           (check-exp (second (tree-content t))))))
    
    (define (check-exp t)
      (list-and
       (map (lambda (x)
              (if (tree? x) (check-term x) #t))
            (tree-content t))))
    
    (define (check-term t)
      (list-and
       (map (lambda (x)
              (if (tree? x) (check-factor x) #t))
            (tree-content t))))
    
    (define (check-factor t)
      (match (tree-type t)
        ['ident
         (let ([id-info (tree-content t)])
           (match (lookup (first id-info) 'type)
             ['var
              (if (lookup (first id-info) 'value)
                  #t
                  (begin
                    (printf "EXPRESSION ERROR: '~a' cannot be used without initial, L~a:~a.~%"
                            (first id-info)
                            (car (second id-info)) (cdr (second id-info)))
                    #f))]
             ['const #t]
             ['proc
              (printf "EXPRESSION ERROR: procedure '~a' cannot be reference in expression, L~a:~a.~%"
                      (first id-info)
                      (car (second id-info)) (cdr (second id-info)))
              #f]
             [#f
              (printf "EXPRESSION ERROR: Unknwon id '~a', L~a:~a.~%"
                      (first id-info) (car (second id-info)) (cdr (second id-info)))
              #f]
             [_ (error "不可能！")]))]
        ['number #t]
        ['exp (check-exp t)]
        [_ (error "不可能！")]))
    
    (if (check-block (tree-content syntax-tree))
        symbol-table
        #f)))
