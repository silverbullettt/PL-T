#lang racket
(require "define.rkt" "util/table.rkt")
(provide make-symbol-table)

(define (make-symbol-table syntax-tree)
  (let ([symbol-table (make-table 2)])
    ; one is id, others are attributes
    (define (insert! id type)
      ((symbol-table 'insert!) id 'type type))
    
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
             (insert! (car (tree-content id)) 'const)))
         (tree-content const-tree))))
    
    (define (add-var var-tree)
      (when (not (null? var-tree))
        (for-each
         (lambda (x)
           (insert! (car (tree-content x)) 'var))
         (tree-content var-tree))))
    
    (define (add-proc proc-tree)
      (insert! (first (tree-content proc-tree)) 'proc)
      (add-block (second (tree-content proc-tree))))
    
    (add-block (tree-content syntax-tree))
    symbol-table))
