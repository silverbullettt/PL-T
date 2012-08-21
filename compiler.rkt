#lang racket
(require rnrs/io/ports-6)
(require "scanner/PL0-scanner.rkt"
         "parser/parser.rkt")

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (compile filename)
  (PL/0-parser
    (PL/0-scanner (read-string-from-file filename))))

(define t (compile "test.pl"))
(print-tree t)
