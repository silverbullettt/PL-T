#lang racket
(require rnrs/io/ports-6)
(require "PL0-scanner.rkt"
         "PL0-parser.rkt"
         "PL0-analyzer.rkt"
         "PL0-generator.rkt"
         "PL0-machine.rkt")

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (exec filename)
  (let* ([t (PL/0-parser
             (PL/0-scanner
              (read-string-from-file filename)))]
         [st (PL/0-analyzer t)])
    (if st
        (let ([code-ent (PL/0-generator t st)])
          (PL/0-machine (first code-ent)
                        (second code-ent)))
        (error 'exec "Something wrong!"))))

; (exec "test.pl")
