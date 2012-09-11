#lang racket
(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
         "PLT-parser.rkt"
         "PLT-analyzer.rkt"
         "PLT-generator.rkt"
         "PLT-machine.rkt"
         "util/table.rkt")
(provide (all-defined-out))

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (exec filename)
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (let ([code-ent (PL/T-generator t st)])
          (PL/T-machine (first code-ent)
                        (second code-ent)))
        (error 'exec "Something wrong!"))))

(define t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file "../sample/test.pl"))))
(define st (PL/T-analyzer t))
(define code (PL/T-generator t st))

(exec "../sample/test.pl")