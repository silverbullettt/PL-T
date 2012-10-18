#lang racket
(require rnrs/io/ports-6)
(require "PLT-scanner.rkt"
         "PLT-parser.rkt"
         "PLT-analyzer.rkt"
         "PLT-generator.rkt"
         "PLT-machine.rkt"
         "define.rkt"
         "util/table.rkt")
(provide (all-defined-out))

(define (read-string-from-file filename)
  ; source code must be wrotten by latin-1-codec
  (get-string-all
   (transcoded-port (open-file-input-port filename)
                    (make-transcoder (latin-1-codec)))))

(define (exec [filename "../sample/test.pl"])
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (let ([code-ent (PL/T-generator t st)])
          (PL/T-machine (first code-ent)
                        (second code-ent)))
        (error 'exec "Something wrong!"))))

(define (scan [filename "../sample/test.pl"])
  (PL/T-scanner
    (read-string-from-file filename)))

(define (parse [filename "../sample/test.pl"])
  (PL/T-parser
   (PL/T-scanner
    (read-string-from-file filename))))

(define (analyse [filename "../sample/test.pl"])
  (PL/T-analyzer
   (PL/T-parser
    (PL/T-scanner
     (read-string-from-file filename)))))

(define (gen [filename "../sample/test.pl"])
  (let* ([t (PL/T-parser
             (PL/T-scanner
              (read-string-from-file filename)))]
         [st (PL/T-analyzer t)])
    (if st
        (PL/T-generator t st)
        #f)))

(print-tree (parse "../sample/test_call.pl"))

(define t (parse "../sample/test_call.pl"))
(print-tree t)
(define st (PL/T-analyzer t))

(define code (PL/T-generator t st))
(print-code (car code))
;(PL/T-machine (first code) (second code))

(exec "../sample/test_null.pl")
