#lang racket
(require "lex-parser.rkt")
(provide PL/0-scanner)

(define PL/0-scanner
  (make-lex-parser 
   '(("~d+" number)
     ("~c+" ident)
     ("~(" \() (")" \))
     ("~+" +) ("-" -) ("~*" *) ("/" /)
     ("=" =) ("#" \#) ("<" <) ("<=" <=) (">" >) (">=" >=) ("ODD" odd)
     ("," \,) (";" \;) (":=" assign) ("." period)
     ("CONST" const) ("VAR" var)
     ("PROCEDURE" proc) ("CALL" call)
     ("BEGIN" begin) ("END" end)
     ("IF" if) ("THEN" then)
     ("WHILE" while) ("DO" do))))
