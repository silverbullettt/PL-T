#lang racket
(require "util/lex-parser.rkt")
(provide PL/T-scanner)

(define PL/T-scanner
  (make-lex-parser 
   '(("~d+" number)
     ("~c(~c|~d|_)*" ident)
     ("~(" \() (")" \))
     ("~+" +) ("-" -) ("~*" *) ("/" /)
     ("=" =) ("#" \#) ("<" <) ("<=" <=) (">" >) (">=" >=) ("ODD" odd)
     ("," \,) (";" \;) (":=" assign) ("." \.)
     ("CONST" const) ("VAR" var)
     ("PROCEDURE" proc) ("CALL" call) ("PRINT" print) ("READ" read)
     ("BEGIN" begin) ("END" end)
     ("IF" if) ("THEN" then)
     ("WHILE" while) ("DO" do)
     ("TRUE" true) ("FALSE" false)
     ("AND" and) ("OR" or) ("NOT" not)
     ("![^!]*!" comment))
   'comment))
