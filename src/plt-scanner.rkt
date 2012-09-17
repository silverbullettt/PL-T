#lang racket
(require "util/lex-parser.rkt")
(provide PL/T-scanner)

(define PL/T-scanner
  (make-lex-parser 
   '(("~d+(.~d*)?" number)
     ("(_|~c)(~c|~d|_)*" ident)
     ("~(" \() (")" \))
     ("~+" +) ("-" -) ("~*" *) ("/" /)
     ("=" =) ("#" \#) ("<" <) ("<=" <=) (">" >) (">=" >=)
     ("," \,) (";" \;) (":=" assign) ("." \.)
     ("CONST" const) ("VAR" var)
     ("PROCEDURE" proc) ("CALL" call) ("RETURN" return)
     ("PRINT" print) ("READ" read)
     ("BEGIN" begin) ("END" end)
     ("IF" if) ("THEN" then)
     ("WHILE" while) ("DO" do)
     ("TRUE" true) ("FALSE" false)
     ("AND" and) ("OR" or) ("NOT" not)
     ("![^!]*!" comment))
   'comment))
