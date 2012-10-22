#lang racket
(require "util/lex-parser.rkt")
(provide PL/T-scanner)

(define PL/T-scanner
  (make-lex-parser 
   '(("~c(~c|~d|_|-|~?)*" ident)

     ("~+|-|~*|/" arith-op)        ; arithmetic operator
     ("=|#|<|<=|>|>=" comp-op)     ; compare operator
     ("AND|OR|NOT" logic-op)       ; logic operator
     ("@|<-" str-op)               ; string operator
     ("NULL" null) ; ("NULL?" null?) ; test null value
     
     ("CONST" const) ("VAR" var)
     ("INT|REAL|BOOL|STRING" type) ; type
     
     ("(~+|-)?~d+" int)
     ("(~+|-)?~d+.~d+" real)
     ("#t|#f" bool)
     ("\"([^\"\\]|\\\"|\\\\)*\"" string)
     
     ("~(" \() (")" \))
     
     ("~[" \[) ("]" \]) ("SIZE" size) ; for array
     ; ("NEW" new)
     ("FOREACH" foreach) ("IN" in)
     
     ("{" \{) ("}" \}) ; for structure
     (":" :) ("," \,) (";" \;) (":=" assign) ("." \.)
     
     ("PROCEDURE" proc) ("->" ->)
     ("CALL" call) ("RETURN" return)
     ("READ" read) ("PRINT" print)    ; I/O
     ("BEGIN" begin) ("END" end) 
     ("IF" if) ("THEN" then) ; ("ELSE" else) 
     ("WHILE" while) ("DO" do) ; ("FOR" for)
     ;("BREAK" break) ("CONTINUE" continue)
     ("![^!]*!" comment))
   'comment
   'string))
