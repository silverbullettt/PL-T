#lang racket
(require "util/lex-parser.rkt")
(provide PL/T-scanner)

(define PL/T-scanner
  (make-lex-parser 
   '(("~c(~c|~d|_|-|~?)*" ident)
     ("CONST" const) ("VAR" var)
     ("INT|REAL|BOOL|STRING" type) ; type
     
     ("~d+" int)
     ("~d+.~d+" real)
     ("TRUE|FALSE" bool)
     ("\"([^\"\\]|\\\"|\\\\)*\"" string)
     
     ("~+|-|~*|/" arith-op)        ; arithmetic operator
     ("=|#|<|<=|>|>=" comp-op)     ; compare operator
     ("AND|OR|NOT" logic-op)       ; logic operator
     ("@|<-" str-op)               ; string operator
     
     ("~(" \() (")" \))
     ; ("~[" \[) ("]" \]) ; for array
     ; ("{" \{) ("}" \}) ; for structure
     
     (":" :) ("," \,) (";" \;) (":=" assign) ("." \.)
     
     ("PROCEDURE" proc) ("CALL" call) ("RETURN" return)
     ("READ" read) ("PRINT" print)     ; I/O
     ("BEGIN" begin) ("END" end)
     ("IF" if) ("THEN" then)           ; branch keywords
     ("WHILE" while) ("DO" do)         ; loop keywords
     
     ("![^!]*!" comment))
   'comment
   'string))
