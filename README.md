# **PL/0 virtual machine instructions set**

<pre><code>
; instruction set:
; (decl var)          -> declare a new variable
; (set e var)         -> set the value of var to e
; (call entry)        -> call dest entry, and push pc + 1 to stack
; (return)            -> return the addr which in the top of stack
; (if-false e addr)   -> if e is #f, go to addr
; (goto addr)         -> go to addr directly
; (arth-op e1 e2 var) -> set the value of e1 arth-op e2 to var
; (cond-op e1 e2 var) -> set the value of e1 cond-op e2 to var
; (odd e var)         -> if e is odd, set var to #t, or #f
; (print e)           -> print the value of e to screen
</code></pre>
