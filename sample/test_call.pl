VAR x1, x2;

PROCEDURE foo(bar:INT, yeah:STRING, pi) !-> (INT, STRING, VAR)!
 PRINT bar, yeah, pi;
 
BEGIN
 x1, x2 := 49, "TT";
 CALL foo(x1, x2, 3.14)
END.
