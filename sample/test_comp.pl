VAR x1, x2;
BEGIN
 READ x1;
 READ x2;
 IF (< x1 x2) THEN
  PRINT (<- "~a < ~a" x1 x2);
 IF (> x1 x2) THEN
  PRINT (<- "~a > ~a" x1 x2);
 IF (= x1 x2) THEN
  PRINT (<- "~a = ~a" x1 x2)
END.
