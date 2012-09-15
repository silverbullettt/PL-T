CONST
  m = 85,
  n = 7;
 
VAR
  x, y, z, q, r;

BEGIN
 x := m;
 y := (AND TRUE FALSE);
 z := (NOT 100);
 r := (+ (* 1 2) (- 10 3 2 1));
 x := TRUE;
 IF TRUE THEN PRINT x;
 IF (> 100 x) THEN PRINT x;
 IF (AND 100 x FALSE) THEN PRINT x;
 PRINT z
END.
