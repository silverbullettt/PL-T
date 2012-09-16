CONST
  m = 85,
  n = 7;
 
VAR
  x, y, z, q, r;

PROCEDURE foo;
 VAR x;
 BEGIN
  q := TRUE;
  CALL bar
 END;

PROCEDURE bar;
 BEGIN
  READ x
 END;

BEGIN
 x := m;
 m := TRUE;
 y := (AND TRUE FALSE);
 r := (+ (* 1 2) (- 10 3 2 1) x);
 CALL foo;
 IF TRUE THEN PRINT x;
 IF (> 100 x) THEN PRINT x;
 IF (AND 100 x FALSE) THEN PRINT x;
 PRINT z
END.
