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

PROCEDURE divide;
 VAR w;

 BEGIN
  r := x;
  q := 0;
  w := y;
  WHILE (<= w r) DO w := (* 2 w);
  WHILE (> w y) DO BEGIN
   q := (* 2 q);
   w := (/ w 2);
   IF (<= w r) THEN BEGIN
    r := (- r w);
    q := (+ q 1)
   END
  END
 END;

BEGIN
 ! x,y := 1,2; !
 !READ m;!
 x := m;
 x := TRUE;
 y := (AND TRUE FALSE);
 r := (+ (* 1 2) (- 10 3 2 1) x);
 CALL foo;
 IF TRUE THEN PRINT x;
 IF (> 100 x) THEN PRINT x;
 IF (AND 100 x y FALSE) THEN PRINT x;
 PRINT z
END.
