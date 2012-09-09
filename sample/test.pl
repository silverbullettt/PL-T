CONST
  m = 85,
  n = 7;
 
VAR
  x, y, z, q, r;

PROCEDURE divide;
 VAR w;
 
 PROCEDURE foo;
  !! this is a comment
  VAR x, bar;
  BEGIN
   PRINT 777;
   x := 999;
   z := 123
  END;

BEGIN
 r := x;
 q := 0;
 w := y;
 WHILE w <= r DO w := 2 * w;
 WHILE w > y DO BEGIN
  CALL foo;
  q := 2 * q;
  w := w / 2;
  IF w <= r THEN BEGIN
   r := r - w;
   q := q + 1
  END
 END
END;

PROCEDURE gcd;
VAR f, g;
BEGIN
  f := x;
  g := y;
  WHILE f # g DO BEGIN
    IF f < g THEN g := g - f;
    IF g < f THEN f := f - g
  END;
  z := f
END;

PROCEDURE func1;
VAR x;
CALL func2;

PROCEDURE func2;
PRINT x;

BEGIN
 PRINT 1234;
 x := m;
 y := n;
 CALL divide;
 PRINT q;
 x := 84;
 y := 36;
 CALL gcd;
 PRINT z
END.
