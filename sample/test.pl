CONST
  m(85), n(7), pi(3.14), name("TT"), right?(TRUE);

VAR
  x:INT, y(100), z:INT, q(m), r((+ 1 2 3)), s("foo"), ok?:BOOL;

PROCEDURE divide;
 VAR w;
 
 PROCEDURE foo;
  ! this is a comment !
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

PROCEDURE gcd;
VAR f:INT, g:INT;
BEGIN
  f := x;
  g := y;
  WHILE (# f g) DO BEGIN
    IF (< f g) THEN g := (- g f);
    IF (< g f) THEN f := (- f g)
  END;
  z := f
END;

PROCEDURE func1;
VAR x, y123Y, z_f;
CALL func2;

PROCEDURE func2;
PRINT x;

BEGIN
 PRINT 1234;
 x := m;
 y := n;
 CALL divide;
 PRINT q;
 x, y := 84, 36;
 CALL gcd;
 PRINT z;
 !READ z;!
 x, y, z := 1.0, 2.1, 3;
 s := (@ "abc" " , " "def");
 PRINT s;
 READ s;
 s := (<- "I'm ~a, ~a years old, I love ~a" "TT" 21 s);
 PRINT s;
 PRINT x, y, z
END.
