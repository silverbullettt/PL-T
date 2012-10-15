CONST
  m(85), n(7), pi(3.14), name("TT"), right?(#t);

VAR
  x:INT, y(100), z:INT, q(m), r((+ 1 2 3)), s("foo"), ok?:BOOL;

PROCEDURE divide;
 VAR w;
 
 PROCEDURE foo;
 ! PROCEDURE foo(bar:INT, yeah:STRING, hey) -> (INT, STRING, VAR); !
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
 PRINT r;
 x := m;
 y := n;
 CALL divide;
 PRINT q;
 x, y := 84, 36;
 CALL gcd;
 PRINT z;
 !READ z;!
 x, y, z := 1.0, 2.1, 3;
 s := (@ "abc" (@ "def" "ghi"));
 PRINT s;
 ok? := (AND 100 x y #f);
 PRINT ok?,100,x,y,#f;
 s := (<- "I'm ~a, ~a years old, I hate ~a!!!" "TT" 21 s);
 PRINT s, ok?
END.
