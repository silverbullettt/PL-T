CONST
  m(85), n(7), pi(3.14), name("TT"), right?(#t);

VAR
  x:INT, y(100), s("foo"), ok?:BOOL;

PROCEDURE divide(r:INT, w:INT) -> (INT)
 VAR q(0);
 BEGIN
  WHILE (<= w r) DO w := (* 2 w);
  WHILE (> w y) DO BEGIN
   q := (* 2 q);
   w := (/ w 2);
   IF (<= w r) THEN BEGIN
    r := (- r w);
    q := (+ q 1)
   END
  END;
  RETURN q
 END;

PROCEDURE gcd(f:INT, g:INT) -> (INT)
 BEGIN
  f := x;
  g := y;
  WHILE (# f g) DO BEGIN
   IF (< f g) THEN g := (- g f);
   IF (< g f) THEN f := (- f g)
  END;
  RETURN f
 END;

PROCEDURE nothing() -> ()
 PRINT "I don't care.";

PROCEDURE divide2(x:INT, y:INT) -> (INT)
 RETURN (/ x y);

BEGIN
 x := m;
 y := n;
 PRINT CALL divide(x, y);
 x, y := 84, 36;
 PRINT CALL gcd(x, y);
 s := (@ "abc" (@ "def" "ghi"));
 PRINT s;
 ok? := (AND 100 x y #f);
 PRINT ok?,100,x,y,#f;
 s := (<- "I'm ~a, ~a years old, I hate ~a!!!" "TT" 21 s);
 PRINT s, ok?;
 CALL nothing();
 PRINT CALL divide2((* 20 5), 3)
END.
