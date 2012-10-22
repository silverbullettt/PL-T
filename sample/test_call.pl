VAR x1, x2;

PROCEDURE divide(dividend:INT, divisor:INT) -> (INT, INT)
 VAR quotient:INT, reminder:INT;
 BEGIN
  quotient := (/ dividend divisor);
  reminder := (- dividend (* quotient divisor));
  RETURN quotient, reminder
 END;

PROCEDURE middle(x:INT, y:INT) -> (INT)
 RETURN (/ (+ x y) 2);

PROCEDURE foo() -> (INT)
 PROCEDURE bar1() -> (INT)
  RETURN 123;
 RETURN CALL bar1();

PROCEDURE foo1() -> (VAR)
 RETURN 456;

PROCEDURE recur(x:INT) -> (INT)
 BEGIN
  IF (>= x 10) THEN RETURN x;
  PRINT (<- "recur: ~a" x);
  RETURN CALL recur((+ x 1))
 END;
 
!PROCEDURE max(x:VAR, y:VAR, less:(VAR, VAR)->(BOOL)) -> (VAR)!
! BEGIN!
!  IF CALL less(x, y) THEN RETURN y;!
!  RETURN x!
! END;!

!PROCEDURE addn(n:INT) -> ((INT)->(INT))!
! return a closure                      !
! PROCEDURE adder(x:INT) -> (INT)       !
!  RETURN (+ n x);                      !
! RETURN adder;                         !

BEGIN
 x1, x2 := CALL divide((+ CALL middle(23, CALL foo()) 100), 17);
 PRINT x1, x2;
 PRINT 12, (+ 100 CALL foo1());
 x1 := CALL middle(CALL divide(178, 23));
 x2 := (+ 100 CALL foo());
 PRINT CALL recur(0)
END.
