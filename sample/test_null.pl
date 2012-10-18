VAR x, y, z:INT;

PROCEDURE null?(x:VAR) -> (BOOL)
 RETURN (= x NULL);

PROCEDURE getNull() -> (VAR, INT)
 RETURN NULL, NULL;

PROCEDURE hund() -> (INT)
 RETURN 100;

PROCEDURE foo(x:INT) -> (INT)
 BEGIN
  IF (= x NULL) THEN RETURN x;
  IF (< x 10) THEN BEGIN
   PRINT x;
   RETURN CALL foo((+ x 1))
  END;
  IF (>= x 10) THEN RETURN NULL
 END;

BEGIN
 PRINT CALL getNull();
 x := CALL null?(y);
 PRINT x;
 y := CALL null?(x);
 PRINT CALL foo(0);
 x := NULL;
 PRINT (<- "x = z? ~a" (= x z))
END.
