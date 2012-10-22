CONST iarr([1,2,3]);
VAR arr:INT[], arr2:VAR[], sarr:STRING[], barr:BOOL[], i:INT;

PROCEDURE show-array(arr:VAR[]) -> ()
 VAR x;
 FOREACH (x IN arr) DO
  PRINT x;

PROCEDURE show-numbers(arr:INT[]) -> ()
 VAR x:INT;
 FOREACH (x IN arr) DO
  PRINT x;

PROCEDURE multi10(arr:INT[]) -> (INT[])
 VAR res(INT[SIZE(arr)]), i(0);
 BEGIN
  WHILE (< i SIZE(arr)) DO BEGIN
   res[i] := (* arr[i] 10);
   i := (+ i 1)
  END;
  RETURN res
 END;

PROCEDURE foo()->(STRING)
 RETURN "foo";

BEGIN
 arr := INT[10];
 i := 0;
 WHILE (< i SIZE(arr)) DO BEGIN
  arr[i] := i;
  i := (+ i 1)
 END;
 CALL show-numbers(arr);
 arr := NULL;
 i := NULL;
 arr2 := [ "Liyuan", "Tian", "Himo", CALL foo() ];
 CALL show-array(arr2);
 CALL show-array([ 2012, 12, 21 ]);
 FOREACH (i IN [ 49, 19, 91 ]) DO
  PRINT (<- "~a |" i);
 PRINT CALL multi10([6, 5, 4]);
 PRINT (> [1,2,3] [4,5,6]);
 PRINT (<- "size of ~a: ~a" [ 1, 2, 3 ] SIZE([ 1, 2, 3 ]))
END.
