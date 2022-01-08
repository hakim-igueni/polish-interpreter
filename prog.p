COMMENT Calcul de la factorielle
READ n
i := 1
r := 1

IF i <= 1
  PRINT n
  
WHILE i <= n
  r := * i r
  WHILE i <= n
    r := * i r
    i := + i 1
    i := + i 1
PRINT r