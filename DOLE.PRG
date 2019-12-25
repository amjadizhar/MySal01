mdate = {10/20/95}
dole = { / / }

IF .NOT. EMPTY(dole) .AND. dole < mdate
clear
@ 10,10 SAY "Employee Left Employment"


ELSE
@ 10,10 SAY "Employee on Employment      :"
ENDIF