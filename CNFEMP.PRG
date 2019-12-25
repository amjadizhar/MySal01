set talk off
CLEAR
CLOSE DATA

DO CnfEmp1
DO CnfEmp2
DO Confirm1

IF mGoAhead = .T.
	DO ConfEmp
	DO CnfEmp3
ENDIF


RETURN
********************


********************
PROCEDURE Confirm1
********************

mYn = space(1)
@ 9,10 SAY "Is it OK to Confirm This Employee ? "

	@ ROW(),COL()+2 GET mYn PICT "@!" VALID INLIST(mYn,'Y','N');
				ERROR "(Y)es,(N)o";
				MESSAGE "(Y)es, (N)o"


READ

PUBLIC mGoAhead
mGoAhead = .F.

IF mYn = "Y"
	mGoAhead = .T.
ENDIF

IF mYn = "N"
	mGoAhead = .F.
	REPLACE employee.doc WITH {  /  /  }
ENDIF

RETURN
*********************



******************
PROCEDURE ConfEmp
******************

REPLACE employee.probation WITH .F.
REPLACE salary.mpay WITH .T.
REPLACE salary.wrdaily WITH .F.

RETURN
********************



