* PRG: EDITEMP.PRG
* THIS REVISION: 20-07-2017

DO IniSet1
DO SetVars
DO OpenDbfs

DO SlctEmp

DO EditEmp1
DO EditEmp2

IF salary.probation = .F. AND salary.working = .t. AND salary.mpay = .T.
	DO EditEmp3
ENDIF

ON KEY LABEL ENTER
CLEAR
CLOSE DATA


RETURN
*****************



*****************
PROCEDURE IniSet1
*****************

CLEA
CLOSE DATA

SET EXCLUSIVE ON
SET TALK OFF
SET SAFETY OFF

SET FUNCTION F1 TO
SET FUNCTION F2 TO

SET FUNCTION F5 TO
SET FUNCTION F10 TO



RETURN
*************


*******************
PROCEDURE SetVars
*******************

PUBLIC mMCode,mYear

mMcode = 0
mYear = 0


RETURN
***************



*******************
PROCEDURE OpenDbfs
*******************
CLEAR

USE attend
STORE mcode TO mmcode
STORE year TO myear
USE

SELECT 1
USE employee ORDER empno
REPLACE ALL empsrl WITH RECNO()

SELECT 2
USE attend ORDER empsrl

SELECT 3
USE ovtime ORDER empsrl

SELECT 4
USE artime ORDER empsrl

SELECT 5
USE dptime ORDER empsrl

SELECT 6
USE deduct ORDER empsrl
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear

SELECT 7
USE salary ORDER empsrl

SELECT 8
USE salcode EXCLUSIVE
INDEX ON salcode TAG salcode
SET ORDER TO salcode


SELECT 9
USE deptt ORDER deptcode


SELECT salary
SET RELATION TO salcode INTO salcode ADDITIVE


SELECT 1
set filter to salary.empno <> 0

SELECT employee
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO empsrl INTO attend ADDITIVE
SET RELATION TO empsrl INTO ovtime ADDITIVE
SET RELATION TO empsrl INTO artime ADDITIVE
SET RELATION TO empsrl INTO dptime ADDITIVE
SET RELATION TO empsrl INTO deduct ADDITIVE
SET RELATION TO empsrl INTO salary ADDITIVE

RETURN
***************



*****************
PROCEDURE SlctEmp
*****************
CLEAR

ON KEY LABEL ENTER DO Slected
ON KEY LABEL F1 DO Ord1
ON KEY LABEL F2 DO Ord2

CLEAR
@ 1,20 SAY "Select Employee you want to Edit: "
@ 8,55 SAY "To Change Order:"
@ 10,55 SAY "F1 - Number Wise"
@ 12,55 SAY "F2 - Name Wise"


DEFINE WINDOW BrEmp FROM 3,20 TO 19,52 DOUBLE

BROW FIELDS;
	deptcode :2 :P="@L" :H=" C",;
	empno :3 :P="@L" :H="ode",;
	name :H= "       Name   ";
WINDOW BrEmp;
WHEN ShowInf();
NOEDIT NODELETE NOAPPEND

ON KEY LABEL ENTER
ON KEY LABEL F1
ON KEY LABEL F2

CLEAR

RETURN
*************


*****************
PROCEDURE ShowInf
*****************

@ 21,22 SAY Deptt.DeptName

RETURN .T.
***********


*****************
PROCEDURE Slected
*****************

RELEASE WINDOW Employee

ON KEY LABEL ENTER
ON KEY LABEL F1
ON KEY LABEL F2


RETURN
********


****************
PROCEDURE Ord1
****************

SET ORDER TO empno

KEYBOARD '{PGUP}'
KEYBOARD '{PGUP}'
LOCATE FOR deptcode = 1 AND empno = 1

RETURN
*******


****************
PROCEDURE Ord2
****************

SET ORDER TO name

KEYBOARD '{PGUP}'
KEYBOARD '{PGUP}'


LOCATE FOR name = "A"


RETURN
*******

