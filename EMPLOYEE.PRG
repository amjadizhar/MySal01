* Entering new employee

DO IniSet1
DO SetVars
DO OpenDbfs
DO PopDept
DO newemp2

PROCEDURE IniSet1
CLEA
CLEA ALL
RETURN

PROCEDURE SetVars
PUBLIC mmcode,myear,mNewDpt,mNewEmpNo
mmcode = 0
myear = 0
mNewDpt = 0
mNewEmpNo = 0
RETURN

PROCEDURE OpenDbfs
USE attend
STORE mcode TO mmcode
STORE year TO myear
USE

SELECT 1
USE employee ORDER empno

SELECT 2
USE attend ORDER empno2

SELECT 3
USE ovtime ORDER empno2

SELECT 4
USE artime ORDER empno2

SELECT 5
USE dptime ORDER empno2

SELECT 6
USE deduct ORDER empno2
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear

SELECT 7
USE salary ORDER empno

SELECT 8
USE salcode ORDER salcode

SELECT 9
USE deptt ORDER deptcode

SELECT salary
SET RELATION TO salcode INTO salcode ADDITIVE

SELECT employee
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO attend ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO ovtime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO artime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO dptime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO deduct ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
RETURN


PROCEDURE PopDept
SELECT deptt
DEFINE POPUP Deptt FROM 3,5 TO 8,45;
PROMPT FIELDS str(deptcode,2,0)+"³"+deptname;
SCROLL
ON SELECTION POPUP Deptt DO AskDept

ACTIVATE POPUP deptt
SELECT employee
RETURN


PROCEDURE AskDept
mNewDpt = ROUND(VAL(LEFT(PROMPT(),2)),0)
SELECT employee
SET FILTER TO
SET FILTER TO deptcode = mNewDpt
GOTO BOTT
mNewEmpNo = empno+1
SET FILTER TO
DEACTIVATE POPUP deptt
RETURN