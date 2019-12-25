* PROGRAM : ENDEMP.PRG 
* Ending an employee, removing name from attendence register
* THIS REVISION: 17-MAY-2017


DO IniSet1
DO RepEmpSr
DO SetVars
DO OpenDbfs
DO AskEmpno
DO Confirm
DO Confirm2
DO EndProg


******************
PROCEDURE IniSet1
*******************

set exclusive on

CLEA
CLOSE DATA

SET TALK OFF
SET DELETED ON
SET DATE TO BRITISH
SET CENT ON



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
USE employee EXCL
SET ORDER TO empno

SELECT 2
USE attend EXCL
SET ORDER TO empsrl

SELECT 3
USE ovtime EXCL
SET ORDER TO empsrl

SELECT 4
USE artime EXCL
SET ORDER TO empsrl

SELECT 5
USE dptime EXCL
SET ORDER TO empsrl

SELECT 6
USE deduct EXCL
SET ORDER TO empsrl
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear

SELECT 7
USE salary EXCL
SET ORDER TO empsrl

SELECT 8
USE salcode ORDER salcode

SELECT 9
USE deptt ORDER deptcode

SELECT salary
SET RELATION TO salcode INTO salcode ADDITIVE

SELECT employee
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO empsrl INTO attend ADDITIVE
SET RELATION TO empsrl INTO ovtime ADDITIVE
SET RELATION TO empsrl INTO artime ADDITIVE
SET RELATION TO empsrl INTO dptime ADDITIVE
SET RELATION TO empsrl INTO deduct ADDITIVE
SET RELATION TO empsrl INTO salary ADDITIVE
RETURN





*******************
PROCEDURE RepEmpSr
*******************
CLEAR


=Center1("Ordering Employees Serial Numbers. Please WAIT ... ")


USE employee ORDER empno
REPLACE ALL empsrl WITH RECNO() FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE attend ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE artime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE ovtime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE dptime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE salary ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA

SELECT 1
USE deduct ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA

SELECT 1
USE saldtail ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA


SELECT 1
USE leaves ORDER EMPNO
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl FOR EMPTY(empsrl)
CLOSE DATA



RETURN
*************************



*******************
PROCEDURE AskEmpno
*******************
PUBLIC mDptCode,mEmpNo

mDptCode = 0
mEmpNo = 0

CLEAR
@ 5,5 SAY "Enter Empno to Delete from Attendence: "
@ ROW(),COL() GET mDptCode PICT "99" VALID VmDptCode() 
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET mEmpNo PICT "999" VALID VmEmpNo()
READ

RETURN
*************************

********************
PROCEDURE VmDptCode
********************

SELECT deptt
LOCATE FOR deptcode = mDptCode
IF FOUND()
	@ 8,0 TO 8,79 CLEAR
	@ 8,5 SAY "Department Name: "
	@ ROW(),COL() SAY deptname
	SELECT 1
	RETURN .T.
ELSE
	@ 8,0 TO 8,79 CLEAR
	WAIT WINDOW "Deptt.Code Not in File! Try Again" TIMEOUT 1
	SELECT 1
	RETURN .F.
ENDIF
SELECT 1
RETURN .T.
*********************


******************
PROCEDURE VmEmpNo
******************

SELECT 1
LOCATE FOR deptcode = mDptCode AND empno = mEmpNo

DO CASE
	CASE FOUND() AND attend.deptcode <> 0
		@ 10,0 TO 10,79 CLEAR
		@ 10,5 SAY "Employee Name: "
		@ ROW(),COL() SAY name
		SELECT 1
		RETURN .T.

	CASE FOUND() AND attend.deptcode = 0
		@ 10,0 TO 10,79 CLEAR
		WAIT WINDOW "Employee is already not on Attendence File!" TIMEOUT 1
		SELECT 1
		_CUROBJ = 1
		RETURN .T.


	CASE NOT FOUND()
		@ 10,0 TO 10,79 CLEAR
		WAIT WINDOW "Employee Code Not in File! Try Again" TIMEOUT 1
		SELECT 1
		_CUROBJ = 1
		RETURN .T.
ENDCASE

SELECT 1

RETURN .T.
*************


*******************
PROCEDURE Confirm
*******************
PUBLIC mConfirm
mConfirm = SPACE(1)

@ 12,5 SAY "Do you Want to Delete Employee from Attendence (Y/N) ? "
@ ROW(),COL() GET mConfirm PICT "@! X" VALID INLIST(mConfirm,'Y','N');
				ERROR "(Y)es,(N)o"

READ

RETURN
*******************


********************
PROCEDURE Confirm2
********************
DO CASE 
	CASE mConfirm = "N"
		RETURN
	CASE mConfirm = "Y"
		DO SeeAnyAtt


ENDCASE
RETURN
*********************


*********************
PROCEDURE SeeAnyAtt
*********************

SELECT attend
DO CASE
	CASE PblDays = 0
		SELECT 1
		DO DelEmp1
	CASE PblDays <> 0
		SELECT 1
		DO CnfAgain
ENDCASE
RETURN
***********************


*********************
PROCEDURE CnfAgain
*********************
PUBLIC mConfirm
mConfirm = SPACE(1)

@ 14,5 SAY "This Employee has attendence Record! Do You Still Want to Delete (Y/N) ? "
@ ROW(),COL() GET mConfirm PICT "@! X" VALID INLIST(mConfirm,'Y','N');
				ERROR "(Y)es,(N)o"

READ

DO CASE
	CASE mConfirm = "N"
		SELECT 1
		RETURN
	CASE mConfirm = "Y"
		DO DelEmp2	
ENDCASE	


RETURN
******************

************************
PROCEDURE DelEmp1
************************
CLOSE DATA

USE employee EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE salary EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE attend EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE ovtime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE artime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE dptime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE deduct EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE saldtail EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK


RETURN
*****************

************************
PROCEDURE DelEmp2
************************

CLOSE DATA

USE employee EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE salary EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE attend EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE ovtime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE artime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE dptime EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE deduct EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK

USE saldtail EXCL
DELETE FOR deptcode = mDptCode AND empno = mEmpNo AND mcode = mMcode AND year = mYear
PACK



RETURN
*****************


***************************
PROCEDURE EndProg
***************************
CLEAR
CLEAR WINDOWS
CLOSE DATABASES
SHOW POPUP ALL
RESTORE SCREEN FROM backgrnd
RETURN
***************************



********************
PROCEDURE Center1
********************

PARAMETER mtext

mlength = LEN(TRIM(mtext))

IF mlength > WCOLS()
	mcolumn = 0
ELSE
	mcolumn = (WCOLS() - LEN(TRIM(mtext))) / 2
ENDIF

mrow = WROWS() / 2

@ mrow,mcolumn SAY TRIM(mtext)

RETURN
*********************