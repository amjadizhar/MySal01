* PROGRAM: EMDT1.PRG
* End Month Data Transfer one
* Transfering Days,overtime totals to saldtail.dbf
* Entering and Editing Deductions,Inst.Advances,Mid.Advances.
********************************

DO mainprog

*******************
PROCEDURE mainprog
********************

DO iniset
DO UpdDays
DO UpdOthrs
DO UpdBonday
DO DedNewDat
DO UpdPfund
DO ConfDed
DO EndProg
RETURN
****************************

***************************
PROCEDURE Iniset
***************************
PUBLIC Pwait1
CLEAR
CLEAR WINDOWS
HIDE POPUP mainmenu
@ 10,30 SAY "Please Wait...."
SAVE SCREEN TO Pwait1
CLEAR MACROS
SET TALK OFF
SET FUNCTION F1 TO
SET FUNCTION F10 TO
RETURN
***************************

***************************
PROCEDURE UpdDays
***************************

CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
SELECT 1
USE attend ORDER empno
SELECT 2
USE saldtail ORDER empno
SELECT 3
USE salsheet ORDER sheetno
GOTO BOTTOM
STORE sheetno TO LastSheet

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE

DO CASE

CASE saldtail.mcode = 0
	SELECT 2
	APPEND FROM attend
	REPLACE ALL sheetno WITH LastSheet FOR EMPTY(sheetno)
	

CASE saldtail.mcode <> 0
	STORE mcode TO mmcode
	SELECT 2
	
	DELETE FOR mcode = mmcode
	PACK
	
	APPEND FROM attend
	REPLACE ALL sheetno WITH 4 FOR EMPTY(sheetno)
	
ENDCASE

SELECT 1
REPLACE ALL saldtail.days WITH Present+Late

RETURN
*******************************************
***************************
PROCEDURE UpdOthrs
***************************

CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
SELECT 1
USE ovtime ORDER empno
SELECT 2
USE saldtail ORDER empno
SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE
REPLACE ALL saldtail.othrs WITH totalot
RETURN

***************************

***************************
PROCEDURE UpdBonday
***************************

CLEAR
CLOSE DATABASES

SELECT 1
USE attend
STORE mcode TO mmcode
STORE year TO myear
USE

USE msetting
SET FILTER TO mcode = mmcode AND year = myear
STORE bonday TO mbonday
USE


RESTORE SCREEN FROM Pwait1
SELECT 1
USE saldtail ORDER empno
SET FILTER TO mcode = mmcode AND year = myear
SELECT 2
USE salary ORDER empno
SELECT 3
USE month ORDER mcode

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE



REPLACE ALL bonday WITH mbonday;
	        FOR mcode = mmcode AND year = myear;
	        AND salary.wrdaily = .t.;
	        AND days = (month.mdays - month.frds)
RETURN
***************************



*******************************************
PROCEDURE DedNewDat
*******************************************

CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
SELECT 1
USE attend ORDER empno
STORE mcode TO mmcode
SELECT 2
USE deduct ORDER empno
SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO deduct ADDITIVE




IF deduct.mcode = 0
	SELECT 2
	APPEND FROM attend
	REPLACE ALL sheetno WITH 3 FOR EMPTY(sheetno)
	SELECT 1	
	DO UpdAdv
ENDIF

RETURN
***************************

***************************
PROCEDURE UpdAdv   && If Data has been newly appended from Attend
***************************


CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
USE attend
STORE mcode TO mmcode
CLOSE DATABASES
SELECT 1
	USE instded ORDER empno
SELECT 2
	USE deduct ORDER empno
	SET FILTER TO 
	SET FILTER TO mcode = mmcode

SELECT 1
	SET RELATION TO str(deptcode)+str(empno) INTO deduct ADDITIVE
	REPLACE ALL deduct.advance WITH Inst FOR NOT DELETED()
RETURN	
***************************


***************************
PROCEDURE UpdPfund
***************************

CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
USE attend
STORE mcode TO mmcode
CLOSE DATABASES

SELECT 1
	USE deduct ORDER empno
	SET FILTER TO 
	SET FILTER TO mcode = mmcode

SELECT 2
	USE salary ORDER empno

SELECT 3
	USE employee ORDER empno
	
SELECT 1
	SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
	SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
	
	REPLACE ALL pfund WITH salary.basic/100*6 FOR NOT employee.probation

RETURN

***************************
PROCEDURE ConfDed
***************************
CLEAR
CLOSE DATABASES
RESTORE SCREEN FROM Pwait1
USE attend
STORE mcode TO mmcode
CLOSE DATABASES

CLEAR
CLOSE DATABASES

SELECT 1
USE deduct ORDER empno
SET FILTER TO
SET FILTER TO mcode = mmcode
GOTO TOP

SELECT 2
USE employee ORDER empno

SELECT 3
USE month ORDER mcode

SELECT 4
USE instded ORDER empno

SELECT 5
USE salary ORDER empno

SELECT 6
USE saldtail ORDER empno
SET FILTER TO
SET FILTER TO mcode = mmcode

SELECT 7
USE salcode ORDER salcode

SELECT 5
SET RELATION TO salcode INTO salcode


SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO instded ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO saldtail ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE

CLEAR
DEFINE WINDOW full FROM 0,0 TO 24,79 DOUBLE
ACTIVATE WINDOW full
DEFINE WINDOW below1 FROM 12,0 TO 22,77 DOUBLE IN WINDOW FULL
ACTIVATE WINDOW below1

DEFINE WINDOW dedbrow FROM 1,1 TO 12,78


BROWS FIELDS;
	Eno = RIGHT(str(deptcode),2)+"-"+;
		RIGHT(str(empno),3)+"�";
		:R :H = "Code",;
	Ename = employee.name :H = "Employee Name",;
	L1 = "�" :1 :R :H = "",;
	advance :V = chkadv() :H = "Inst.Adv" :P = "@Z",;
	L2 = "�" :1 :R :H = "",;
	midadv :V = ChkMidAdv() :H = "Mid.Adv" :P = "@Z",;
	L3 = "�" :1 :R :H = "",;
	pfund :R :H = "P.Fund" :P = "@Z",;
	L4 = "�" :1 :R :H = "",;
	other :V = ChkOther() :H = "Other" :P = "@Z",;
	L5 = "�" :1 :R :H = "",;
	itax :H = "I.Tax" :P = "@Z";
	WINDOW dedbrow;
	WHEN ShowSal();
	TITLE "Data Input for Deductions"+"  "+RTRIM(month.month)+;
		  ","+ALLTRIM(str(year))
	
	
CLEAR
CLEAR WINDOWS
CLOSE DATABASES

RETURN
***************************



******************
PROCEDURE ShowSal
******************

IF salary.mpay

GrossPay = salary.basic+salary.hrent+salary.convy+salary.medical+;
		   salary.cma+salary.pma+salary.other+salary.special

ELSE
GrossPay = ROUND(salary.dwrate*saldtail.days,0)+ROUND(salary.otrated*saldtail.othrs,0)+;
		   salary.dwratem+(saldtail.bonday * salary.dwrate)
ENDIF



@ 1,0 TO 8,76 CLEAR
@ 1,0 FILL TO 8,76 COLOR GR+/RB


DO CASE

	CASE salary.mpay
		
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name+" "+salcode.descrip+space(6) COLOR W+/R


		IF salary.basic <> 0
		@ 1,1 SAY "Basic" COLOR GR+/RB
		@ 2,1 SAY salary.basic PICT "9999" COLOR GR+/RB
		ENDIF

		IF salary.hrent <> 0
		@ 1,7 SAY "H.R." COLOR GR+/RB
		@ 2,6 SAY "+" COLOR GR+/RB
		@ 2,7 SAY salary.hrent PICT "9999" COLOR GR+/RB
		ENDIF

		IF salary.convy <> 0
		@ 1,14 SAY "Conv" COLOR GR+/RB
		@ 2,13 SAY "+" COLOR GR+/RB
		@ 2,14 SAY salary.convy PICT "9999" COLOR GR+/RB
		ENDIF

		IF salary.medical <> 0
		@ 1,19 SAY "Med." COLOR GR+/RB
		@ 2,18 SAY "+" COLOR GR+/RB
		@ 2,19 SAY salary.medical PICT "9999" COLOR GR+/RB
		ENDIF
		
		IF salary.cma <> 0
		@ 1,24 SAY "Cma" COLOR GR+/RB
		@ 2,23 SAY "+" COLOR GR+/RB
		@ 2,24 SAY salary.cma PICT "9999" COLOR GR+/RB
		ENDIF

		IF salary.pma <> 0
		@ 1,29 SAY "Pma" COLOR GR+/RB
		@ 2,28 SAY "+" COLOR GR+/RB
		@ 2,29 SAY salary.pma PICT "9999" COLOR GR+/RB
		ENDIF
		
		IF salary.other <> 0
		@ 1,34 SAY "Oth." COLOR GR+/RB
		@ 2,33 SAY "+" COLOR GR+/RB
		@ 2,34 SAY salary.other PICT "9999" COLOR GR+/RB
		ENDIF
		
		IF salary.special <> 0
		@ 1,39 SAY "Spec" COLOR GR+/RB
		@ 2,38 SAY "+" COLOR GR+/RB
		@ 2,39 SAY salary.special PICT "9999" COLOR GR+/RB
		ENDIF

		@ 1,46 SAY "Gross" COLOR GR+/RB
		@ 2,44 SAY "=" COLOR GR+/RB
		@ 2,46 SAY GrossPay PICT "99,999" COLOR GR+/RB				

	IF saldtail.othrs <> 0 
		@ 3,1 SAY " Over Time Calculation"+space(20) COLOR W/G+
	ENDIF

		IF saldtail.othrs <> 0 
		@ 4,3 SAY "OT Hrs" COLOR GR+/RB
		@ 5,3 SAY saldtail.othrs PICT "999.99" COLOR GR+/RB
		ENDIF

		IF salary.otrated <> 0 .AND. saldtail.othrs <> 0
		@ 4,12 SAY "OT Rate" COLOR GR+/RB
		@ 5,10 SAY "x" COLOR GR+/RB
		@ 5,12 SAY salary.otrated PICT "999.99" COLOR GR+/RB
		ENDIF

		IF saldtail.othrs <> 0 .AND. salary.otrated <> 0
		@ 4,22 SAY "Tot.OT" COLOR GR+/RB
		@ 5,20 SAY "=" COLOR GR+/RB
		@ 5,22 SAY ROUND(salary.otrated * saldtail.othrs,0) PICT "9999" COLOR GR+/RB
		ENDIF

	IF (advance+midadv+other+pfund+itax) <> 0
		@ 6,1 SAY " Deductions"+space(31) COLOR W/G+
	ENDIF

	CASE NOT salary.mpay

		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name+" "+salcode.descrip+space(7) COLOR W/GR+

		IF saldtail.days <> 0
		@ 1,2 SAY "Days" COLOR GR+/RB
		@ 2,0 SAY "(" COLOR GR+/RB
		@ 2,2 SAY saldtail.days PICT "99" COLOR GR+/RB
		ENDIF

		IF salary.dwrate <> 0 .AND. saldtail.days <> 0
		@ 1,8 SAY "Rate" COLOR GR+/RB
		@ 2,6 SAY "x" COLOR GR+/RB
		@ 2,8 SAY salary.dwrate PICT "99" COLOR GR+/RB
		ENDIF

		IF saldtail.days <> 0 .AND. salary.dwrate <> 0
		@ 1,13 SAY "Wages" COLOR GR+/RB
		@ 2,12 SAY "=" COLOR GR+/RB
		@ 2,13 SAY salary.dwrate * saldtail.days PICT "9999" COLOR GR+/RB
		ENDIF

		IF saldtail.othrs <> 0 
		@ 1,22 SAY "OT Hrs" COLOR GR+/RB
		@ 2,18 SAY ")" COLOR GR+/RB
		@ 2,19 SAY "+" COLOR GR+/RB
		@ 2,20 SAY "(" COLOR GR+/RB
		@ 2,22 SAY saldtail.othrs PICT "999.99" COLOR GR+/RB
		ENDIF

		IF salary.otrated <> 0 .AND. saldtail.othrs <> 0
		@ 1,30 SAY "OT Rate" COLOR GR+/RB
		@ 2,29 SAY "x" COLOR GR+/RB
		@ 2,30 SAY salary.otrated PICT "999.99" COLOR GR+/RB
		ENDIF

		IF saldtail.othrs <> 0 .AND. salary.otrated <> 0
		@ 1,38 SAY "Tot.OT" COLOR GR+/RB
		@ 2,37 SAY "=" COLOR GR+/RB
		@ 2,38 SAY ROUND(salary.otrated * saldtail.othrs,0) PICT "9999" COLOR GR+/RB
		@ 2,43 SAY ")" COLOR GR+/RB
		ENDIF

		IF saldtail.bonday = 0
		@ 1,46 SAY "Gross" COLOR GR+/RB
		@ 2,44 SAY "=" COLOR GR+/RB
		@ 2,46 SAY GrossPay PICT "99,999" COLOR GR+/RB
		ELSE
		@ 1,45 SAY "Bns" COLOR GR+/RB
		@ 2,44 SAY "+" COLOR GR+/RB
		@ 2,45 SAY saldtail.bonday * salary.dwrate PICT "999" COLOR GR+/RB
		@ 1,50 SAY "Gross" COLOR GR+/RB
		@ 2,49 SAY "=" COLOR GR+/RB
		@ 2,50 SAY GrossPay PICT "99,999" COLOR GR+/RB
		ENDIF

	IF (advance+midadv+other+pfund+itax) <> 0
		@ 6,1 SAY " Deductions"+space(31) COLOR W/G+
	ENDIF


ENDCASE



RETURN
******************




***************************
PROCEDURE chkadv   && Brows field advance Valid clause
***************************

DO CASE

CASE advance > instded.inst
	?? chr(7)
	WAIT WINDOW "Invalid Amount"
	REPLACE advance WITH instded.inst
CASE advance = instded.inst
	REPLACE advance WITH instded.inst
ENDCASE


RETURN .T.
***************************

***************************
PROCEDURE ChkMidAdv
***************************


GrossPay = salary.basic+salary.hrent+salary.convy+salary.medical+;
		   salary.cma+salary.pma+salary.other+salary.special+;
		   (salary.dwrate*saldtail.days)+(salary.otrated*saldtail.othrs)+;
		   salary.dwratem

DStrength = GrossPay - (advance+pfund+itax+other)

IF midadv >  DStrength
	?? chr(7)
	WAIT WINDOW "You can only Deduct amount upto Rs."+ALLTRIM(STR(DStrength))
	REPLACE midadv WITH 0
	RETURN .F.
ENDIF

RETURN .T.


*****************************************************
PROCEDURE ChkOther && Valid Clause for Other in Brows
*****************************************************

GrossPay = salary.basic+salary.hrent+salary.convy+salary.medical+;
		   salary.cma+salary.pma+salary.other+salary.special+;
		   (salary.dwrate*saldtail.days)+(salary.otrated*saldtail.othrs)+;
		   salary.dwratem

DStrength = GrossPay - (advance+pfund+itax+midadv)

IF other >  DStrength
	
	?? chr(7)
	WAIT WINDOW "You can only Deduct amount upto Rs."+ALLTRIM(STR(DStrength))
	RETURN .F.

ENDIF

RETURN
**********************************************************



***************************
PROCEDURE EndProg
***************************
CLEAR
CLEAR WINDOWS
CLOSE DATABASES
SHOW POPUP mainmenu
RESTORE SCREEN FROM backgrnd
RETURN
***************************