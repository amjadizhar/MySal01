* PROGRAM: EMDT1.PRG
* Print taken on :  05-06-96
* This revision : 17-Apr-2017
* End Month Data Transfer one
* Transfering Days,overtime totals to saldtail.dbf
* Entering and Changing Deductions,Inst.Advances,Mid.Advances.
********************************


DO mainprog


*******************
PROCEDURE mainprog
********************

DO iniset
DO IniSet2
DO UpdDays
DO UpdOthrs
DO UpdBonday
DO DedNewDat


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

SET EXCLUSIVE OFF
SET TALK OFF
SET SAFETY OFF

RETURN
***************************


*********************
PROCEDURE IniSet2
*********************

PUBLIC mMcode,mYear
mMcode = 0
mYear = 0


USE attend
GOTO TOP
STORE mcode TO mMcode
STORE year TO mYear
USE


SELECT STR(deptcode,2,0)+" -"+STR(empno,3,0) AS "CodAdv",;
	   sum(advance) AS "TAdvDed",;
	   deptcode,;
	   empno,;
	   mcode,;
	   year;
FROM deduct;
WHERE advance => 0;
ORDER BY CodAdv;
GROUP BY CodAdv;
INTO DBF advded


SELECT STR(deptcode,2,0)+" -"+STR(empno,3,0) AS "CodCmnth",;
	   sum(advance) AS "CmDed",;
	   deptcode,;
	   empno,;
	   mcode,;
	   year;
FROM deduct;
WHERE advance => 0 AND mcode = mmcode AND year = myear;
ORDER BY CodCmnth;
GROUP BY CodCmnth;
INTO DBF adcmnth


SELECT advded
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno

SELECT adcmnth
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno

CLOSE DATABASES
RETURN
***************************


***************************
PROCEDURE UpdDays
***************************

CLEAR
CLOSE DATABASES
SET EXCLUSIVE ON


IF _DOS
RESTORE SCREEN FROM Pwait1
ENDIF

SELECT 1
USE attend EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno


*PUBLIC mmcode,myear,LastSheet
*mmcode = 0
*myear = 0
*LastSheet = 0


STORE mcode TO mmcode
STORE year TO myear


SELECT 2
USE saldtail EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno


SELECT 3
USE salsheet EXCLUSIVE
INDEX ON sheetno TAG sheetno
SET ORDER TO sheetno

SELECT attend 
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE

DO CASE

CASE saldtail.mcode = 0
	SELECT salsheet

	LOCATE FOR mcode = mmcode
		
		IF NOT FOUND()
			GOTO BOTTOM
			STORE sheetno TO LastSheet
			APPEND BLANK
			REPLACE sheetno WITH LastSheet+1
			REPLACE mcode WITH mmcode
			REPLACE year WITH myear
			STORE sheetno TO LastSheet
		ENDIF
		
		IF FOUND()
				STORE sheetno TO LastSheet
		ENDIF
				
	SELECT saldtail
	APPEND FROM attend
	REPLACE ALL sheetno WITH LastSheet+1 FOR EMPTY(sheetno)
	

CASE saldtail.mcode <> 0

	SELECT salsheet
	GOTO BOTTOM
	STORE sheetno TO LastSheet

	SELECT saldtail
	DELETE FOR mcode = mmcode AND year = myear
	PACK
	APPEND FROM attend
	REPLACE ALL sheetno WITH LastSheet FOR EMPTY(sheetno)
	
ENDCASE

SELECT attend
REPLACE ALL saldtail.days WITH attend.Pbldays

SET EXCLUSIVE OFF

RETURN
*******************************************

***************************
PROCEDURE UpdOthrs
***************************

CLEAR
CLOSE DATABASES


IF _DOS
RESTORE SCREEN FROM Pwait1
ENDIF

SELECT 1
USE ovtime EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno

SELECT 2
USE saldtail EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno

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

PUBLIC mbonday,BdStr,mmcode,myear
mbonday = 0
BdStr = 0
mmcode = 0
myear = 0


SELECT 1
USE attend
STORE mcode TO mmcode
STORE year TO myear
USE

USE ysetting
SET FILTER TO year = myear
STORE bonday TO mbonday
USE

SELECT 1
USE month ORDER mcode
SEEK(mmcode)
BdStr = mdays - (frds+hdays)
USE

IF _DOS
RESTORE SCREEN FROM Pwait1
ENDIF

SELECT 1
USE saldtail ORDER empno

SELECT 2
USE salary EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno

SELECT 3
USE month ORDER mcode

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO salary ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE


SELECT saldtail
SET FILTER TO mcode = mmcode AND year = myear AND salary.wrdaily = .t.

REPLACE ALL bonday WITH mbonday;
		FOR days >= BdStr
		
RETURN
***************************



*******************************************
PROCEDURE DedNewDat
*******************************************

CLEAR
CLOSE DATABASES


IF _DOS
RESTORE SCREEN FROM Pwait1
ENDIF

SELECT 1
USE attend EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno
STORE mcode TO mmcode

SELECT 2
USE deduct EXCLUSIVE
INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
SET ORDER TO empno

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO deduct ADDITIVE



IF deduct.mcode = 0

	SELECT 2
	APPEND FROM attend
	REPLACE ALL sheetno WITH 3 FOR EMPTY(sheetno)
	SELECT 1        
	DO UpdAdv
	DO UpdPfund

ENDIF


RETURN
***************************


***************************
PROCEDURE UpdAdv   && If Data has been newly appended from Attend
***************************


CLEAR
CLOSE DATABASES

*RESTORE SCREEN FROM Pwait1

USE attend
STORE mcode TO mmcode
CLOSE DATABASES

SELECT 1
	USE instded EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno

SELECT 2
	USE deduct EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno
	SET FILTER TO 
	SET FILTER TO mcode = mmcode

SELECT 1
	SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO deduct ADDITIVE
	REPLACE ALL deduct.advance WITH Inst FOR NOT DELETED()

RETURN  
***************************


***************************
PROCEDURE UpdPfund
***************************

CLEAR
CLOSE DATABASES
*RESTORE SCREEN FROM Pwait1
USE attend
STORE mcode TO mmcode
STORE year TO myear

CLOSE DATABASES

SELECT 1
	USE deduct EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno
	SET FILTER TO 
	SET FILTER TO mcode = mmcode AND year = myear

SELECT 2
	USE salary EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno

SELECT 3
	USE employee EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno
	
SELECT 4
	USE saldtail EXCLUSIVE 
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno
	SET ORDER TO empno
	SET FILTER TO
	SET FILTER TO mcode = mmcode AND year = myear

SELECT 1
	SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO salary ADDITIVE
	SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO employee ADDITIVE
	SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE
	
	REPLACE ALL pfund WITH salary.basic*6/100 FOR NOT salary.probation;
		AND salary.mpay = .T.;
		AND salary.working = .T.;
		AND salary.salcode <> 0;
		AND saldtail.days > 5

RETURN
***************

***************************
PROCEDURE ConfDed
***************************
CLEAR
CLOSE DATABASES

IF _DOS
RESTORE SCREEN FROM Pwait1
ENDIF

USE attend
STORE mcode TO mmcode
STORE year TO myear

CLOSE DATABASES

CLEAR
CLOSE DATABASES

SELECT 1
USE deduct EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear
GOTO TOP

SELECT 2
USE employee EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 3
USE month EXCLUSIVE
INDEX ON mcode TAG mcode
SET ORDER TO mcode

SELECT 4
USE instded EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 5
USE salary EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
INDEX ON STR(deptcode)+STR(empno) TAG empno2
SET ORDER TO empno

SELECT 6
USE saldtail EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
INDEX ON STR(deptcode)+STR(empno) TAG empno2
SET ORDER TO empno
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear

SELECT 7
USE salcode EXCLUSIVE
INDEX ON salcode TAG salcode
SET ORDER TO salcode

SELECT 8
USE advded EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 9
USE adcmnth EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 10
USE advance EXCLUSIVE
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 5
SET RELATION TO salcode INTO salcode


SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO employee ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO instded ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO salary ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO advded ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO adcmnth ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO advance ADDITIVE

CLEAR

IF _DOS
DEFINE WINDOW full FROM 0,0 TO 24,79 DOUBLE
ACTIVATE WINDOW full
ENDIF


IF _DOS
DEFINE WINDOW below1 FROM 12,0 TO 23,78 IN WINDOW FULL
ENDIF

IF _WINDOWS
DEFINE WINDOW below1 FROM 16,3 TO 30,100;
	 COLOR RGB(255,255,0,0,0,255);
	 FONT "Arial",9
ENDIF

ACTIVATE WINDOW below1


IF _DOS
DEFINE WINDOW dedbrow FROM 1,1 TO 12,78
ENDIF

IF _WINDOWS
DEFINE WINDOW dedbrow FROM 1,1 TO 13,65 FLOAT GROW
ENDIF


ON KEY LABEL F2 DO RstPfnd

IF _DOS
BROWS FIELDS;
	Eno = RIGHT(str(deptcode),2)+"-"+;
		RIGHT(str(empno),3)+"�";
		:R :H = "Code",;
	Ename = employee.name :H = "Employee Name",;
	L1 = "�" :1 :R :H = "",;
	advance :V = chkadv() :H = "Inst.Adv" :P = "@Z",;
	L2 = "�" :1 :R :H = "",;
	midadv :V = ChkMidAdv() :F :H = "Mid.Adv" :P = "@Z",;
	L3 = "�" :1 :R :H = "",;
	pfund :H = "P.Fund" :P = "@Z",;
	L4 = "�" :1 :R :H = "",;
	other :V = ChkOther() :H = "Other" :P = "@Z",;
	L5 = "�" :1 :R :H = "",;
	itax :H = "I.Tax" :P = "@Z";
	WINDOW dedbrow;
	WHEN ShowSal();
	TITLE "Data Input for Deductions"+"  "+RTRIM(month.month)+;
		  ","+ALLTRIM(str(year))
	
ENDIF


IF _WINDOWS
BROWS FIELDS;
	Eno = RIGHT(str(deptcode),2)+"-"+;
		RIGHT(str(empno),3) :R :H = "Code",;
	Ename = employee.name :H = "Employee Name",;
	advance :V = chkadv() :H = "Inst.Adv" :P = "@Z",;
	midadv :V = ChkMidAdv() :F :H = "Mid.Adv" :P = "@Z",;
	pfund :H = "P.Fund" :P = "@Z",;
	other :V = ChkOther() :H = "Other" :P = "@Z",;
	itax :H = "I.Tax" :P = "@Z";
	WINDOW dedbrow;
	PREF brded1;
	WHEN ShowSal();
	TITLE "Data Input for Deductions"+"  "+RTRIM(month.month)+;
		  ","+ALLTRIM(str(year));
	COLOR RGB(0,0,0,255,255,0);
	FONT "Arial",10
	
ENDIF



ON KEY LABEL F2

CLEAR
CLEAR WINDOWS
CLOSE DATABASES

DO CalcMnWs

RETURN
***************************



********************
PROCEDURE CalcMnWs
********************

PUBLIC GrossPay,OtAmt,NetPay
GrossPay = 0
OtAmt = 0
NetPay = 0

CLOSE DATA

SELECT 1
USE salary

SELECT 2
USE saldtail ORDER empno

SELECT 3
USE deduct ORDER empno

SELECT 4
USE month ORDER mcode


SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year);
	INTO saldtail ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year);
	INTO deduct ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
	



SCAN

IF salary.mcode <= 12 AND year <= 1995
DO CASE

CASE salary.mpay AND salary.dwratem = 0

GrossPay = salary.basic+;
		   salary.hrent+;
		   salary.convy+;
		   salary.medical+;
		   salary.cma+;
		   salary.pma+;
		   salary.other+;
		   salary.special
		   
OtAmt = ROUND(salary.otrated*saldtail.othrs,0)

NetPay = GrossPay - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)+OtAmt


CASE NOT salary.mpay AND salary.dwrate <> 0

GrossPay = ROUND(salary.dwrate*saldtail.days,0);
		   +(saldtail.bonday * salary.dwrate)

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)

NetPay = GrossPay - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)+OtAmt


CASE NOT salary.mpay AND salary.dwratem <> 0

GrossPay = salary.dwratem

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)		   

NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)


ENDCASE

REPLACE Tot_Pay WITH GrossPay
REPLACE Ot_Amt WITH OtAmt
REPLACE Net_Pay WITH NetPay


ENDIF




IF salary.mcode <= 3 AND year <= 1996
DO CASE

CASE salary.mpay AND salary.dwratem = 0

GrossPay = salary.basic+;
		   salary.hrent+;
		   salary.convy+;
		   salary.medical+;
		   salary.cma+;
		   salary.pma+;
		   salary.other+;
		   salary.special
		   
OtAmt = ROUND(salary.otrated*saldtail.othrs,0)


NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)



CASE NOT salary.mpay AND salary.dwrate <> 0

GrossPay = ROUND(salary.dwrate*saldtail.days,0);
		   +(saldtail.bonday * salary.dwrate)

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)


NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)


CASE NOT salary.mpay AND salary.dwratem <> 0

GrossPay = salary.dwratem

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)		   

NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)


ENDCASE

REPLACE Tot_Pay WITH GrossPay
REPLACE Ot_Amt WITH OtAmt
REPLACE Net_Pay WITH NetPay

ENDIF





IF salary.mcode >= 4 AND year >= 1996
DO CASE

CASE salary.mpay AND salary.dwratem = 0

GrossPay = ROUND(salary.basic / month.mdays * saldtail.days,0)+;
		   ROUND(salary.hrent / month.mdays * saldtail.days,0)+;
		   ROUND(salary.convy / month.mdays * saldtail.days,0)+;
		   ROUND(salary.medical / month.mdays * saldtail.days,0)+;
		   ROUND(salary.cma / month.mdays * saldtail.days,0)+;
		   ROUND(salary.pma / month.mdays * saldtail.days,0)+;
		   ROUND(salary.other / month.mdays * saldtail.days,0)+;
		   ROUND(salary.special / month.mdays * saldtail.days,0)

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)


NetPay = GrossPay - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)+OtAmt


CASE NOT salary.mpay AND salary.dwrate <> 0

GrossPay = ROUND(salary.dwrate*saldtail.days,0)+;
		   (saldtail.bonday * salary.dwrate)

OtAmt = ROUND(salary.otrated*saldtail.othrs,0)

NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)


CASE NOT salary.mpay AND salary.dwratem <> 0

GrossPay = ROUND(salary.dwratem / month.mdays * saldtail.days,0)
		   
OtAmt = ROUND(salary.otrated*saldtail.othrs,0)

NetPay = (GrossPay+OtAmt) - (deduct.advance+deduct.midadv+deduct.other+;
					deduct.pfund+deduct.itax)


ENDCASE

REPLACE Tot_Pay WITH GrossPay
REPLACE Ot_Amt WITH OtAmt
REPLACE Net_Pay WITH NetPay

ENDIF

ENDSCAN


RETURN
****************







*****************
PROCEDURE RstPfnd
*****************

	REPLACE ALL pfund WITH salary.basic/100*6 FOR NOT salary.probation;
		AND salary.mpay = .T.;
		AND salary.working = .T.;
		AND salary.salcode <> 0;
		AND saldtail.days > 5

RETURN
***************


******************
PROCEDURE ShowSal
******************


DO CASE

CASE salary.mpay

GrossPay = ROUND(salary.basic / month.mdays * saldtail.days,0)+;
		   ROUND(salary.hrent / month.mdays * saldtail.days,0)+;
		   ROUND(salary.convy / month.mdays * saldtail.days,0)+;
		   ROUND(salary.medical / month.mdays * saldtail.days,0)+;
		   ROUND(salary.cma / month.mdays * saldtail.days,0)+;
		   ROUND(salary.pma / month.mdays * saldtail.days,0)+;
		   ROUND(salary.other / month.mdays * saldtail.days,0)+;
		   ROUND(salary.special / month.mdays * saldtail.days,0)+;
		   ROUND(salary.dwratem / month.mdays * saldtail.days,0)

CASE NOT salary.mpay AND salary.dwrate <> 0

GrossPay = ROUND(salary.dwrate*saldtail.days,0)+ROUND(salary.otrated*saldtail.othrs,0)+;
		   salary.dwratem+(saldtail.bonday * salary.dwrate)


CASE NOT salary.mpay AND salary.dwratem <> 0

GrossPay = ROUND(salary.dwratem / month.mdays * saldtail.days,0)+ROUND(salary.otrated*saldtail.othrs,0)
		   

ENDCASE


IF _DOS
@ 1,0 TO 8,76 CLEAR
ENDIF

IF _DOS
@ 1,0 FILL TO 8,76 COLOR GR+/RB
ENDIF


DO CASE

CASE salary.mpay AND salary.working AND salary.salcode = 0

	IF _DOS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name+SPACE(33);
		COLOR W+/R
	ENDIF


	IF _WINDOWS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name
		
	ENDIF


	IF _DOS
		@ 1,1 SAY "The employee is on Monthly Pay";
		COLOR GR+/RB
	ENDIF

	IF _WINDOWS
		@ 1,1 SAY "The employee is on Monthly Pay"
		
	ENDIF



	IF _DOS
		@ 2,1 SAY "The employee is Working";
		COLOR GR+/RB
		@ 3,1 SAY "No Salary Sheet Code allocated";
		COLOR GR+/RB
	ENDIF
	

	IF _WINDOWS
		@ 2,1 SAY "The employee is Working"
		
		@ 3,1 SAY "No Salary Sheet Code allocated"
		
	ENDIF

	

CASE salary.mpay AND NOT salary.working AND salary.salcode = 0

	IF _DOS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name
		
	ENDIF

	IF _WINDOWS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name
		
	ENDIF


	IF _DOS
		@ 1,1 SAY "The employee is on Monthly Pay";
		COLOR GR+/RB
	ENDIF


	IF _WINDOWS
		@ 1,1 SAY "The employee is on Monthly Pay"
		
	ENDIF


	IF _DOS
		@ 2,1 SAY "The employee is Not Working";
		COLOR GR+/RB
	ENDIF
	

	IF _WINDOWS
		@ 2,1 SAY "The employee is Not Working"
		
	ENDIF



	IF _DOS
		@ 3,1 SAY "No Salary Sheet Code allocated";
		COLOR GR+/RB
	ENDIF

	IF _WINDOWS
		@ 3,1 SAY "No Salary Sheet Code allocated"
		
	ENDIF



CASE salary.mpay AND NOT salary.working AND salary.salcode <> 0



	IF _DOS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name+SPACE(33);
		COLOR W+/R
	ENDIF

	IF _WINDOWS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name
		
	ENDIF

	IF _DOS
		@ 1,1 SAY "The employee is on Monthly Pay";
		COLOR GR+/RB
	ENDIF
	
	IF _WINDOWS
		@ 1,1 SAY "The employee is on Monthly Pay"
		
	ENDIF

	IF _DOS
		@ 2,1 SAY "The employee is Not Working";
		COLOR GR+/RB
	ENDIF
	
	IF _WINDOWS
		@ 2,1 SAY "The employee is Not Working"
		
	ENDIF

	IF _DOS
		@ 3,1 SAY "Salary Sheet Code allocated:  "+;
		ALLTRIM(STR(salcode.salcode))+"-"+salcode.descrip;
		COLOR GR+/RB
	ENDIF
	
	IF _WINDOWS
		@ 3,1 SAY "Salary Sheet Code allocated:  "+;
		ALLTRIM(STR(salcode.salcode))+"-"+salcode.descrip
		
	ENDIF



CASE salary.mpay AND salary.working AND salary.salcode <> 0

	IF _DOS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name+" "+;
		salcode.descrip+SPACE(6) COLOR W+/R
	ENDIF
	
	IF _WINDOWS
		@ 0,0 SAY " Salary Calculation: "+employee.salutation+;
		"."+employee.name+" "+;
		salcode.descrip+SPACE(42);
		COLOR RGB(255,255,0,255,0,0)
	ENDIF


	
	IF _DOS
		IF salary.basic <> 0
		mCalcBasic = ROUND((salary.basic / month.mdays) * saldtail.days,0) 
		@ 1,1 SAY "Basic:" COLOR GR+/RB
		@ 1,8 SAY mCalcBasic;
          PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.name WITH employee.name
		REPLACE saldtail.basic WITH mCalcBasic
		ENDIF
	ENDIF


	IF _DOS
		IF salary.hrent <> 0
		mCalcHrent = ROUND((salary.hrent / month.mdays) * saldtail.days,0) 
		@ 2,1 SAY "H.R. :" COLOR GR+/RB
		@ 2,8 SAY mCalcHrent;
		  PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.hrent WITH mCalcHrent
		ENDIF
	ENDIF



	IF _DOS
		IF salary.convy <> 0
		mCalcConvy = ROUND((salary.convy / month.mdays) * saldtail.days,0)
		@ 3,1 SAY "Conv:" COLOR GR+/RB
		@ 3,8 SAY mCalcConvy;
          PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.convy WITH mCalcConvy
		ENDIF

	ENDIF

	IF _DOS
		IF salary.medical <> 0
		mCalcMedical = ROUND((salary.medical / month.mdays) * saldtail.days,0)
		@ 4,1 SAY "Med." COLOR GR+/RB
		@ 4,8 SAY mCalcMedical;
		           PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.medical WITH mCalcMedical
		ENDIF
	ENDIF



	IF _DOS
		IF salary.cma <> 0
		mCalcCma = ROUND((salary.cma / month.mdays) * saldtail.days,0)
		@ 1,20 SAY "Cma" COLOR GR+/RB
		@ 1,28 SAY mCalcCma;
		   PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.cma WITH mCalcCma
		ENDIF
	ENDIF



	IF _DOS
		IF salary.pma <> 0
		mCalcPma = ROUND((salary.pma / month.mdays) * saldtail.days,0)
		@ 2,20 SAY "Pma" COLOR GR+/RB
		@ 2,28 SAY mCalcPma;
		   PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.pma WITH mCalcPma
		ENDIF
	ENDIF



	IF _DOS
		IF salary.other <> 0
		mCalcOther = ROUND((salary.other / month.mdays) * saldtail.days,0)
		@ 3,20 SAY "Other:" COLOR GR+/RB
		@ 3,28 SAY mCalcOther;
  		   PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.other WITH mCalcOther
		ENDIF
	ENDIF



	IF _DOS
		IF salary.special <> 0
		mCalcSpecial = ROUND((salary.special / month.mdays) * saldtail.days,0)
		@ 4,20 SAY "Spec:" COLOR GR+/RB
		@ 4,28 SAY salary.special;
		   PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.special WITH mCalcSpecial
		ENDIF
	ENDIF



	IF _DOS
		IF GrossPay <> 0
		@ 1,46 SAY "Gross" COLOR GR+/RB
		@ 2,44 SAY "=" COLOR GR+/RB
		@ 2,46 SAY GrossPay PICT "999,999" COLOR GR+/RB                          
		REPLACE saldtail.grosspay WITH GrossPay
		ENDIF
	ENDIF



	IF _DOS
		@ 1,55 SAY "Days: "+;
			       ALLTRIM(STR(saldtail.days));
			       COLOR GR+/RB

		mCalcNetPay = GrossPay - (advance+midadv+other+pfund+itax)
		@ 2,55 SAY "Net Pay: " COLOR GR+/RB
		@ 2,64 SAY mCalcNetPay;
				   PICT "999,999";
				   COLOR GR+/RB
		REPLACE saldtail.advance WITH advance
		REPLACE saldtail.midadv WITH midadv
		REPLACE saldtail.dedother WITH other
		REPLACE saldtail.pfund WITH pfund
		REPLACE saldtail.itax WITH itax
		REPLACE saldtail.netpay WITH mCalcNetPay
	ENDIF


	*IF _DOS
	*	IF advded.deptcode <> 0
	*		@ 3,55 SAY "Adv.Bal: "
	*		
	*		@ 3,64 SAY advance.advamt - advded.tadvded;
	*			   PICT "999,999";
	*			   COLOR GR+/RB
	*	ENDIF
	*
	*ENDIF




	*IF _DOS
	*	IF saldtail.othrs <> 0 
	*		@ 3,1 SAY " Over Time Calculation"+space(20) COLOR W/G+
	*	ENDIF
	*ENDIF
	

	IF _DOS
		IF saldtail.othrs <> 0 
		@ 5,1 SAY "OT Hrs:" COLOR GR+/RB
		@ 5,8 SAY saldtail.othrs PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF
	

	IF _DOS
		IF salary.otrated <> 0 .AND. saldtail.othrs <> 0
		@ 5,20 SAY "OT Rate" COLOR GR+/RB
		@ 5,28 SAY salary.otrated PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.otrated WITH salary.otrated
		ENDIF
	ENDIF

	IF _DOS
		IF saldtail.othrs <> 0 .AND. salary.otrated <> 0
		mCalcOtAmt = ROUND(salary.otrated * saldtail.othrs,0)
		@ 4,46 SAY "Tot.OT" COLOR GR+/RB
		@ 5,44 SAY "=" COLOR GR+/RB
		@ 5,46 SAY mCalcOtAmt;
			 PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.otamt WITH mCalcOtAmt
		ENDIF
	ENDIF



	IF _DOS
		IF (advance+midadv+other+pfund+itax) <> 0
			@ 6,1 SAY " Deductions"+space(31) COLOR W/G+
		ENDIF
	ENDIF

	IF _WINDOWS
		IF (advance+midadv+other+pfund+itax) <> 0
			@ 6,1 SAY " Deductions"
		ENDIF
	ENDIF


	IF _DOS
		IF advance <> 0
		@ 7,1 SAY "Inst.Adv" COLOR GR+/RB
		@ 8,1 SAY advance COLOR GR+/RB
		@ 8,8 SAY "+" COLOR GR+/RB
		ENDIF
	ENDIF

	IF _WINDOWS
		IF advance <> 0
		@ 7,1 SAY "Inst.Adv"
		@ 8,1 SAY advance
		@ 8,8 SAY "+"
		ENDIF
	ENDIF


	IF _DOS
		IF midadv <> 0
		@ 7,11 SAY "Mid.Adv" COLOR GR+/RB
		@ 8,12 SAY midadv COLOR GR+/RB
			IF other <> 0
			@ 8,18 SAY "+" COLOR GR+/RB
			ENDIF
		ENDIF
	ENDIF

	IF _WINDOWS
		IF midadv <> 0
		@ 7,11 SAY "Mid.Adv"
		@ 8,12 SAY midadv
			IF other <> 0
			@ 8,18 SAY "+"
			ENDIF
		ENDIF
	ENDIF



	IF _DOS
		IF pfund <> 0
		@ 7,20 SAY "P.Fund" COLOR GR+/RB
			IF midadv <> 0
			@ 8,18 SAY "+" COLOR GR+/RB
			ENDIF
		@ 8,21 SAY pfund COLOR GR+/RB
		ENDIF
	ENDIF

	IF _WINDOWS
		IF pfund <> 0
		@ 7,20 SAY "P.Fund"
			IF midadv <> 0
			@ 8,18 SAY "+"
			ENDIF
		@ 8,21 SAY pfund
		ENDIF
	ENDIF


	IF _DOS
		IF other <> 0
		@ 7,27 SAY "Other" COLOR GR+/RB
			IF pfund <> 0
			@ 8,25 SAY "+" COLOR GR+/RB
			ENDIF
		@ 8,26 SAY other COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF itax <> 0
		@ 7,34 SAY "I.Tax" COLOR GR+/RB
		@ 8,32 SAY "+" COLOR GR+/RB
		@ 8,35 SAY itax COLOR GR+/RB
		ENDIF
	ENDIF




	IF _DOS
		IF (advance+midadv+other+pfund+itax) <> 0
		mCalcTotDed = (advance+midadv+other+pfund+itax)
		@ 7,46 SAY "Tot.Ded" COLOR GR+/RB
		@ 8,44 SAY "=" COLOR GR+/RB
		@ 8,46 SAY mCalcTotDed;
			PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF GrossPay <> 0
			@ 6,55 SAY " Net Payable Amount " COLOR W+/G
			@ 7,55 SAY " Rs." COLOR N/BG
			mCalcTotDed = advance+midadv+other+pfund+itax
			mCalcOtAmt = ROUND(salary.otrated * saldtail.othrs,0)
			mCalcNetPayable = GrossPay + mCalcOtAmt - mCalcTotDed
			@ 7,59 SAY mCalcNetPayable PICT "9999,999" COLOR N/BG
			@ 7,67 SAY SPACE(8) COLOR N/BG
			REPLACE saldtail.totded WITH mCalcTotDed
			REPLACE saldtail.payable WITH mCalcNetPayable
		ENDIF
	ENDIF

CASE NOT salary.mpay AND NOT salary.working AND salary.salcode = 0


	IF _DOS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name+SPACE(33);
		COLOR W/GR+
		@ 1,1 SAY "The employee is on Daily Wages";
		COLOR GR+/RB
		@ 2,1 SAY "The employee is not Working";
		COLOR GR+/RB
		@ 3,1 SAY "No Salary Sheet Code allocated";
		COLOR GR+/RB
	ENDIF

	IF _WINDOWS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name
		
		@ 1,1 SAY "The employee is on Daily Wages"
		
		@ 2,1 SAY "The employee is not Working"
		
		@ 3,1 SAY "No Salary Sheet Code allocated"
		
	ENDIF


CASE NOT salary.mpay AND salary.working AND salary.salcode = 0

	IF _DOS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name+SPACE(33);
		COLOR W/GR+
		@ 1,1 SAY "The employee is on Daily Wages";
		COLOR GR+/RB
		@ 2,1 SAY "The employee is Working";
		COLOR GR+/RB
		@ 3,1 SAY "No Salary Sheet Code allocated";
		COLOR GR+/RB
	ENDIF

	IF _WINDOWS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name
		
		@ 1,1 SAY "The employee is on Daily Wages"
		
		@ 2,1 SAY "The employee is Working"
		
		@ 3,1 SAY "No Salary Sheet Code allocated"
		
	ENDIF


CASE NOT salary.mpay AND NOT salary.working AND salary.salcode <> 0

	IF _DOS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name+SPACE(33);
		COLOR W/GR+
		@ 1,1 SAY "The employee is on Daily Wages";
		COLOR GR+/RB
		@ 2,1 SAY "The employee is Not Working";
		COLOR GR+/RB
		@ 3,1 SAY "Salary Sheet Code allocated:  "+;
		ALLTRIM(STR(salcode.salcode))+" "+salcode.descrip;
		COLOR GR+/RB
	ENDIF

	IF _WINDOWS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name
		
		@ 1,1 SAY "The employee is on Daily Wages"
		
		@ 2,1 SAY "The employee is Not Working"
		
		@ 3,1 SAY "Salary Sheet Code allocated:  "+;
		ALLTRIM(STR(salcode.salcode))+" "+salcode.descrip
		
	ENDIF



CASE NOT salary.mpay AND salary.working AND salary.salcode <> 0

	IF _DOS
		@ 0,0 SAY " Wages Calculation: "+employee.salutation+;
		"."+employee.name+" "+salcode.descrip+space(7) COLOR W/GR+
	ENDIF



	IF _DOS
		IF saldtail.days <> 0
		@ 1,1 SAY "Days:" COLOR GR+/RB
		@ 1,8 SAY saldtail.days PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF salary.dwrate <> 0 .AND. saldtail.days <> 0
		@ 2,1 SAY "Rate:" COLOR GR+/RB
		@ 2,8 SAY salary.dwrate PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF salary.dwrate = 0 .AND. saldtail.days <> 0;
				 AND salary.dwratem <> 0
		@ 1,8 SAY "Wages" COLOR GR+/RB
		@ 2,6 SAY "  " COLOR GR+/RB
		@ 2,8 SAY ROUND(salary.dwratem / month.mdays * saldtail.days,0);
				  PICT "99999" COLOR GR+/RB
		@ 3,2 SAY "Fixed Monthly Lump Sum Wages Rate: ";
			 +ALLTRIM(STR(salary.dwratem)) COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF saldtail.days <> 0 .AND. salary.dwrate <> 0
		mCalcWages = (salary.dwrate * saldtail.days)
		@ 3,1 SAY "Wages:" COLOR GR+/RB
		@ 3,8 SAY mCalcWages PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.wagerate WITH salary.dwrate
		REPLACE saldtail.name WITH employee.name
		REPLACE saldtail.wagesamt WITH mCalcWages
		REPLACE saldtail.otrated WITH salary.dwrate
		ENDIF
	ENDIF




	IF _DOS
		IF saldtail.othrs <> 0 
		@ 1,20 SAY "OT Hrs:" COLOR GR+/RB
		@ 1,28 SAY saldtail.othrs PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF


	IF _DOS
		IF salary.otrated <> 0 .AND. saldtail.othrs <> 0
		@ 2,20 SAY "OT Rate" COLOR GR+/RB
		@ 2,28 SAY salary.otrated PICT "999,999" COLOR GR+/RB
		ENDIF
	ENDIF



	IF _DOS
		IF saldtail.othrs <> 0 .AND. salary.otrated <> 0
		mCalcOTAmt = ROUND(salary.otrated * saldtail.othrs,0)
		@ 3,20 SAY "OT Amt:" COLOR GR+/RB
		@ 3,28 SAY mCalcOTAmt PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.otamt WITH mCalcOTAmt
		ENDIF
	ENDIF



	IF _DOS
		
		IF saldtail.bonday = 0
		 	@ 1,66 SAY "Gross" COLOR GR+/RB
		 	@ 2,66 SAY "=" COLOR GR+/RB
		 	@ 2,68 SAY GrossPay PICT "999,999" COLOR GR+/RB
		ELSE
		mCalcBonus = (saldtail.bonday * salary.dwrate)
		@ 1,40 SAY "Bonus:" COLOR GR+/RB
		@ 1,48 SAY mCalcBonus PICT "999,999" COLOR GR+/RB
		REPLACE saldtail.bonus WITH mCalcBonus
		@ 1,66 SAY "Gross" COLOR GR+/RB
		@ 2,66 SAY "=" COLOR GR+/RB
		@ 2,68 SAY GrossPay PICT "999,999" COLOR GR+/RB
		
		ENDIF
	
	ENDIF


	IF _DOS
		IF (advance+midadv+other+pfund+itax) <> 0
			@ 6,1 SAY " Deductions"+space(31) COLOR W/G+
		ENDIF
	ENDIF



	IF _DOS
		IF advance <> 0
		@ 7,1 SAY "Inst.Adv" COLOR GR+/RB
		@ 8,1 SAY advance COLOR GR+/RB
		@ 8,8 SAY "+" COLOR GR+/RB
		REPLACE saldtail.advance WITH advance
		ENDIF
	ENDIF



	IF _DOS
		IF midadv <> 0
		@ 7,11 SAY "Mid.Adv" COLOR GR+/RB
		@ 8,12 SAY midadv COLOR GR+/RB
		REPLACE saldtail.midadv WITH midadv
			IF other <> 0
			@ 8,18 SAY "+" COLOR GR+/RB
			ENDIF
		ENDIF
	ENDIF



	IF _DOS
		IF pfund <> 0
		@ 7,20 SAY "P.Fund" COLOR GR+/RB
			IF midadv <> 0
			@ 8,18 SAY "+" COLOR GR+/RB
			ENDIF
		@ 8,21 SAY pfund COLOR GR+/RB
		REPLACE saldtail.pfund WITH pfund
		ENDIF
	ENDIF




	IF _DOS
		IF other <> 0
		@ 7,27 SAY "Other" COLOR GR+/RB
			IF pfund <> 0
			@ 8,25 SAY "+" COLOR GR+/RB
			ENDIF
		@ 8,26 SAY other COLOR GR+/RB
		REPLACE saldtail.dedother WITH other
		ENDIF
	ENDIF



	IF _DOS
		IF itax <> 0
		@ 7,34 SAY "I.Tax" COLOR GR+/RB
		@ 8,32 SAY "+" COLOR GR+/RB
		@ 8,35 SAY itax COLOR GR+/RB
		REPLACE saldtail.itax WITH itax
		ENDIF
	ENDIF



	IF _DOS
		IF (advance+midadv+other+pfund+itax) <> 0
		mCalcTotDed = (advance+midadv+other+pfund+itax)
		@ 7,46 SAY "Tot.Ded" COLOR GR+/RB
		@ 8,44 SAY "=" COLOR GR+/RB
		@ 8,46 SAY mCalcTotDed;
			PICT "99,999" COLOR GR+/RB
		REPLACE saldtail.totded WITH mCalcTotDed
		ENDIF
	ENDIF

* Editing on 17-04-2017

	IF _DOS
		IF GrossPay <> 0
			mCalcNetPayable = GrossPay-(advance+midadv+other+pfund+itax)
			@ 6,55 SAY " Net Payable Amount " COLOR W+/G
			@ 7,55 SAY " Rs." COLOR N/BG
			@ 7,59 SAY mCalcNetPayable PICT "999,999" COLOR N/BG
			@ 7,66 SAY "         " COLOR N/BG
			REPLACE saldtail.netpay WITH mCalcNetPayable
			REPLACE saldtail.payable WITH mCalcNetPayable
		ENDIF
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

IF salary.mpay = .T.

GrossPay = salary.basic+salary.hrent+salary.convy+salary.medical+;
		   salary.cma+salary.pma+salary.other+salary.special
ENDIF

IF salary.mpay = .F. AND salary.wrdaily = .T. ;
   AND NOT EMPTY(salary.dwrate)

GrossPay = (salary.dwrate * saldtail.days) +;
		   (salary.otrated * saldtail.othrs)+;
	       (saldtail.bonday * salary.dwrate)

ENDIF

IF salary.mpay = .F. AND salary.wrdaily = .F. AND EMPTY(salary.dwrate) ;
	AND NOT EMPTY(salary.dwratem)
   
GrossPay = salary.dwratem

ENDIF




DStrength = GrossPay - (advance+pfund+itax+other)

IF midadv >  DStrength
	?? chr(7)
	WAIT WINDOW "You can only Deduct amount upto Rs."+ALLTRIM(STR(DStrength))
	REPLACE midadv WITH 0
	RETURN .F.

ELSE
RETURN .T.
ENDIF

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

IF _DOS
RESTORE SCREEN FROM backgrnd
ENDIF

RETURN
***************************
