CLOSE DATA

CLEA
SET TALK OFF
SET SAFETY OFF

SELECT 1
USE attend ORDER empno

SELECT 2
USE employee ORDER empno

SELECT 3
USE ovtime ORDER empno3

SELECT 4
USE artime ORDER empno2

SELECT 5
USE deptt ORDER deptcode

SELECT 6
USE month ORDER mcode


SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO str(deptcode)+str(atord)+str(empno) INTO ovtime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO artime ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE


CLEAR

PUBLIC Vp
Vp = space(1)

@ 11,19 SAY "Report to Printer or View (P/V) ? "
@ ROW(),COL() GET Vp PICT "@!" VALID INLIST(Vp,'P','V');
	   ERROR "(P)rinter,(V)iew "
READ

DO CASE

	CASE Vp = "P"

		??? CHR(15)
		??? CHR(15)
		
		REPORT FORM ATsheet.FRX TO PRINTER NOEJECT

	CASE Vp = "V"

		REPORT FORM ATsheet.FRX PREVIEW


ENDCASE

CLEAR
CLOSE DATA