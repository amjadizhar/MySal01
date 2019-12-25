*******************************
* PROGRAM: FRIDAY.PRG
* For Calculation of Fidays in a month
*******************************

SET TALK OFF

DO MnthFrds
DO PutF && Put F in all attend.dbf Fridays
DO PutH && Put H in all attend.dbf Holidays

*********************************
PROCEDURE MnthFrds
*********************************
PUBLIC CurMnth,CurYr,Holidays,Fridays
CurMnth = 0
CurYr = 0
Holidays = 0
Fridays = 0

CLOSE DATABASES

SELECT 1

USE attend ORDER Ymcode
STORE mcode TO CurMnth
STORE year TO CurYr

SELECT 2
USE month ORDER mcode

SELECT 3
USE holiday ORDER mcode


SELECT 1
SET RELATION TO mcode INTO month



IF DAY(Givenday(attend.mcode,attend.year,6,5)) = 1
	Fridays = 4
ELSE
	Fridays = 5
ENDIF

REPLACE month.Frds WITH Fridays

SELECT 3
COUNT FOR mcode = CurMnth TO Holidays

SELECT 1
REPLACE month.Hdays WITH Holidays

CLOSE DATABASES

RETURN
*********************


****************************************
PROCEDURE PutF
****************************************
PUBLIC mHdays

SELECT 1
USE attend ORDER Ymcode
STORE mcode TO CurMnth
STORE year TO CurYr

SELECT 2
USE month ORDER mcode
SET FILTER TO mcode = CurMnth

SELECT 3
USE holiday ORDER mcode
SET FILTER TO mcode = CurMnth
COUNT TO mHdays FOR mcode = CurMnth


SELECT 1
SET RELATION TO mcode INTO month

clear
DIMENSION Fri(Fridays)
DIMENSION FriFld(Fridays)

FOR i = 1 TO Fridays

STORE DAY(Givenday(attend.mcode,attend.year,6,i)) TO Fri(i)


STORE "D"+ALLTRIM(STR(Fri(i))) TO FriFld(i)

ENDFOR

FOR n = 1 TO Fridays

REPLACE ALL &FriFld(n) WITH "F"


ENDFOR

RETURN
******************************************************

*******************************************
PROCEDURE PutH
*******************************************

IF mHdays <> 0
DIMENSION HdFld(mHdays)


SELECT 3
GOTO TOP 


FOR h = 1 TO mHdays

STORE "D"+ALLTRIM(STR(DAY(date))) TO HdFld(h)

IF .NOT. EOF()
SKIP
ENDIF

ENDFOR

SELECT 1

FOR h = 1 TO mHdays

REPLACE ALL &HdFld(h) WITH "H"

ENDFOR

ENDIF

RETURN
*******************************************




*** Construct a date from month,day and year

FUNCTION Makedate
PARAMETERS mmonth,mday,myear
mcmonth = PADL(LTRIM(STR(mmonth,2)),2,"0")  && Convert month to string
mcday = PADL(LTRIM(STR(mday,2)),2,"0")		&& Convert day to string
IF LEN(LTRIM(STR(myear,4))) =4				&& Convert year to string
	mcyear = STR(myear,4)
ELSE
	mcyear = PADL(LTRIM(STR(myear,2)),2,"0")

ENDIF

RETURN CTOD(mcmonth + "/" + mcday + "/" + mcyear)

***** End of function Makedate ****


FUNCTION Givenday
PARAMETERS mmonth,myear,mday,moccur
mdate = Makedate(mmonth,1,myear)	&& First Day of month
mdaysfound = 0						&& Occurence counter
DO WHILE mdaysfound <moccur AND MONTH(mdate) = mmonth
	IF DOW(mdate) = mday			&& If correct day of week.
		mdaysfound = mdaysfound+1	&& Increment occurrence counter
	ENDIF
	IF mdaysfound = moccur
		EXIT
	ELSE
		mdate = mdate+1
	ENDIF
ENDDO
RETURN mdate
	
	