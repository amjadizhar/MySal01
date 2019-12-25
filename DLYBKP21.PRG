* PROGRAM: DLYATND.PRG
* Program to feed daily attendence

DO mainprog


***********************************************************************
PROCEDURE mainprog  && The main routine to run this program

***********************************************************************


DO iniset										&& Initial Settings
DO defwinds										&& Define Windows
DO getdate					&& Get Date to which entering Attendence
DO paintbrow				&& Paint the Brows Window for getting data
DO endprog					&& End the Program and clear things


RETURN
*************************************************************************
*	End of PROCEDURE  mainprog  The main routine to run this program
*************************************************************************





***********************************************************************
PROCEDURE iniset							&& Initial Settings

***********************************************************************

CLEAR

PUBLIC top0
CLEAR MACROS
ON KEY LABEL F1 ?? CHR(7)
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
ON KEY LABEL F10 ?? CHR(7)
SET FUNCTION F11 TO
SET FUNCTION F12 TO


SET SYSMENU OFF
SET TALK OFF
SET SAFETY OFF

SELECT 1
USE attend
SET ORDER TO TAG empno OF attend.cdx

SELECT 2
USE employee
SET ORDER TO TAG empno OF employee.cdx

SELECT 3
USE deptt
SET ORDER TO TAG deptcode OF deptt.cdx

SELECT 4
USE artime
SET ORDER TO TAG empno OF artime.cdx

SELECT 5
USE ovtime
SET ORDER TO TAG empno OF ovtime.cdx

SELECT 6
USE month
SET ORDER TO TAG mcode OF month.cdx

SELECT 7
USE salary
SET ORDER TO TAG empno OF salary.cdx

SELECT 8
USE dptime
SET ORDER TO TAG empno OF dptime

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO artime ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO ovtime ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO dptime ADDITIVE

@ 0,0 SAY "Today's Date: "
@ 0,14 SAY CDOW(DATE())
@ 0,25 SAY DATE() PICT "@E"
SAVE SCREEN TO top0

RETURN
*************************************************************************
*					End of PROCEDURE iniset  Initial Settings			*
*************************************************************************




***********************************************************************
PROCEDURE defwinds								&& Define Windows

***********************************************************************

DEFINE WINDOW bratnd FROM 2,1 TO 13,37 COLOR N/BG
DEFINE WINDOW full FROM 1,0 TO 24,79 DOUBLE
DEFINE WINDOW getdate FROM 0,0 TO 24,79 DOUBLE


RETURN

*************************************************************************
*					End of PROCEDURE defwinds  Define Windows   		*
*************************************************************************








***********************************************************************
PROCEDURE getdate		&& Get Date to which entering Attendence

***********************************************************************
PUBLIC mdate,Brf1,totemp,dp,da,dl,de,dt,Pal,tovtime


mdate = {  /  /  }				&& Setting Variable to get input from
								&& User

ACTIVATE WINDOW getdate


ON KEY LABEL ESCAPE ?? CHR(7)

@ 2,8 SAY "Daily Attendence Register"
@ 5,8 SAY "To which date you want to "
@ 6,8 SAY "enter Attendence         :"
@ 6,35 GET mdate PICT "@E" VALID chkmnth()	&& Get date and also check
READ										&& if the month of this date

ON KEY LABEL ESCAPE							&& is in attendence data.



Brf1 = "D"+ALLTRIM(str(day(mdate)))			&& Making Browse Field for
											&& Brows data entry screen

lat = Brf1+"LT" 		&& Late and Arrival Time Field
otime = Brf1+"OT"		&& Over Time Field
dtime = Brf1+"DT"		&& Leaving Time field


DO WHILE .NOT. EOF()						&& Loop for Calculating
											&& Total Employeed working
											&& on this date.
DO CASE

CASE .NOT. EMPTY(employee.dole);
	.AND. employee.dole < mdate

	REPLACE attend.fbr WITH .F.							

CASE EMPTY(employee.dole);
	.AND. employee.doa > mdate

	REPLACE attend.fbr WITH .F.							



OTHERWISE													

	REPLACE attend.fbr WITH .T.							

ENDCASE

SKIP
ENDDO

COUNT TO totemp FOR fbr .AND. mcode = month(mdate) .and. year = year(mdate)

COUNT TO dp FOR fbr .AND. &brf1 = "P" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO da FOR fbr .AND. &brf1 = "A" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO dl FOR fbr .AND. &brf1 = "L" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO dt FOR fbr .AND. &brf1 = "T" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO de FOR fbr .AND. EMPTY(&brf1) .and. mcode = month(mdate) .and. year = year(mdate)
SUM ovtime.&otime TO tovtime

                          
                           		&& Total Working Employees On This date
								&& fbr is used in BROWS command FOR fbr
GOTO TOP						&& Go To Top of the file for browsing

							
DEACTIVATE WINDOW getdate


RETURN

*************************************************************************
*	End of PROCEDURE  getdate   Get Date to which entering Attendence
*************************************************************************










***********************************************************************
PROCEDURE paintbrow			&& Paint the Brows Window for getting data

***********************************************************************
ON KEY LABEL TAB DO getdtl
ON KEY LABEL F5 DO search


RESTORE SCREEN FROM top0

ACTIVATE WINDOW full
set escape off
@ 12,0,12,79 BOX REPLICATE("Í",9)
@ 0,37,11,37 BOX REPLICATE("º",9)
@ 13,37,24,37 BOX REPLICATE("³",9)
@ 1,38,1,79 BOX REPLICATE("Ä",9)

@ 0,40 SAY CDOW(mdate)
@ 0,52 SAY ALLTRIM(str(day(mdate)))+" "+cmonth(mdate)+","+;
			ALLTRIM(str(year(mdate)))

BROWS FIELDS;
	Eno = RIGHT(str(deptcode),2)+"-"+;
		RIGHT(str(empno),3)+"³";
		:R :H = "Code",;
	Ename = employee.name :H = "Employee Name",;
	L1 = "³" :1 :R :H = "",;
	&brf1 :H = " " :P = "@!" :V = INLIST(&brf1,'P','A','L','T',' ') :F;
	:E = "(P)resent,(A)bsent,(L)eave,La(T)e",;
	L2 = "³" :1 :H = " ";
	FOR fbr .and. mcode = month(mdate) .and. year = year(mdate);
	WINDOW bratnd;
	FREEZE &brf1;
	TITLE "Daily Attendence Register";
	WHEN empinf();
	VALID total()


ON KEY LABEL TAB		&& Clear TAB key
ON KEY LABEL F5			&& Clear F5 key


RETURN

*************************************************************************
*	End of PROCEDURE  paintbrow	 Paint the Brows Window for getting data*
*************************************************************************



*************************************************************************
PROCEDURE chkmnth
*************************************************************************


SET TALK OFF
SET STATUS OFF
SET SAFETY OFF
SET DATE TO BRITISH

SELECT attend
SET ORDER TO mcode

SEEK(MONTH(mdate))

IF FOUND() .AND. year = year(mdate)
	SET ORDER TO empno
	GOTO TOP
	RETURN .T.
ELSE
	WAIT WINDOW "Month not Opened. Try Again !!!" TIMEOUT 2
	SET ORDER TO empno
	GOTO TOP
	RETURN .F.
ENDIF

SET ORDER TO empno
RETURN .F.
*************************************************************************
* End of Procedure ChkMonth() valid Clause from Get date.
*************************************************************************




*************************************************************************
PROCEDURE empinf  && When Clause of Brow from Paintbrow
*************************************************************************

@ 2,39 TO 11,78 CLEAR

mrecord = RECNO()

lat = Brf1+"LT" 		&& Late and Arrival Time Field
otime = Brf1+"OT"		&& Over Time Field
dtime = Brf1+"DT"		&& Leaving Time field

DO CASE

CASE &brf1 = "P" .OR. &brf1 = "T"

		DO CASE		
								
			CASE VAL(artime.&lat) = 0 .AND. ovtime.&otime = 0;
					.AND. VAL(dptime.&dtime) = 0

				SET BELL TO 1000,1
				?? CHR(7)
				SET BELL TO 512,2
				?? CHR(7)
				
				@ 4,48,7,65 BOX REPLICATE("Í",9)
				@ 5,52 SAY "All Data"
				@ 6,51 SAY "Yet to feed"		


			CASE VAL(artime.&lat) <> 0 ;
				.AND. ovtime.&otime = 0 .AND. salary.otrated=0;
				.AND. VAL(dptime.&dtime) <> 0				
				
					@ 5,50,7,64 BOX
					@ 6,51 SAY "Data Complete"
					@ 9,39,9,78 BOX REPLICATE("Ä",9)

			CASE VAL(artime.&lat) <> 0 .AND. ovtime.&otime <> 0;
					.AND. VAL(dptime.&dtime) <> 0				
								
					@ 5,50,7,64 BOX
					@ 6,51 SAY "Data Complete"
					@ 9,39,9,78 BOX REPLICATE("Ä",9)

			CASE VAL(artime.&lat) <> 0 .AND. ovtime.&otime = 0;
				.AND. VAL(dptime.&dtime) <> 0				
														 
				@ 3,45 SAY "Data not yet fed for: "		
				@ 5,48 SAY "- Over Time"				
				@ 9,39,9,78 BOX REPLICATE("Ä",9)

			CASE VAL(artime.&lat) <> 0 .AND. ovtime.&otime = 0;
				.AND. VAL(dptime.&dtime) = 0 .AND. salary.otrated <> 0			
														 
				@ 3,45 SAY "Data not yet fed for: "		
				@ 5,48 SAY "- Over Time"				
				@ 6,48 SAY "- Leaving Time"
				

			CASE VAL(artime.&lat) <> 0 ;
				.AND. ovtime.&otime = 0 .AND. salary.otrated=0;
				.AND. VAL(dptime.&dtime) = 0				

				@ 3,45 SAY "Data not yet fed for: "		
				@ 6,48 SAY "- Leaving Time"

			CASE VAL(artime.&lat) = 0 .AND. ovtime.&otime <> 0;
				.AND. VAL(dptime.&dtime) <> 0
											 
				@ 3,45 SAY "Data not yet fed for: "
				@ 5,48 SAY "- Arrival Time"			
				
			CASE VAL(artime.&lat) = 0 .AND. ovtime.&otime <> 0;
				.AND. VAL(dptime.&dtime) = 0
											 
				@ 3,45 SAY "Data not yet fed for: "
				@ 5,48 SAY "- Arrival Time"			
				@ 6,48 SAY "- Leaving Time"

			CASE VAL(dptime.&dtime) = 0 .AND. ovtime.&otime <> 0;
				.AND. VAL(artime.&lat) <> 0

				@ 3,45 SAY "Data not yet fed for: "
				@ 5,48 SAY "- Leaving Time"

		ENDCASE


CASE &brf1 = "L"
		@ 2,40 SAY employee.salutation+"."
		@ 2,43 SAY employee.name
		@ 3,42 SAY "is On Leave"

CASE &brf1 = "A"
		@ 2,40 SAY employee.salutation+"."
		@ 2,43 SAY employee.name
		@ 3,42 SAY "is Absent"



OTHERWISE
		@ 2,39 TO 11,78 CLEAR

ENDCASE



			@ 7,39,11,78 BOX 
			@ 8,41 SAY "Arrival Time:" COLOR B+/N
			@ 9,41 SAY "Leaving Time:" COLOR B+/N
			@ 10,41 SAY "Over Time   :" COLOR B+/N
			@ 10,55 SAY ovtime.&otime PICT "99.99"

			DO CASE
				
				CASE VAL(artime.&lat) < 12.00
					
				@ 8,55 SAY VAL(SUBSTR(artime.&lat,1,2)) PICT "@L 99"
				@ 8,57 SAY ":"
				@ 8,58 SAY VAL(SUBSTR(artime.&lat,4,2)) PICT "@L 99"
				@ 8,62 SAY "a.m."

				CASE VAL(artime.&lat) > 12.00
				@ 8,55 SAY VAL(SUBSTR(artime.&lat,1,2)) - 12 PICT "@L 99";
						COLOR R+/N					
				@ 8,57 SAY ":" COLOR R+/N
				@ 8,58 SAY VAL(SUBSTR(artime.&lat,4,2)) PICT "@L 99";
						COLOR R+/N					
				@ 8,62 SAY "p.m."

			ENDCASE
			
			DO CASE
				
				CASE VAL(dptime.&dtime) < 12.00
					
				@ 9,55 SAY VAL(SUBSTR(dptime.&dtime,1,2)) PICT "@L 99"
				@ 9,57 SAY ":"
				@ 9,58 SAY VAL(SUBSTR(dptime.&dtime,4,2)) PICT "@L 99"
				@ 9,62 SAY "a.m."

				CASE VAL(dptime.&dtime) > 12.00
				@ 9,55 SAY VAL(SUBSTR(dptime.&dtime,1,2)) - 12 PICT "@L 99"
				@ 9,57 SAY ":"
				@ 9,58 SAY VAL(SUBSTR(dptime.&dtime,4,2)) PICT "@L 99"
				@ 9,62 SAY "p.m."

			ENDCASE




@ 13,1 SAY "This Month Employee Information"
@ 14,0,14,36 BOX REPLICATE("Ä",9)
@ 15,1 SAY "Department    :"
@ 15,16 SAY deptt.deptname

Pal = d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+;
	  d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+;
	  d29+d30+d31


@ 16,1 SAY "Present       :"
@ 16,18 SAY OCCURS("P",Pal) PICT "99"
REPLACE present WITH OCCURS("P",Pal)

@ 17,1 SAY "Absent        :"
@ 17,18 SAY OCCURS("A",Pal) PICT "99"
REPLACE absent WITH OCCURS("A",Pal)

@ 18,1 SAY "Leaves        :"
@ 18,18 SAY OCCURS("L",Pal) PICT "99"
REPLACE leave WITH OCCURS("L",Pal)

@ 19,1 SAY "Late          :"
@ 19,18 SAY OCCURS("T",Pal) PICT "99"
REPLACE late WITH OCCURS("T",Pal)

@ 20,1 SAY "Blank         :"
@ 20,18 SAY OCCURS(" ",Pal) PICT "99"


IF ovtime.totalot <> 0
	@ 21,1 SAY "Tot.Over time :"
	@ 21,16 SAY ovtime.totalot PICT "9999.99"
ELSE
	@ 21,1 TO 21,24 CLEAR

ENDIF


@ 13,40 SAY "Total Working Employees"
@ 14,40 SAY "on"

@ 14,44 SAY ALLTRIM(str(day(mdate)))+" "+cmonth(mdate)+","+;
			ALLTRIM(str(year(mdate)))
@ 14,62 SAY ":"
@ 14,64 SAY totemp PICT "99999"


@ 16,40 SAY "Total Present"
@ 16,62 SAY ":"
@ 16,64 SAY dp PICT "99999"


@ 17,40 SAY "Total Absent"
@ 17,62 SAY ":"
@ 17,64 SAY da PICT "99999"

@ 18,40 SAY "Total On Leave"
@ 18,62 SAY ":"
@ 18,64 SAY dl PICT "99999"

@ 19,40 SAY "Total Late Coming"
@ 19,62 SAY ":"
@ 19,64 SAY dt PICT "99999"

@ 20,40 SAY "Blank(Yet to feed)"
@ 20,62 SAY ":"
@ 20,64 SAY de PICT "99999"

@ 21,40 SAY "Total Over Time"
@ 21,62 SAY ":"
@ 21,64 SAY tovtime PICT "9999.99"


SET CLOCK TO 0,69
SET CLOCK ON
GOTO mrecord



RETURN .T.

*************************************************************************
* End of Procedure Empinfo() When Clause from Paintbrow
*************************************************************************



**********************************************************
PROCEDURE getdtl  && the Tab Key Program within Paintbrow
**********************************************************


PUBLIC lat,otime

@ 2,39 TO 11,78 CLEAR

lat = Brf1+"LT"		&& Arrival Time & Late Time
otime = Brf1+"OT"	&& Over Time to be feeded

DO CASE

CASE &brf1 = "T"

	@ 2,39 SAY employee.salutation+"."
	@ 2,42 SAY employee.name

	@ 3,40 SAY "Late arrival Time"
	@ 3,58 SAY ":"
	@ 3,60 GET artime.&lat PICT "NN:NN"  	&& Get Late Arrival Time
	READ

IF salary.otrated <> 0	
	@ 5,40 SAY "Over Time Hours"
	@ 5,58 SAY ":"
	@ 5,60 GET ovtime.&otime PICT "99.99" 
	READ VALID ovtotal()
ENDIF

CASE &brf1 = "P"

	@ 2,39 SAY employee.salutation+"."
	@ 2,42 SAY employee.name

	@ 3,40 SAY "Arrival Time     "
	@ 3,58 SAY ":"
	@ 3,60 GET artime.&lat PICT "NN:NN"	&& Get Arrival Time
	READ

IF salary.otrated <> 0
	@ 6,40 SAY "Over Time Hours"
	@ 6,58 SAY ":"
	@ 6,60 GET ovtime.&otime PICT "99.99"
	READ VALID ovtotal()
ENDIF

ENDCASE


RETURN
*************************************************************************
* End of Procedure Getdtl the Tab Key program within Paintbrow
*************************************************************************


***********************************************************************
PROCEDURE search			&& Searching name during data entry
							&& On Key Label F5
***********************************************************************
PUBLIC mname

mname = space(20)

DEFINE WINDOW search FROM 4,40 TO 10,77 DOUBLE SHADOW

ACTIVATE WINDOW search

mrecord = recno()

@ 0,3 SAY "Enter the Employee Name"
@ 1,3 SAY "to Search for; "

DO WHILE .T.
@ 2,3 GET mname
READ

mname2 = ALLTRIM(mname)

LOCATE FOR fbr .AND. UPPER(employee.name) = UPPER(mname2)

IF FOUND()
EXIT
ENDIF

IF .NOT. FOUND()
DO notfound.PRG
GOTO mrecord
LOOP
ENDIF


ENDDO

DEACTIVATE WINDOW search
RETURN

***********************************************************************
*	End of PROCEDURE  search	&& Searching name during data entry
***********************************************************************




**********************************************************************
PROCEDURE total
**********************************************************************

COUNT TO dp FOR &brf1 = "P" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO da FOR &brf1 = "A" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO dl FOR &brf1 = "L" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO dt FOR &brf1 = "T" .and. mcode = month(mdate) .and. year = year(mdate)
COUNT TO de FOR EMPTY(&brf1) .and. mcode = month(mdate) .and. year = year(mdate)



RETURN .T.
**********************************************************************


**********************************************************************
PROCEDURE ovtotal  	&& READ Valid Clause of Get ovtime.&otime from PROCEDURE getdtl
**********************************************************************

mrecord = RECNO()

movtotal = ovtime.d1ot+ovtime.d2ot+ovtime.d3ot+ovtime.d4ot+ovtime.d5ot+;
		   ovtime.d6ot+ovtime.d7ot+ovtime.d8ot+ovtime.d9ot+ovtime.d10ot+;
		   ovtime.d11ot+ovtime.d12ot+ovtime.d13ot+ovtime.d14ot+;
		   ovtime.d15ot+ovtime.d16ot+ovtime.d17ot+ovtime.d18ot+;
		   ovtime.d19ot+ovtime.d20ot+ovtime.d21ot+ovtime.d22ot+;
		   ovtime.d23ot+ovtime.d24ot+ovtime.d25ot+ovtime.d26ot+;
		   ovtime.d27ot+ovtime.d28ot+ovtime.d29ot+ovtime.d30ot+;
		   ovtime.d31ot
		   
	REPLACE ovtime.totalot WITH movtotal	

SUM ovtime.&otime TO tovtime

GOTO mrecord

RETURN .T.
**********************************************************************
* End of Procedure Total
**********************************************************************


***********************************************************************
PROCEDURE endprog			&& End the Program and clear things

***********************************************************************


CLEAR WINDOWS	
CLOSE DATABASES
CLEAR
RESTORE SCREEN FROM backgrnd
ON KEY LABEL ESCAPE ?? CHR(7)
RETURN

***********************************************************************
*	End of PROCEDURE  endprog	 End the Program and  things
***********************************************************************