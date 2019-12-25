*************************
* PROGRAM: DWOTV2.PRG
* Program to paint a brow for attendence register
* Overtime View
* with complete 31 days
* Programmer: Amjad Izhar
**************************

DO mainprog

**************************
PROCEDURE mainprog
DO iniset
DO SetVars
DO painbrow
DO endprog
RETURN
**************************


*************************
PROCEDURE iniset
*************************
CLEAR
HIDE POPUP mainmenu
SET TALK OFF
SET FUNCTION F1 TO
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
SET FUNCTION F10 TO
SET FUNCTION F11 TO
SET FUNCTION F12 TO

* Department wise total of Over Time Query

CLEAR
CLOSE DATA
PUBLIC mMonth
mMonth = space(16)

SELECT 1
USE attend

SELECT 2
USE month ORDER mcode

SELECT 1
SET RELATION TO mcode INTO month ADDITIVE

STORE ALLTRIM(month.month)+","+ALLTRIM(STR(year)) TO mMonth


SELECT deptcode,sum(d1ot) AS "D1",sum(d2ot) AS "D2",sum(d3ot) AS "D3",;
	   sum(d4ot) AS "D4",sum(d5ot) AS "D5",sum(d6ot) AS "D6",sum(d7ot) AS "D7",;
	   sum(d8ot) AS "D8",sum(d9ot) AS "D9",sum(d10ot) AS "D10",sum(d11ot) AS "D11",;
	   sum(d12ot) AS "D12",sum(d13ot) AS "D13",sum(d14ot) AS "D14",sum(d15ot) AS "D15",;
	   sum(d16ot) AS "D16",sum(d17ot) AS "D17",sum(d18ot) AS "D18",sum(d19ot) AS "D19",;
	   sum(d20ot) AS "D20",sum(d21ot) AS "D21",sum(d22ot) AS "D22",sum(d23ot) AS "D23",;
	   sum(d24ot) AS "D24",sum(d25ot) AS "D25",sum(d26ot) AS "D26",sum(d27ot) AS "D27",;
	   sum(d28ot) AS "D28",sum(d29ot) AS "D29",sum(d30ot) AS "D30",sum(d31ot) AS "D31";
FROM ovtime;
GROUP BY deptcode;
INTO CURSOR dptwtot 


RETURN
*************************


*************************
PROCEDURE SetVars
*************************
PUBLIC CurFld,BrFields1,BrFields2,BrFields3,BrFields4
CurFld = space(3)


BrFields1 = "D1:H='1':6:P='@Z':W=.F.,;
	D2:H='2':6:P='@Z':W=.F.,D3:H='3':6:P='@Z':W=.F.,;
	D4:H='4':6:P='@Z',D5:H='5':6:P='@Z',;
	D6:H='6':6:P='@Z',D7:H='7':6:P='@Z',;
	D8:H='8':6:P='@Z',D9:H='9':6:P='@Z'"

BrFields2 = "D10:H='10':6:P='@Z',;
	D11:H='11':6:P='@Z',D12:H='12':6:P='@Z',;
	D13:H='13':6:P='@Z',D14:H='14':6:P='@Z',;
	D15:H='15':6:P='@Z',D16:H='16':6:P='@Z',;
	D17:H='17':6:P='@Z',D18:H='18':6:P='@Z'"

		
BrFields3 = "D19:H=' 19':6:P='@Z',;
	D20:H=' 20':6:P='@Z',D21:H=' 21':6:P='@Z',;
	D22:H=' 22':6:P='@Z',D23:H=' 23':6:P='@Z',;
	D24:H=' 24':6:P='@Z',D25:H=' 25':6:P='@Z',;
	D26:H=' 26':6:P='@Z',D27:H=' 27':6:P='@Z'"

BrFields4 = "D28:H='28':6:P='@Z',;
	D29:H='29':6:P='@Z',D30:H='30':6:P='@Z',;
	D31:H='31':6:P='@Z'"


RETURN
*************************


******************************************
PROCEDURE painbrow
******************************************
PUBLIC Pal

@ 0,0,21,79 BOX
DEFINE WINDOW b FROM 1,1 TO 20,78
mrecord = RECNO()

USE deptt IN 0
SELECT deptt
SET ORDER TO deptcode


SELECT dptwtot
SET RELATION TO deptcode INTO deptt

BROWS FIELDS;
	deptcode :2 :R :H=" " :w=.f.,;
	deptt.deptname :H="Deptt.Name":R :w=.f.,;
	L1 = "³" :1 :R :H = "" :w=.f.,;
	&BrFields1,;
	&BrFields2,;
	&BrFields3,;
	&BrFields4;
WINDOW b;
LOCK 3;
WHEN Below();
TITLE "Department Wise Over Time Total"	
	
	

RETURN
***************************************

PROCEDURE Below

@ 22,55 SAY mMonth COLOR GR+/B

RETURN .T.




************************
PROCEDURE endprog
************************
CLEAR
CLOSE DATABASES
CLEAR WINDOWS
SHOW POPUP mainmenu
*RESTORE SCREEN FROM backgrnd
RETURN
************************