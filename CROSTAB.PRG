SET TALK OFF
SET SAFETY OFF

CLEAR ALL
CLOSE ALL


PUBLIC mYr1,mYr2
mYr1 = 0
mYr2 = 0

USE MASTER
STORE YEAR(date) TO mYr1
LOCATE FOR YEAR(date) = mYr1+1

IF FOUND()
	STORE YEAR(date) TO mYr2
	GOTO TOP
ELSE
	STORE mYr1 TO mYr2
	GOTO TOP
ENDIF

CLOSE DATA



PUBLIC mc1,mc2,mc3,tc1,tc2,tc3,mDrCr,CurDrive,CurDir
STORE SYS(5) TO CurDrive
STORE CURDIR() TO CurDir
mc1 = 0
mc2 = 0
mc3 = 0
tc1 = 0
tc2 = 0
tc3 = 0
mDrCr = SPACE(2)

IF FILE("tab.dbf")
dele file tab.dbf
ENDIF
IF FILE("tmp1.dbf")
dele file tmp1.dbf
ENDIF

CLEAR

@ 0,10 SAY "Accounts Monthly Break-Up Report"
@ 1,10 SAY "--------------------------------"

@ 5,5 SAY "Account From: "
@ ROW(),COL()+2 GET mC1 PICT "99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET mC2 PICT "99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET mC3 PICT "999"
READ

tC1 = mC1
tC2 = mC2

@ 7,5 SAY "          To: "
@ ROW(),COL()+2 GET tC1 PICT "99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET tC2 PICT "99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET tC3 PICT "999"
READ

@ 9,5 SAY "DR Amounts Or CR amounts (DR/CR) ? "
@ ROW(),COL()+2 GET mDrCr PICT "@! XX" VALID INLIST(mDrCr,'DR','CR') ERROR "Type DR or CR"
READ

@ 14,5 SAY "Processing Please Wait ... "


SELECT ALLTRIM(STR(c1,2)+"-"+STR(c2,2)+"-"+STR(c3,3)) AS "acc",;
	   title;
FROM ldgrcode;
INTO DBF ldgr

DO CASE
	CASE mDrCr = "DR"
		@ 12,5 SAY "Preparing Accounts Debit Side Totals From Master File"
		DO dr_side
	CASE mDrCr = "CR"
		@ 12,5 SAY "Preparing Accounts Credit Side Totals From Master File"
		DO cr_side
ENDCASE


DO GENXTAB WITH 'TAB'

USE tab2 IN 0

SELECT tab2
ZAP
APPEND FROM tab



SET DECIMALS TO 0
SELECT VAL(SUBSTR(ACCOUNT,1,2)) AS  "C1",;
		VAL(SUBSTR(ACCOUNT,4,2)) AS "C2",;
		VAL(SUBSTR(ACCOUNT,7,3)) AS "C3",;
		title,;
		july,;
		august,;
		september,;
		october,;
		november,;
		december,;
		january,;
		february,;
		march,;
		april,;
		may,;
		june;
FROM tab2,ldgr;
WHERE tab2.account = ldgr.acc;
INTO DBF rep1

INDEX ON str(c1)+str(c2)+str(c3) TAG acc ADDITIVE


SELECT tab
USE

IF FILE("tab.dbf")
dele file tab.dbf
ENDIF
IF FILE("tmp1.dbf")
dele file tmp1.dbf
ENDIF

CLOSE DATA

USE rep1 ORDER acc

IF january+february+march+april+may+june = 0
	mYr2 = mYr1+1
ENDIF


************************
DO OPTION
***********************



**************************
PROCEDURE BROW
**************************

IF mYr1 <> mYr2

BROW FIELDS;
		c1,c2,c3 :P="@L",;
		title,;
		Grand_tot = july+august+september+october+november+december+;
					january+february+march+april+may+june :16 :P="@Z 99999,999,999.99",;
		july :14 :H="July "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		august :14 :H="August "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		september :14 :H="September "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		october :14 :H="October "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		november :14 :H="November "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		december :14 :H="December "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		january :14 :H="January "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		february :14 :H="February "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		march :14 :H="March "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		april :14 :H="April "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		may :14 :H="May "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		june :14 :H="June "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99";
		FOR APRIL+AUGUST+OCTOBER+SEPTEMBER+NOVEMBER+DECEMBER+JULY+JUNE+FEBRUARY+JANUARY+MAY+MARCH <> 0 ;
TITLE CurDrive+CurDir+"     Accounts Monthly Break-Up Report";
NOAPPEND NODELETE NOEDIT 
ENDIF


IF mYr1 = mYr2

BROW FIELDS;
		c1,c2,c3 :P="@L",;
		title,;
		Grand_tot = july+august+september+october+november+december+;
					january+february+march+april+may+june :16 :P="@Z 99999,999,999.99",;
		january :14 :H="January "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		february :14 :H="February "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		march :14 :H="March "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		april :14 :H="April "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		may :14 :H="May "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		june :14 :H="June "+ALLTRIM(STR(myr2)) :P="@Z 999,999,999.99",;
		july :14 :H="July "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		august :14 :H="August "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		september :14 :H="September "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		october :14 :H="October "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		november :14 :H="November "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99",;
		december :14 :H="December "+ALLTRIM(STR(myr1)) :P="@Z 999,999,999.99";
		FOR APRIL+AUGUST+OCTOBER+SEPTEMBER+NOVEMBER+DECEMBER+JULY+JUNE+FEBRUARY+JANUARY+MAY+MARCH <> 0 ;
TITLE CurDrive+CurDir+"     Accounts Monthly Break-Up Report";
NOAPPEND NODELETE NOEDIT 
ENDIF


CLEAR GETS
CLEAR READ
CLEAR
CLEAR ALL
CLOSE ALL
RELEASE WINDOWS
RELEASE ALL
SET TALK ON
SET SAFETY ON


RETURN
**************


******************
PROCEDURE cr_side
******************

SELECT ALLTRIM(STR(c1,2)+"-"+STR(c2,2)+"-"+STR(c3,3)) AS "account",;
	   PADR(Cmonth(date),10) AS COLHEAD,;
	   sum(Cr_amount);
FROM master;
WHERE c1 BETWEEN mc1 AND tc1 AND c2 BETWEEN mc2 AND tc2 AND c3 BETWEEN mc3 AND tc3;
GROUP BY account,COLHEAD;
ORDER BY account,COLHEAD;
INTO DBF tmp1

RETURN
*******************


******************
PROCEDURE dr_side
******************

SELECT ALLTRIM(STR(c1,2)+"-"+STR(c2,2)+"-"+STR(c3,3)) AS "account",;
	   PADR(Cmonth(date),10) AS COLHEAD,;
	   sum(Dr_amount);
FROM master;
WHERE c1 BETWEEN mc1 AND tc1 AND c2 BETWEEN mc2 AND tc2 AND c3 BETWEEN mc3 AND tc3;
GROUP BY account,COLHEAD;
ORDER BY account,COLHEAD;
INTO DBF tmp1

RETURN
*******************
PROCE PRINT
*****************
CLOSE DATA
DEACTIVATE WINDOW REPORT
RELEASE WINDOWS
CLEAR
PUBLIC m.Head
m.Head = SPACE(60)
@0,0 CLEAR TO 23,80
@4,1 Say "Please Enter The Title Which Title You Want In Report"
@5,1 GET m.Head
READ
USE REP1
REPO FORM REP1 preview FOR APRIL+AUGUST+OCTOBER+SEPTEMBER+NOVEMBER+DECEMBER+JULY+JUNE+FEBRUARY+JANUARY+MAY+MARCH <> 0 
REPO FORM REP2 preview FOR APRIL+AUGUST+OCTOBER+SEPTEMBER+NOVEMBER+DECEMBER+JULY+JUNE+FEBRUARY+JANUARY+MAY+MARCH <> 0 

CLEAR ALL
CLOSE ALL
CLEAR

RETURN

*******************
PROCEDURE OPTION
********************

DEFINE WIND REPORT FROM 20,10 TO 23,65 TITLE "REPORT MAIN MENU " SHAD
ACTI WIND REPORT
CHOICE =0
@1,4 GET CHOICE PICT '@*RH \<Report To Print;Report To \<Screen' Size 1,8,9 valid choice()
READ CYCLE
RETURN
****************


PROCEDURE choice

DO CASE 
   CASE CHOICE = 1 
        
        CLEAR
        DO PRINT
		
   CASE CHOICE = 2
	    
	    DEAC WIND REPORT
	   	DO BROW

ENDCASE

DEAC WIND REPORT

RETURN .t.
************** 
 
 
 
 
 
     
    