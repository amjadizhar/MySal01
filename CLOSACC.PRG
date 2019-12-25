
* PROGRAM: CLOSACC.PRG
* Program to Start New Year Keeping Closing Balances
* as Opening Balances for the New Year
* Programmer: Amjad Izhar
* Written On 02-07-99
* This Revision: 04-11-2015
* Accounts Annual Closing Program
* and Opening New balances for the new year
* First of all Post All ledgers in your current directory
* and then copy all files to the new directory in which
* you want to run the program for the new year.
* then in the new directory run this program routine.

*********************************************************

* master.dbf is the posted file of this accounting system
* We will keep only the lines containing Opening Balances.
* and delete all other transaction lines.
*********************************************
* Structure of master.dbf
* DATE		D		8
* C1		N		2	0
* C2		N		2	0
* C3		N		3	0
* VCH_TYPE	N		1
* VCH_NO1	N		2	0
* VCH_NO2	N		5	
* SR_NO		N		6
* DESC		C		30
* DESC		C		30
* DR_AMOUNT	N		12	2
* CR_AMOUNT	N		12	2
* BL_AMOUNT	N		12	2
* STATUS	C		2
*********************************************
* INDEXES OF master.dbf
* TAG: 	AC2
* EXPR: STR(c1,2)+STR(c2,2)+STR(c3,3)
* TAG:	MASTDATE
* EXPR: date
*********************************************
SET TALK OFF
SET SAFETY OFF

CLEAR



	@ 1,8 SAY "  .....    W A R N I N G   .....   " FONT 'FoxFont',11



TEXT

1-	This routine permanently removes all previous records.
2-	Zaps all dbfs and starts new year. 
3- 	Updates last year closing balances with new year opening 
	balances.
4- 	First you have to Copy all files in current directory to
	new directory for the new year.
5- 	Then you can run this procedure.
6-	If you have done so then proceed.

ENDTEXT

WAIT
YesNo = "N"

CLEAR
mDisk=SPACE(2)
mDir = SPACE(25)

STORE SYS(5) TO mDisk
STORE CURDIR() TO MDir
@ 5,8 SAY "You are in Directory" FONT 'FoxFont',11
@ ROW(),COL()+2 SAY ALLTRIM(mDisk+mDir)
@ 10,8 SAY "Are you sure you want to continue ? (Y/N)" FONT 'FoxFont',11
@ row(),col()+2 GET YesNo Pict "@!"
READ

IF YesNo = "N"
	RETURN
ENDIF

@ 10,8 SAY "Deleting All Master Previous year records ..." FONT 'FoxFont',11

USE master EXCL
DELETE FOR NOT EMPTY(vch_type)
PACK

* Entering the date of the new begining year
mDate = date()
CLEAR
@ 10,8 SAY "Please Enter the New Year Begining Date: " FONT 'FoxFont',11
@ 12,8 GET mDate PICTURE "@E" FONT 'FoxFont',11
READ

@ 10,0 TO 18,79 CLEAR
@ 10,8 SAY "Replacing Master with Begining Year date....." FONT 'FoxFont',11

REPLACE ALL date WITH mdate

@ 10,0 TO 18,79 CLEAR
@ 10,8 SAY "Replacing Master Opening Accounts balances with 0" FONT 'FoxFont',11
@ 11,8 SAY "for those accounts which have c1 > 60" FONT 'FoxFont',11

REPLACE ALL Bl_amount WITH 0 FOR c1 > 60
REPLACE ALL Sr_no WITH 1

USE ldgrcode EXCL

REPLACE ALL ldgr_bal WITH 0 FOR c1 > 60
REPLACE ALL open_bal WITH 0 FOR c1 > 60

INDEX ON  str(c1)+str(c2)+str(c3) TAG acc ADDITIVE

REPLACE ALL open_bal WITH ldgr_bal
REPLACE ALL open_stat WITH status

REPLACE ALL Sr_no WITH 1 FOR c3 <> 0

CLEAR ALL
CLOSE ALL


SELECT 1
USE master EXCL
INDEX ON str(c1)+str(c2)+str(c3) TAG acc ADDITIVE

SELECT 2
USE ldgrcode EXCL ORDER acc

SELECT 1
SET RELATION TO str(c1)+str(c2)+str(c3) INTO ldgrcode ADDITIVE

@ 10,0 TO 18,79 CLEAR
@ 10,8 SAY "Updating Opening Balances .... " FONT 'FoxFont',11

REPLACE ALL bl_amount WITH ldgrcode.ldgr_bal
REPLACE ALL status WITH ldgrcode.status
*REPLACE ALL open_bal WITH ldgrcode.ldgr_bal

@ 10,0 TO 18,79 CLEAR
@ 10,8 SAY "Now you will see a Browse window showing you" FONT 'FoxFont',11
@ 11,8 SAY "Opening Balances from Master.dbf, Ldgrcode.dbf" FONT 'FoxFont',11
@ 12,8 SAY "You have to see is there any difference. Press" FONT 'FoxFont',11
@ 13,8 SAY "PgUp, PgDn or arrow keys to move around" FONT 'FoxFont',11
@ 14,8 SAY "When you have done Press ESC key to close this window." FONT 'FoxFont',11
WAIT

BROW FIELDS;
	date,c1,c2,c3,sr_no :1,bl_amount,status :2,;
	ldgrcode.ldgr_bal,ldgrcode.status :2,;
	ldgrcode.open_bal,ldgrcode.open_stat :2,;
	ldgrcode.title
	
CLEA ALL
CLOSE ALL

CLEAR
@ 10,8 SAY "Now all other DBFs Are being emptied for the New Year." FONT 'FoxFont',11

IF FILE("smq.dbf")
	USE smq EXCL
	ZAP
ENDIF

IF FILE("tran.dbf")
	USE tran EXCL
	ZAP
ENDIF

IF FILE("tran-key.dbf")
	USE tran-key EXCL
	ZAP
ENDIF


IF FILE("tran-key")
	USE tran-key EXCL
	ZAP
ENDIF

IF FILE("abc.dbf")
	USE abc EXCL
	ZAP
ENDIF

IF FILE("bs-sum.dbf")
	USE bs-sum EXCL
	ZAP
ENDIF

IF FILE("ldgr.dbf")
	USE ldgr EXCL
	ZAP
ENDIF

IF FILE("mast-ref.dbf")
	USE mast-ref EXCL
	ZAP
ENDIF

IF FILE("mfil.dbf")
	USE mfil EXCL
	ZAP
ENDIF

IF FILE("mldgr.dbf")
	USE mldgr Excl
	ZAP
ENDIF

IF FILE("mmast.dbf")
	USE mmast EXCL
	ZAP
ENDIF

IF FILE("mtran.dbf")
	USE mtran EXCL
	ZAP
ENDIF

IF FILE("pl-dtl.dbf")
	USE pl-dtl EXCL
	ZAP
ENDIF

IF FILE("pl-sum.dbf")
	USE pl-sum EXCL
	ZAP
ENDIF

IF FILE("pl-work2.dbf")
	USE pl-work2 EXCL
	ZAP
ENDIF

IF FILE("rep1.dbf")
	USE rep1 EXCL
	ZAP
ENDIF

IF FILE("tab2.dbf")
	USE tab2 EXCL
	ZAP
ENDIF

IF FILE("tran-rep.dbf")
	USE tran-rep EXCL
	ZAP
ENDIF

IF FILE("trl-dtl.dbf")
	USE trl-dtl EXCL
	ZAP
ENDIF

IF FILE("trl-ref.dbf")
	USE trl-ref EXCL
	ZAP
ENDIF

IF FILE("trl-sum.dbf")
	USE trl-sum EXCL
	ZAP
ENDIF

IF FILE("trl-work.dbf")
	USE trl-work EXCL
	ZAP
ENDIF

IF FILE("vch-cor.dbf")
	USE vch-cor EXCL
	ZAP
ENDIF

IF FILE("voucher.dbf")
	USE voucher EXCL
	ZAP
ENDIF

IF FILE("work.dbf")
	USE work EXCL
	ZAP
ENDIF

CLEA ALL
CLOSE ALL

