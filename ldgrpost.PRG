**********************
*PROCEDURE LDGRPOST
***********************


CLEAR

IF _DOS
	SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL

SET DATE TO BRIT

HEADING = .T.

STORE DATE() TO MPDATE

SELECT 1		&& Tran.dbf order TranDate

USE tran
SORT ON VCH_TYPE, VCH_NO1, VCH_NO2, SR_NO TO smq
ZAP

APPEND FROM smq
USE

*RUN del smq.dbf

SELECT 1		&& Tran.dbf order TranDate
USE tran ORDER trandate


GOTO BOTTOM

CLEAR

STORE DATE TO MDATE, MPDATE

IF _DOS
	SET COLOR TO +7/1
ENDIF

@ 1, 2 SAY 'Post Transactions To Ledger:'
@ 3, 2 SAY 'This option will post all un-posted vouchers to their respective'
@ 4, 2 SAY 'ledger heads Upto Given Date....'

IF _DOS
	SET COLOR TO 7/1     
ENDIF

@ 13, 14 SAY 'Vouchers upto            exist in file '

IF _DOS
	SET COLOR TO *+/W
ENDIF

@ 13, 29 SAY MDATE

IF _DOS
	SET COLOR TO 7/1     
ENDIF

@ 14, 14 SAY 'Date upto which you want vouchers to be posted ' GET MPDATE
READ

P= ' '

DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO


IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF


CLEAR
@ 12, 23 SAY 'Press any key to start posting'
WAIT ''

CLEAR

@ 13, 23 SAY 'Please wait data is being posted....'

SELECT 2		&& LdgrCode.dbf order AC1
USE ldgrcode ORDER ac1

SELECT 3		&& master.dbf order nil
USE master

STORE 0 TO MDB_CODE, MCR_CODE, WSRNO, WRECNO, MSRNO, WAMOUNT, MAMOUNT, Q, MVCH_NO1, MVCH_NO2

MVOUCH_NO= SPACE(8)
MDISC= SPACE(30)
MDISC1= SPACE(30)
MDATE= DATE()		

STORE '  ' TO ST, RST

SELECT 1		&& Tran.dbf order TranDate
SET FILTER TO FLAG=' '

GOTO TOP		&& Tran.dbf order TranDate

R= 2

CLEAR

* 1st Do While loop Starts Here....
DO WHILE .not. EOF()	&& Tran.dbf order TranDate

   SELECT 1		&& Tran.dbf order TranDate

   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE		&& mCode Variable to seek Acct Code in LdgrCode.dbf

   SELECT 2			&& LdgrCode.dbf order AC1
   SEEK MCODE		&& LdgrCode.dbf order AC1

   IF .not. FOUND()

      IF HEADING

         @ R, 10 SAY 'The Following Account Codes Do Not Exist In Code File'
         R= R+1
         @ R, 10 SAY 'These Account Codes Were Probably Deleted Before Data Posting'
         R= R+1
         @ R, 10 SAY 'Please Correct Your Vouchers Or Create Missing Ledger Heads'
         R= R+1
         @ R, 10 SAY 'Before Data Posting'
         R= R+1
         @ R, 1 TO R, 78
         R= R+1
         @ R, 1 SAY 'Voucher No'
         @ R, 13 SAY 'Voucher Type'
         @ R, 28 SAY 'Account Code'
         R= R+1
         @ R, 1 TO R, 78


         HEADING= .F.

      ENDIF

      SELECT 1			&& Tran.dbf order TranDate
      R= R+1


      @ R, 1 SAY VCH_NO1 PICTURE '99'
      @ R, 3 SAY '-'
      @ R, 4 SAY VCH_NO2 PICTURE '99999'
      @ R, 18 SAY VCH_TYPE
      @ R, 28 SAY C1
      @ R, 30 SAY '-'
      @ R, 31 SAY C2
      @ R, 33 SAY '-'
      @ R, 34 SAY C3


      Q= 21


   ENDIF


   IF R>=21
      WAIT
      CLEAR
      R= 2
      HEADING= .T.
   ENDIF


   SELECT 1			&& Tran.dbf order TranDate
   SKIP
ENDDO
* 1st Do While loop Ends Here....



IF EOF().and.Q=21
   @ 24, 5 SAY 'Vouchers Not Posted --- Press Any Key To Return to Menu '
   WAIT ''
   CLEAR
   CLOSE DATA
   RETURN
ENDIF

CLEAR



* Posting process starts here .....

@ 13, 23 SAY 'Please wait data is being posted....'


SELECT 1			&& Tran.dbf order TranDate
GOTO TOP			&& Tran.dbf order TranDate

* 2nd Do While Loop starts here .....
DO WHILE .not. EOF()			&& SELECT 1 Tran.dbf order TranDate

   DO chk IN tbl_proc.prg


   IF FLAG='Y'.or.DATE>MPDATE	&& If in Tran.dbf record already posted then go to next record and start loop 2nd
   								&& or tran.date > mPdate (required upto date) then also skip
      SKIP
      LOOP
   ENDIF


   IF DR_CR='DR'	&& if tran.dr_cr = 'DR' then tran.amount will be stored in mDR_AMOUNT
      MDR_AMOUNT= AMOUNT
      MCR_AMOUNT= 0		&& and mCR_AMOUNT will be stored as 0
   ELSE
      MCR_AMOUNT= AMOUNT	&& if tran.dr_cr = 'CR' then tran.amount will be stored in mCR_AMOUNT
      MDR_AMOUNT= 0			&& and mDR_AMOUNT will be stored as 0
   ENDIF


   MDATE= DATE	&& mDate will be stored from tran.date

   MC1= C1		&& Tran.dbf order TranDate
   MC2= C2		&& Tran.dbf order TranDate
   MC3= C3		&& Tran.dbf order TranDate
   MVCH_TYPE= VCH_TYPE		&& Tran.dbf order TranDate
   MVCH_NO1= VCH_NO1		&& Tran.dbf order TranDate
   MVCH_NO2= VCH_NO2		&& Tran.dbf order TranDate
   MDISC= DESCRIP			&& Tran.dbf order TranDate
   MDISC1= DESCRIP1			&& Tran.dbf order TranDate


   SELECT 2			&& LdgrCode.dbf order AC1
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)		&& seek tran.c1,c2,c3 in LdgrCode.dbf


   IF FOUND()

      WSRNO= SR_NO+1		&& replace wSrNo Variable with ldgrcode.sr_no + 1

      IF  B->STATUS='  '		&& LdgrCode.dbf order AC1

         IF MDR_AMOUNT>0			&& if tran.amount is DR then replace wAmount with tran.amount
            WAMOUNT= MDR_AMOUNT
            ST= 'DR'				&& make status with 'DR'
         ELSE
            WAMOUNT= MCR_AMOUNT		&& if tran.amount is CR then replace wAmount with tran.amount
            ST= 'CR'				&& make status with 'CR'

         ENDIF

      ENDIF

      IF  B->STATUS='DR'		&& LdgrCode.dbf order AC1
         IF MDR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MDR_AMOUNT 	&& if tran.dr_cr = 'DR' then variable wAmount replace with ldgrcode.ldgr_bal + dr_amount
            ST= 'DR'						&& Replace variable ST with 'DR'
         ELSE
            WAMOUNT= LDGR_BAL-MCR_AMOUNT	&& if tran.dr_cr = 'CR' then variable wAmount replace with ldgrcode.ldgr_bal + cr_amount
            IF WAMOUNT>0
               ST= 'DR'						&& Replace variable ST with 'DR'
            ELSE
               ST= 'CR'						&& Replace variable ST with 'DR'
            ENDIF
         ENDIF
      ENDIF

      IF  B->STATUS='CR'		&& LdgrCode.dbf order AC1
         IF MCR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MCR_AMOUNT
            ST= 'CR'
         ELSE
            WAMOUNT= LDGR_BAL-MDR_AMOUNT
            IF WAMOUNT>0
               ST= 'CR'
            ELSE
               ST= 'DR'
            ENDIF
         ENDIF
      ENDIF


      REPLACE LDGR_BAL WITH ABS(WAMOUNT)		&& replace ldgrcode.ldgr_bal with varialbe wAmount
      REPLACE SR_NO WITH WSRNO					&& replace ldgrcode.sr_no with variable wSrNo
      REPLACE B->STATUS WITH ST		&& LdgrCode.dbf order AC1
   ENDIF


   SELECT 3			&& master.dbf order nil
   APPEND BLANK
   REPLACE DATE WITH MDATE			&& replace master.date with variable mDate
   REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3		&& replace master.c1 with mC1, master.c2 with mC2, master.c3 with mC3

   REPLACE BL_AMOUNT WITH ABS(WAMOUNT) && replace master.BL_AMOUNT with wAmount
   
   IF MDR_AMOUNT>0
      REPLACE DR_AMOUNT WITH MDR_AMOUNT		&& replace master.dr_amount with mDr_amount
   ELSE
      REPLACE CR_AMOUNT WITH MCR_AMOUNT		&& replace master.cr_amount with mCr_amount
   ENDIF

   REPLACE DESC WITH MDISC					&& replace master.desc with mDisc
   REPLACE DESC1 WITH MDISC1				&& replace master.desc1 with mDisc1
   REPLACE VCH_TYPE WITH MVCH_TYPE			&& replace master.vch_type with mVch_type
   REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2	&& replace master.vch_no1 with mvch_no1, master.vch_no2 with mVch_No2
   REPLACE SR_NO WITH WSRNO					&& replace master.sr_no with wSrNo
   REPLACE C->STATUS WITH ST				&& replace master.status with st

   WSRNO= 0

   SELECT 1			&& Tran.dbf order TranDate
   REPLACE FLAG WITH 'Y'		&& replace tran.flag with 'Y' - means entry posted.
   SKIP				&& Tran.dbf order TranDate


ENDDO
* 2nd Do While Loop starts here .....







CLOSE ALL

USE master
REINDEX

SELECT * ;
FROM TRAN ;
ORDER BY DATE,VCH_TYPE,VCH_NO1,VCH_NO2,SR_NO ;
INTO DBF TRANSORT.DBF

USE TRANSORT
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG CODE

SELE TRAN
USE

USE TRAN
ZAP

APPEND FROM TRANSORT


CLEAR
CLOSE ALL


RETURN
******************************

