* PROGRAM: ISSUPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* WRITTEN ON: 01-06-2005
* THIS REVISION: 01-06-2005
***********************************



***********************
PROCEDURE ISSU_MEN
***********************

DO WHILE .T.

   SET DELETED ON
   USE st-key ORDER st_idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 19

IF _DOS
   SET COLOR TO 7/1
ELSE
SET COLOR TO
ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ELSE
SET COLOR TO
ENDIF

   @ 6, 20 SAY 'Inventory Issue To Production / Returns '

IF _DOS
   SET COLOR TO 7/1
ELSE
SET COLOR TO
ENDIF

   @ L+0, M PROMPT ' 1 - Issue To Production ........ Entry      '
   @ L+1, M PROMPT ' 2 - Issue To Production ........ Correction '
   @ L+2, M PROMPT ' 3 - Issue To Production ........ Deletion   '
   @ L+3, M PROMPT ' 4 - Issue To Production Note ... Printing   '
   @ L+4, M PROMPT ' 5 - Issue To Production with Formula        '
   @ L+5, M TO L+5, 61
   @ L+6, M PROMPT ' 6 - Issue To Production Return.. Entry      '
   @ L+7, M PROMPT ' 7 - Issue To Production Return.. Correction '
   @ L+8, M PROMPT ' 8 - Issue To Production Return.. Deletion   '
   @ L+9, M PROMPT ' 9 - Issue To Production Return.. Printing   '
   @ L+11, M PROMPT '10 - Return to Previous Menu                 '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE
      CASE MCHOICE=1
         CLEAR
         DO issu_ent

      CASE MCHOICE=2
         CLEAR
         DO issu_cor

      CASE MCHOICE=3
         CLEAR
         DO issu_del

      CASE MCHOICE=4
         CLEAR
         DO issu_prt

      CASE MCHOICE=5
         CLEAR
         DO issuent

      CASE MCHOICE=6
         CLEAR
         DO ispr_ent

      CASE MCHOICE=7
         CLEAR
         DO ispr_cor

      CASE MCHOICE=8
         CLEAR
         DO ispr_del

      CASE MCHOICE=9
         CLEAR
         DO ispr_prt

      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
*******



*1-
******************
PROCEDURE ISSU_ENT
******************

CLEAR
CLOSE ALL

SET DATE brit
SET DELETED ON
SET EXCLUSIVE ON
SET AUTOSAVE ON

PUBLIC MDOC_NO
MDOC_NO = 0

SELECT 1
USE st-tran ALIAS st_tran EXCLUSIVE
INDEX ON STR(doc_type,1)+STR(doc_no,7) TAG docno
SET ORDER TO docno
SET FILTER TO doc_type = 3
GOTO BOTTOM
MDOC_NO = doc_no + 1
USE

FLUSH

SELECT 3
USE item ORDER item_idx

SELECT 4
USE dept ORDER dept_idx

SELECT 5
USE loc ORDER loc_idx

SELECT 7
USE item-bal ORDER itm_bal


DO WHILE .T.

FLUSH

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

   R= 3
   MDOC_TYPE= 3

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 15 SAY 'INVENTORY : ISSUE TO PRODUCTION ENTRY '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE

   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO


   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
      READ

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

      @ 23, 45 CLEAR TO 23, 78
      IF MDOC_NO=0
         LOOP
      ENDIF

      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MVCH_NO

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)

      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF

      APPEND BLANK

      IF .not. RLOCK()
         rlock()
      ENDIF

      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
		FLUSH
      UNLOCK

      SELECT 2
      USE

      @ R, 57 SAY 'Date ' GET MDATE
      READ

      DO chk IN TBL_PROC.PRG

      R= R+1
      ABC= 0

      DO WHILE ABC=0

         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ R, 33 SAY DEPT
         MDEPT= DEPT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
         READ
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 33 SAY LOC
         MLOC= LOC

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 45 SAY 'Qty '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 13 SAY ITEM

IF _DOS
            SET COLOR TO 7/1
ENDIF

            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               *LOOP
            ENDIF
            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 45 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 3, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
         UNLOCK
         FLUSH
         USE
         IF ANS='Y'
            IF R<=21
               STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
               R= R+1
               LOOP
            ELSE
               STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
               @ 9, 0 CLEAR TO 24, 79
               R= 9
               LOOP
            ENDIF
         ENDIF
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO p_cor1 IN PRODPROC.PRG
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE
         USE st-tran EXCLUSIVE
         INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
         SET ORDER TO stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
            FLUSH
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         EXIT
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******

*2-
********************
PROCEDURE ISSU_COR
********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=3
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 3

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'ISSUE TO PRODUCTION CORRECTION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MNAVG
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   R= R+1
   DO chk IN TBL_PROC.PRG
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /W
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1

   DO p_cor1 IN PRODPROC.PRG

   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DEPT_CODE WITH MDEPT_CODE
   FLUSH
   USE
   SELECT 2
   USE st-tran ORDER stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=3
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
********

*3-
*******************
PROCEDURE ISSU_DEL
*******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=3
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 3

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'ISSUE TO PRODUCTION DELETION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   DO chk IN TBL_PROC.PRG
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   SELECT 1
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MQTY= T_QTY

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 2 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 2 SAY MP1
      @ R, 4 SAY '-'
      @ R, 5 SAY MI2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 5 SAY MP2
      @ R, 7 SAY '-'
      @ R, 8 SAY MI3 PICTURE '9999'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 8 SAY MP3

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 13 SAY ITEM

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 1

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 45 SAY MQTY PICTURE '9999999.9999'

IF _DOS
      SET COLOR TO 7/1
ENDIF

      R= R+1
      IF R>=23
         WAIT
         @ 9, 0 CLEAR TO 24, 78
         R= 9
      ENDIF
      SKIP
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran ORDER stidx
   IF ANS='Y'
      SELECT 2
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=3
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion   - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******


*4-
**********************
PROCEDURE ISSU_PRT
**********************
CLEAR
CLOSE ALL
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Issue Note '
   @ 4, 33 SAY '---------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 3
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From I. Note : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   USE st-tran ORDER stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   I. Note : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF
   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=3
   SELECT 2
   USE
   SELECT 1
   USE st-rep
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEAD= .T.
CLEAR
DO WHILE .not. EOF()
   MDOC_NO= DOC_NO
   STORE 0 TO MTOT
   DO WHILE DOC_NO=MDOC_NO
      IF HEAD
         L= 3
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 30 SAY 'Issue Note '
            L= L+1
            @ L, 30 SAY '---------- '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Issue Note '
            L= L+1
            @ L, 30 SAY '---------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'I. Note No   : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MDEPT_CODE= DEPT_CODE
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Department   : '
         MDEPT_CODE= REPLICATE('0',2-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
         @ L, 18 SAY MDEPT_CODE
         SELECT 4
         SEEK MDEPT_CODE
         IF FOUND()
            @ L, 30 SAY DEPT
         ENDIF
         L= L+1
         SELECT 1
         @ L, 1 SAY 'Location     : '
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 5
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 45 SAY 'Unit'
         @ L, 58 SAY 'Qty '
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEAD= .F.
      ENDIF
      SELECT 1
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MP3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 3
      SEEK MI_CODE
      IF FOUND()
         MUNIT= UNIT
         @ L, 12 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      @ L, 45 SAY MUNIT
      @ L, 55 SAY MQTY PICTURE '9999999.9999'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ENDIF

         CHO= 0
         DO WHILE .T.
            @ 24, 70 GET CHO PICTURE '@z 9'
            READ
            IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
               EXIT
            ENDIF
         ENDDO
         IF READKEY()=0
            CLEAR
            RUN del st-rep.dbf
            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'.and.L>56
         HEAD= .T.
      ENDIF
   ENDDO
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
      SET COLOR TO 7/1
ENDIF

      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         RUN del st-rep.dbf
         CLOSE ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEAD= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      HEAD= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
********


*5-
*******************
PROCEDURE ISSUENT
*******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item ORDER item-idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
HEAD= .T.
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   MDOC_TYPE= 3

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'ISSUE TO PRODUCTION ENTRY '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   STORE 0 TO MFI1, MFI2, MFI3, FMQTY, Q
   DO WHILE .T.
      R= 3
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MVCH_NO

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      FLUSH
      SELECT 2
      USE
      @ R, 54 SAY 'Date ' GET MDATE
      READ
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ R, 33 SAY DEPT
         MDEPT= DEPT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
         READ
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 33 SAY LOC
         MLOC= LOC

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ 23, 50 SAY 'Press Ctrl Q To Quit '
         @ R, 2 SAY 'Finished Item No ..'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 23 SAY '  -  -    '

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 23 GET MFI1 PICTURE '99'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78

IF _DOS
         SET COLOR TO /w
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
         @ R, 23 SAY MP1
         @ R, 25 SAY '-'
         @ R, 26 GET MFI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
         @ R, 26 SAY MP2
         @ R, 28 SAY '-'
         @ R, 29 GET MFI3 PICTURE '9999'
         READ
         IF MFI3=0

IF _DOS
            SET COLOR TO 7/1
ENDIF

            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
         @ R, 29 SAY MP3

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ 23, 50 CLEAR TO 23, 78
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         IF MFI1=0.and.MFI2=0.and.MFI3=0
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
         SEEK MFICODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 38 SAY ITEM

IF _DOS
         SET COLOR TO 7/1
ENDIF

         MUNIT= UNIT
         SELECT 8
         USE formula ORDER for_idx1
         STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
         SEEK MFICODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 7 SAY 'This Item Code Does Not Exist In Your Formula File ... Please Re-try'
            WAIT ''
            @ 23, 5 SAY SPACE(70)
            LOOP
         ENDIF
         MT= 1
      ENDDO
      SELECT 8
      USE
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, 38 SAY 'Unit '

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 44 SAY MUNIT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 2 SAY 'Qty '
         @ R, 23 GET FMQTY PICTURE '9999999.9999'
         READ
         IF FMQTY=0
            LOOP
         ENDIF
         MT= 1
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 24, 20 CLEAR TO 24, 78
      IF ANS='Y'
         LOOP
      ENDIF
      SELECT 6
      USE for-work
      APPEND FROM formula FOR FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
      GOTO TOP
      DO WHILE .not. EOF()
         MI1= RI1
         MI2= RI2
         MI3= RI3
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            IF HEAD
               CLEAR
               R= 4
               @ 2, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'

IF _DOS
               SET COLOR TO +7/1
ENDIF

               @ R, 10 SAY 'Location Code'
               @ R, 30 SAY 'Item Code'

IF _DOS
               SET COLOR TO 7/1
ENDIF

               R= R+2
               HEAD= .F.
            ENDIF
            MLOC= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
            @ R, 10 SAY MLOC PICTURE '9999'
            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 30 SAY MP1 PICTURE '99'
            @ R, 32 SAY '-'
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 33 SAY MP2 PICTURE '99'
            @ R, 35 SAY '-'
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 36 SAY MP3 PICTURE '9999'
            Q= 21
         ENDIF
         IF R>=21
            WAIT
            CLEAR
            R= 2
            HEAD= .T.
         ENDIF
         R= R+1
         SELECT 6
         SKIP
      ENDDO
      IF EOF().and.Q=21
         @ 24, 5 SAY 'Vouchers Not Posted --- Press Any Key To Return to Menu '
         WAIT ''
         CLEAR
         CLEAR ALL
         RETURN
      ENDIF
      SELECT 6
      GOTO TOP
      STORE 0 TO MSR_NO
      DO WHILE .not. EOF()
         MI1= RI1
         MI2= RI2
         MI3= RI3
         MQTY= QTY
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 3, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         REPLACE T_QTY WITH MQTY*FMQTY
         UNLOCK
         FLUSH
         USE
         SELECT 6
         SKIP
      ENDDO
      SELECT 2
      USE st-tran ORDER stidx
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      USE
      SELECT 2
      USE st-key ORDER st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
         FLUSH
      ENDIF
      USE
      SELECT 1
      USE st-work
      DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      USE
      SELECT 6
      DELETE ALL
      USE
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******


*6-
*******************
PROCEDURE ISPR_ENT
*******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 4

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'ISSUE TO PRODUCTION RETURN ENTRY '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Production Return   ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MVCH_NO

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 10 SAY 'This Production Return No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      FLUSH
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ R, 33 SAY DEPT
         MDEPT= DEPT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
         READ
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 33 SAY LOC
         MLOC= LOC

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 45 SAY 'Qty '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 13 SAY ITEM

IF _DOS
            SET COLOR TO 7/1
ENDIF

            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               *LOOP
            ENDIF
            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 45 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
         UNLOCK
         FLUSH
         USE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            LOOP
         ENDIF
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO p-cor1
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran ORDER stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key ORDER st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
            FLUSH
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         EXIT
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******


*7-
******************
PROCEDURE ISPR_COR
******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=4
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 4

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 25 SAY 'ISSUE TO PRODUCTION RETURN CORRECTION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MNAVG
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   R= R+1
   DO chk IN TBL_PROC.PRG
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /W
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   DO p-cor1
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DEPT_CODE WITH MDEPT_CODE
   FLUSH
   USE
   SELECT 2
   USE st-tran ORDER stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=4
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******



*8-
*******************
PROCEDURE ISPR_DEL
*******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=4
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 4

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 25 SAY 'ISSUE TO PRODUCTION RETURN DELETION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Issue To Production ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   DO chk IN TBL_PROC.PRG
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   SELECT 1
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MQTY= T_QTY

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 2 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 2 SAY MP1
      @ R, 4 SAY '-'
      @ R, 5 SAY MI2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 5 SAY MP2
      @ R, 7 SAY '-'
      @ R, 8 SAY MI3 PICTURE '9999'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 8 SAY MP3

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 13 SAY ITEM

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 1

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 45 SAY MQTY PICTURE '9999999.9999'

IF _DOS
      SET COLOR TO 7/1
ENDIF

      R= R+1
      IF R>=23
         WAIT
         @ 9, 0 CLEAR TO 24, 78
         R= 9
      ENDIF
      SKIP
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran ORDER stidx
   IF ANS='Y'
      SELECT 2
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=4
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion   - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******


*9-
********************
PROCEDURE ISPR_PRT
********************
CLEAR
CLOSE ALL
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Issue Return Note '
   @ 4, 33 SAY '----------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 4
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From I.R. Note : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   USE st-tran ORDER stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   I.R. Note : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF
   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=4
   SELECT 2
   USE
   SELECT 1
   USE st-rep
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEAD= .T.
CLEAR
DO WHILE .not. EOF()
   MDOC_NO= DOC_NO
   STORE 0 TO MTOT
   DO WHILE DOC_NO=MDOC_NO
      IF HEAD
         L= 3
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 30 SAY 'Issue Return Note '
            L= L+1
            @ L, 30 SAY '----------------- '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Issue Return Note '
            L= L+1
            @ L, 30 SAY '----------------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'I. Note No   : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MDEPT_CODE= DEPT_CODE
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Department   : '
         MDEPT_CODE= REPLICATE('0',2-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
         @ L, 18 SAY MDEPT_CODE
         SELECT 4
         SEEK MDEPT_CODE
         IF FOUND()
            @ L, 30 SAY DEPT
         ENDIF
         L= L+1
         SELECT 1
         @ L, 1 SAY 'Location     : '
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 5
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 45 SAY 'Unit'
         @ L, 58 SAY 'Qty '
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEAD= .F.
      ENDIF
      SELECT 1
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MP3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 3
      SEEK MI_CODE
      IF FOUND()
         MUNIT= UNIT
         @ L, 12 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      @ L, 45 SAY MUNIT
      @ L, 55 SAY MQTY PICTURE '9999999.9999'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ENDIF

         CHO= 0
         DO WHILE .T.
            @ 24, 70 GET CHO PICTURE '@z 9'
            READ
            IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
               EXIT
            ENDIF
         ENDDO
         IF READKEY()=0
            CLEAR
            RUN del st-rep.dbf
            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'.and.L>56
         HEAD= .T.
      ENDIF
   ENDDO
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
      SET COLOR TO 7/1
ENDIF


      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         RUN del st-rep.dbf
         CLOSE ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEAD= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      HEAD= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
*******


