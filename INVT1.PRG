
*************
PROCEDURE TOP
*************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
@ 1, 1 SAY 'Gold Coin Plus Inventory '
@ 2, 0 TO 2, 79
@ 4, 0 TO 4, 79
@ 3, 18 SAY '         ASIA SNACKS (PVT) LIMITED  '
RETURN
******

*************
PROCEDURE BOT
*************
@ 22, 3 SAY 'Designed & Developed by:'
@ 23, 0 TO 23, 79 DOUBLE
@ 24, 3 SAY 'Logical Systems (Pvt) Limited, Lahore - Pakistan. Tel: (042) 872987, 879298 '
RETURN
******

**************
PROCEDURE LOGO
**************
CLEAR
TEXT

    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
    ±±±±±±±±±±±±±±±±±±±  (C) LOGICAL SYSTEMS (PVT) LIMITED   ±±±±±±±±±±±±±
    ±±±±±±±±±±±±±±±±±±±±±±±± 129-E-1, MAIN BOULEVARD  ±±±±±±±±±±±±±±±±±±±±
    ±±±±±±±±±±±±±±±±±±±±±±± GULBERG-3, Lahore, PAKISTAN ±±±±±±±±±±±±±±±±±±
    ±±±±±±±±±±±±±±±±±±±±±±±±±±±± TEL (042) 879927 ±±±±±±±±±±±±±±±±±±±±±±±±
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€


               Program :  Gold Coin Inventory Program
               Version :  1.0
               Revision:  0 
               Dated   :  July 14, 1992
               
7              This Program Is Licensed Exclusively For The
               Use Of Messrs.
               
       
               ASIA SNACKS (PVT) LIMITED  
               License # LSL-288-065

               

ENDTEXT
STORE DATE() TO DD1
STORE ' ' TO CH
@ 22, 25 SAY 'Enter Todays Date: (dd/mm/yy) ' GET DD1
READ

IF _DOS
SET COLOR TO 
ENDIF

DD2= LTRIM(STR(MONTH(DD1),2))+'-'+LTRIM(STR(DAY(DD1),2))+'-'+LTRIM(STR(YEAR(DD1),4))
!date &dd2
RETURN
******

****************
PROCEDURE SCREEN
****************
@ 1, 25 SAY ' ASIA SNACKS (PVT) LIMITED. '
@ 2, 25 SAY '--------------------------- '
RETURN
******

*****************
PROCEDURE TOP_PRT
*****************
@ 1, 0 SAY CHR(14)
@ 1, 6 SAY ' ASIA SNACKS (PVT) LIMITED. '
RETURN
******
******************
PROCEDURE CODE-MEN
******************
DO WHILE .T.
   INT= 1
   L= 8
   M= 26

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1        
ENDIF

   @ 6, 28 SAY '  Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1        
ENDIF

   @ L+0, M PROMPT ' 1  -  Item       .. Codes '
   @ L+1, M PROMPT ' 2  -  Location   .. Codes '
   @ L+2, M PROMPT ' 3  -  Department .. Codes '
   @ L+3, M PROMPT ' 4  -  Machine    .. Codes '
   @ L+4, M PROMPT ' 5  -  Zone       .. Codes '
   @ L+5, M PROMPT ' 6  -  City       .. Codes '
   IF INT<>1
      @ L+6, M PROMPT ' 7  -  Debtors    .. Codes '
      @ L+7, M PROMPT ' 8  -  Creditors  .. Codes '
      @ L+9, M PROMPT ' 9  -  Return to Main Menu '
   ELSE
      @ L+7, M PROMPT ' 9  -  Return to Main Menu '
   ENDIF
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         CLEAR
         DO item_men
      CASE CHOICE=2
         CLEAR
         DO loc_men
      CASE CHOICE=3
         CLEAR
         DO dept_men
      CASE CHOICE=4
         CLEAR
         DO mach_men
      CASE CHOICE=5
         CLEAR
         DO zone_men
      CASE CHOICE=6
         CLEAR
         DO city_men
      CASE CHOICE=7
         CLEAR
         IF INT<>1
            DO debt_men
         ELSE
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
      CASE CHOICE=8
         CLEAR
         IF INT<>1
            DO cred_men
         ENDIF
      CASE CHOICE=9
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
*******
******************
PROCEDURE ITEM_MEN
******************
DO WHILE .T.
   INT= 1
   L= 8
   M= 24

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 26 SAY '  Item Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Item Code ...... Entry          '
   @ L+1, M PROMPT ' 2 - Item Code ...... Correction     '
   @ L+2, M PROMPT ' 3 - Item Code ...... Deletion       '
   @ L+3, M PROMPT ' 4 - Item Code ...... Printing       '
   @ L+4, M PROMPT ' 5 - Complete Item Codes Printing    '
   @ L+5, M PROMPT ' 6 - Item Codes Printing with Dr/Cr  '
   @ L+6, M PROMPT ' 7 - Item Codes Printing (Code Wise) '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=8
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=7
         CLEAR
         DO itemprt3
      CASE MCHOICE=6
         CLEAR
         DO itemprt2
      CASE MCHOICE=5
         CLEAR
         DO itemprt1
      CASE MCHOICE=4
         CLEAR
         DO item_prt
      CASE MCHOICE=3
         CLEAR
         DO item_del
      CASE MCHOICE=2
         CLEAR
         DO item_cor
      CASE MCHOICE=1
         CLEAR
         DO item_ent
   ENDCASE
ENDDO
RETURN
*******
******************
PROCEDURE ITEM_ENT
******************
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MI1, MI2, MI3, CNT
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 30 SAY 'Item Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MITEM= SPACE(30)
   MUNIT= SPACE(6)
   STORE SPACE(5) TO MI_CODE2
   STORE 0 TO MMIN_LEV, MMAX_LEV, MS_U_PRICE, CNT
   STORE 0 TO MLAST_PRIC, MREO_LEV, MREO_QTY, MC1, MC2, MC3
   STORE 0 TO MCC1, MCC2, MCC3, MDC1, MDC2, MDC3
   STORE DATE() TO MDATE
   DO WHILE .T.
      SELECT 1
      @ 23, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 3, 25 SAY '  -  -    '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 3, 10 SAY 'Item     '
      @ 3, 25 GET MI1 PICTURE '99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 49 CLEAR TO 23, 73
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 3, 25 SAY MP1 PICTURE '99'
      @ 3, 27 SAY '-'

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 3, 28 GET MI2 PICTURE '99'
      READ
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 3, 28 SAY MP2 PICTURE '99'
      @ 3, 30 SAY '-'
      @ 3, 31 GET MI3 PICTURE '9999'
      READ
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 3, 31 SAY MP3 PICTURE '9999'

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 49 CLEAR TO 23, 73
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
      IF MI1=0.and.MI2=0.and.MI3=0
         LOOP
      ENDIF
      SELECT 1
      SEEK MITEM_CODE
      IF CNT=0
         IF FOUND()
            @ 23, 20 SAY 'This Code Already Exists ... Please Re-try'
            WAIT ''
            @ 23, 19 SAY SPACE(43)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         UNLOCK
         CNT= 1
      ENDIF
      IF MI3=0
         @ 8, 1 CLEAR TO 23, 77
         @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
         @ 13, 8 SAY '          will make it a consolidation head i.e you'
         @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
         @ 15, 8 SAY '          directly into this head.'
         @ 16, 8 SAY '          Hit any key to continue ..........'
         WAIT ''
         @ 12, 7 CLEAR TO 16, 63
      ENDIF
      @ 3, 40 GET MITEM
      READ
      IF MI3<>0
         @ 5, 10 SAY 'Unit          ' GET MUNIT FUNCTION '!'
         @ 7, 10 SAY 'Reording Level' GET MREO_LEV PICTURE '9999999'
         @ 9, 10 SAY 'Reording Qty  ' GET MREO_QTY PICTURE '9999999'
         READ
         @ 7, 42 SAY 'Minimum Level.      ' GET MMIN_LEV PICTURE '9999999'
         READ
         MT= 0
         DO WHILE MT=0
            @ 9, 42 SAY 'Maximum Level.      ' GET MMAX_LEV PICTURE '9999999'
            READ
            IF MMAX_LEV<MMIN_LEV
               @ 23, 15 SAY 'Maximum Level Is Less Than Minimum Level --- Please Retry'
               WAIT ''
               @ 23, 14 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ 11, 10 SAY 'Last Purchased From (Party Code)    '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 11, 45 GET MC1 PICTURE '99'
            @ 11, 47 SAY '-'
            @ 11, 50 SAY '-'

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ 11, 48 GET MC2 PICTURE '@z 99'
            @ 11, 51 GET MC3 PICTURE '@z 999'
            READ
            IF MC1=0.and.MC2=0.and.MC3=0
               EXIT
            ENDIF
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 11, 45 SAY MPC1
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 11, 48 SAY MPC2
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
            @ 11, 51 SAY MPC3

IF _DOS
            SET COLOR TO 7/1
ENDIF

            SELECT 2
            SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
            IF .not. FOUND()
               @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
               WAIT ''
               @ 23, 19 SAY SPACE(46)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ 13, 45 SAY TITLE

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ 15, 10 SAY 'Last Purchase Price'
            @ 15, 45 GET MLAST_PRIC PICTURE '9999999.99'
            READ
            MT= 1
         ENDDO
         IF INT=1
            R= 16
            @ R, 1 TO R, 78
            R= R+1
            @ R, 5 SAY 'Credit Code (For Sale Transaction) '
            R= R+1
            MT= 0
            DO WHILE MT=0

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 16 SAY '  -  -   '

IF _DOS
               SET COLOR TO 7/1
ENDIF

               @ R, 16 GET MCC1 PICTURE '@z 99'

IF _DOS
               SET COLOR TO /w
ENDIF

               MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MCC1))))+LTRIM(STR(MCC1))
               @ R, 16 SAY MPC1
               @ R, 19 GET MCC2 PICTURE '@z 99'
               READ
               MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MCC2))))+LTRIM(STR(MCC2))
               @ R, 19 SAY MPC2
               @ R, 22 GET MCC3 PICTURE '@z 999'
               READ
               MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MCC3))))+LTRIM(STR(MCC3))
               @ R, 22 SAY MPC3

IF _DOS
               SET COLOR TO 7/1
ENDIF

               IF MCC1=0.and.MCC2=0.and.MCC3=0
                  EXIT
               ENDIF
               SELECT 2
               STORE STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3) TO MCCODE
               SEEK MCCODE
               IF .not. FOUND()
                  @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 18 CLEAR TO 23, 63
                  LOOP
               ENDIF

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 29 SAY TITLE

IF _DOS
               SET COLOR TO 7/1
ENDIF

               MT= 1
            ENDDO
            R= R+2
            @ R, 5 SAY 'Debit Code (For Purchase Transaction) '
            R= R+1
            MT= 0
            DO WHILE MT=0

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 16 SAY '  -  -   '

IF _DOS
               SET COLOR TO 7/1
ENDIF

               @ R, 16 GET MDC1 PICTURE '@z 99'

IF _DOS
               SET COLOR TO /w
ENDIF

               MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MDC1))))+LTRIM(STR(MDC1))
               @ R, 16 SAY MPC1
               @ R, 19 GET MDC2 PICTURE '@z 99'
               READ
               MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MDC2))))+LTRIM(STR(MDC2))
               @ R, 19 SAY MPC2
               @ R, 22 GET MDC3 PICTURE '@z 999'
               READ
               MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MDC3))))+LTRIM(STR(MDC3))
               @ R, 22 SAY MPC3

IF _DOS
               SET COLOR TO 7/1
ENDIF

               IF MDC1=0.and.MDC2=0.and.MDC3=0
                  EXIT
               ENDIF
               SELECT 2
               STORE STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3) TO MDCODE
               SEEK MDCODE
               IF .not. FOUND()
                  @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 18 CLEAR TO 23, 63
                  LOOP
               ENDIF

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 29 SAY TITLE

IF _DOS
               SET COLOR TO 7/1              
ENDIF

               MT= 1
            ENDDO
         ENDIF
      ENDIF
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 30 SAY ' Any Corrections - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      @ 23, 25 CLEAR TO 23, 72
      IF P='Y'
         LOOP
      ENDIF
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE REORD_LEV WITH MREO_LEV, REORD_QTY WITH MREO_QTY, UNIT WITH MUNIT
      REPLACE MIN_LEV WITH MMIN_LEV, MAX_LEV WITH MMAX_LEV, ITEM WITH MITEM
      REPLACE LAST_PRICE WITH MLAST_PRIC, C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      IF INT=1
         REPLACE CC1 WITH MCC1, CC2 WITH MCC2, CC3 WITH MCC3
         REPLACE DC1 WITH MDC1, DC2 WITH MDC2, DC3 WITH MDC3
      ENDIF
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 30 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
   MI3= MI3+1
ENDDO
RETURN
******

******************
PROCEDURE ITEM_COR
******************
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE ldgrcode INDEX ac1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 30 SAY 'Item Codes Correction '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MITEM= SPACE(30)
   MUNIT= SPACE(6)
   STORE SPACE(5) TO MI_CODE2
   STORE 0 TO MI1, MI2, MI3
   STORE 0 TO MMIN_LEV, MMAX_LEV, MS_U_PRICE
   STORE 0 TO MCC1, MCC2, MCC3, MDC1, MDC2, MDC3
   STORE 0 TO MLAST_PRIC, MREO_LEV, MREO_QTY, MC1, MC2, MC3
   STORE DATE() TO MDATE
   SELECT 1
   @ 23, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 3, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 3, 10 SAY 'Item     '
   @ 3, 25 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 73
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 3, 25 SAY MP1 PICTURE '99'
   @ 3, 27 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 3, 28 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 3, 28 SAY MP2 PICTURE '99'
   @ 3, 30 SAY '-'
   @ 3, 31 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 3, 31 SAY MP3 PICTURE '9999'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 73
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
      WAIT ''
      @ 23, 19 SAY SPACE(43)
      LOOP
   ENDIF
   IF MI3=0
      @ 8, 1 CLEAR TO 23, 77
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
   ENDIF
   @ 3, 40 GET ITEM
   READ
   IF MI3<>0
      @ 5, 10 SAY 'Unit          ' GET UNIT FUNCTION '!'
      @ 7, 10 SAY 'Reording Level' GET REORD_LEV PICTURE '9999999'
      @ 9, 10 SAY 'Reording Qty  ' GET REORD_QTY PICTURE '9999999'
      READ
      @ 7, 42 SAY 'Minimum Level.      ' GET MIN_LEV PICTURE '9999999'
      READ
      MT= 0
      DO WHILE MT=0
         @ 9, 42 SAY 'Maximum Level.      ' GET MAX_LEV PICTURE '9999999'
         READ
         IF MMAX_LEV<MMIN_LEV
            @ 23, 15 SAY 'Maximum Level Is Less Than Minimum Level --- Please Retry'
            WAIT ''
            @ 23, 14 SAY SPACE(58)
            LOOP
         ENDIF
         MT= 1
      ENDDO
      MT= 0
      DO WHILE MT=0
         MC1= C1
         MC2= C2
         MC3= C3
         @ 11, 10 SAY 'Last Purchased From (Party Code)    '

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 11, 45 GET MC1 PICTURE '99'
         @ 11, 47 SAY '-'
         @ 11, 50 SAY '-'

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ 11, 48 GET MC2 PICTURE '@z 99'
         @ 11, 51 GET MC3 PICTURE '@z 999'
         READ
         IF MC1=0.and.MC2=0.and.MC3=0
            EXIT
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 11, 45 SAY MPC1
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 11, 48 SAY MPC2
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 11, 51 SAY MPC3

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 2
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF .not. FOUND()
            @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
            WAIT ''
            @ 23, 19 SAY SPACE(46)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 13, 45 SAY TITLE

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 1
         @ 15, 10 SAY 'Last Purchase Price'
         @ 15, 45 GET LAST_PRICE PICTURE '9999999.99'
         READ
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         MT= 1
      ENDDO
      IF INT=1
         R= 16
         @ R, 1 TO R, 78
         R= R+1
         @ R, 5 SAY 'Credit Code (For Sale Transaction) '
         R= R+1
         MT= 0
         DO WHILE MT=0
            SELECT 1
            MCC1= CC1
            MCC2= CC2
            MCC3= CC3

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 16 SAY '  -  -   '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 16 GET MCC1 PICTURE '@z 99'

IF _DOS
            SET COLOR TO /w
ENDIF

            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MCC1))))+LTRIM(STR(MCC1))
            @ R, 16 SAY MPC1
            @ R, 19 GET MCC2 PICTURE '@z 99'
            READ
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MCC2))))+LTRIM(STR(MCC2))
            @ R, 19 SAY MPC2
            @ R, 22 GET MCC3 PICTURE '@z 999'
            READ
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MCC3))))+LTRIM(STR(MCC3))
            @ R, 22 SAY MPC3

IF _DOS
            SET COLOR TO 7/1
ENDIF

            IF MCC1=0.and.MCC2=0.and.MCC3=0
               EXIT
            ENDIF
            SELECT 2
            STORE STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3) TO MCCODE
            SEEK MCCODE
            IF .not. FOUND()
               @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 18 CLEAR TO 23, 63
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 29 SAY TITLE

IF _DOS
            SET COLOR TO 7/1
ENDIF

            MT= 1
         ENDDO
         R= R+2
         @ R, 5 SAY 'Debit Code (For Purchase Transaction) '
         R= R+1
         MT= 0
         DO WHILE MT=0
            SELECT 1
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 16 SAY '  -  -   '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 16 GET MDC1 PICTURE '@z 99'

IF _DOS
            SET COLOR TO /w
ENDIF

            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MDC1))))+LTRIM(STR(MDC1))
            @ R, 16 SAY MPC1
            @ R, 19 GET MDC2 PICTURE '@z 99'
            READ
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MDC2))))+LTRIM(STR(MDC2))
            @ R, 19 SAY MPC2
            @ R, 22 GET MDC3 PICTURE '@z 999'
            READ
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MDC3))))+LTRIM(STR(MDC3))
            @ R, 22 SAY MPC3

IF _DOS
            SET COLOR TO 7/1
ENDIF

            IF MDC1=0.and.MDC2=0.and.MDC3=0
               EXIT
            ENDIF
            SELECT 2
            STORE STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3) TO MDCODE
            SEEK MDCODE
            IF .not. FOUND()
               @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 18 CLEAR TO 23, 63
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 29 SAY TITLE

IF _DOS
            SET COLOR TO 7/1
ENDIF

            MT= 1
         ENDDO
         SELECT 1
         IF .not. RLOCK()
            rlock() 
         ENDIF
         REPLACE CC1 WITH MCC1, CC2 WITH MCC2, CC3 WITH MCC3
         REPLACE DC1 WITH MDC1, DC2 WITH MDC2, DC3 WITH MDC3
         UNLOCK
      ENDIF
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 25 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE ITEM_DEL
******************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE item INDEX item-idx
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE item-bal INDEX itmd-idx
SELECT 4
USE stock INDEX itemidx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 3, 30 SAY 'Item Codes Deletion '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MITEM= SPACE(30)
   MUNIT= SPACE(6)
   STORE SPACE(5) TO MI_CODE2
   STORE 0 TO MI1, MI2, MI3
   STORE 0 TO MMIN_LEV, MMAX_LEV, MS_U_PRICE
   STORE 0 TO MLAST_PRIC, MREO_LEV, MREO_QTY, MC1, MC2, MC3
   STORE DATE() TO MDATE
   SELECT 1
   @ 23, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 6, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 6, 10 SAY 'Item     '
   @ 6, 25 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 73
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 6, 25 SAY MP1 PICTURE '99'
   @ 6, 27 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 6, 28 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 6, 28 SAY MP2 PICTURE '99'
   @ 6, 30 SAY '-'
   @ 6, 31 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 6, 31 SAY MP3 PICTURE '9999'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 73
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   SELECT 1
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
      WAIT ''
      @ 23, 19 SAY SPACE(43)
      LOOP
   ENDIF
   IF MI3=0
      @ 8, 1 CLEAR TO 23, 77
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
   ENDIF
   IF MI3<>0
      SELECT 3
      SEEK MITEM_CODE
      IF FOUND()
         MSR_NO= SR_NO
         IF MSR_NO<>1
            @ 12, 20 SAY 'Transactions already exist  for  this Item at  '
            @ 13, 20 SAY 'this  location.  Therefore, you cannot Delete  '
            @ 14, 20 SAY 'its " Opening Balance " for this location. If  '
            @ 15, 20 SAY 'you still want to delete the  Opening Balance  '
            @ 16, 20 SAY 'of this item please use UN-POST Option first.  '
            @ 17, 20 SAY 'Press any key to return ..... '
            WAIT ''
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
      ENDIF
   ENDIF
   IF MI3<>0
      SELECT 4
      SEEK MITEM_CODE
      IF FOUND()
         MSR_NO= SR_NO
         IF MSR_NO<>1
            @ 12, 20 SAY 'Transactions already exist  for  this Item at  '
            @ 13, 20 SAY 'this  location.  Therefore, you cannot Delete  '
            @ 14, 20 SAY 'its " Opening Balance " for this location. If  '
            @ 15, 20 SAY 'you still want to delete the  Opening Balance  '
            @ 16, 20 SAY 'of this item please use UN-POST Option first.  '
            @ 17, 20 SAY 'Press any key to return ..... '
            WAIT ''
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
      ENDIF
   ENDIF
   SELECT 1

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 6, 40 SAY ITEM

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MI3<>0
      @ 8, 10 SAY 'Unit          '
      @ 10, 10 SAY 'Reording Level'
      @ 12, 10 SAY 'Reording Qty  '
      @ 10, 42 SAY 'Minimum Level.      '
      @ 12, 42 SAY 'Maximum Level.      '

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 8, 25 SAY UNIT FUNCTION '!'
      @ 10, 25 SAY REORD_LEV PICTURE '9999999'
      @ 12, 25 SAY REORD_QTY PICTURE '9999999'
      @ 10, 62 SAY MIN_LEV PICTURE '9999999'
      @ 12, 62 SAY MAX_LEV PICTURE '9999999'

IF _DOS
      SET COLOR TO 7/1
ENDIF

      MT= 0
      DO WHILE MT=0
         MC1= C1
         MC2= C2
         MC3= C3
         @ 14, 10 SAY 'Last Purchased From (Party Code)    '

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 14, 45 SAY MC1 PICTURE '99'
         @ 14, 47 SAY '-'
         @ 14, 50 SAY '-'
         @ 14, 48 SAY MC2 PICTURE '@z 99'
         @ 14, 51 SAY MC3 PICTURE '@z 999'

IF _DOS
         SET COLOR TO 7/1
ENDIF

         IF MC1=0.and.MC2=0.and.MC3=0
            EXIT
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 14, 45 SAY MPC1
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 14, 48 SAY MPC2
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 14, 51 SAY MPC3

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 2
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF .not. FOUND()
            @ 21, 20 SAY 'This Code Does Not Exists ... Please Re-try'
            WAIT ''
            @ 21, 19 SAY SPACE(46)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 16, 45 SAY TITLE

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 1
         @ 18, 10 SAY 'Last Purchase Price'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 18, 45 SAY LAST_PRICE PICTURE '9999999.99'

IF _DOS
         SET COLOR TO 7/1
ENDIF

         MT= 1
      ENDDO
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 30 SAY ' Are You Sure - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   @ 23, 29 CLEAR TO 23, 66
   IF P='Y'
      IF MI3=0
         SELECT 1
         DELETE
      ELSE
         SELECT 1
         DELETE
         SELECT 3
         DELETE
         SELECT 4
         DELETE
      ENDIF
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 30 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE ITEM_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
USE item INDEX item-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
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
HEADING= .T.
STORE 0 TO CNT, PAGE
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'ITEM  CODES '
         @ 4, 32 SAY '----------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'ITEM  CODES '
         L= L+1
         @ L, 30 SAY '-----------  '
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 23 SAY 'Item'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      HEADING= .F.
   ENDIF
   IF I3=0
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 5 SAY MPC1 PICTURE '99'
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 8 SAY MPC2 PICTURE '99'
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 11 SAY MPC3 PICTURE '9999'
      @ L, 23 SAY ITEM
      MLEN= LEN(RTRIM(ITEM))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
   ENDIF
   IF I3<>0
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 8 SAY MPC1 PICTURE '99'
      @ L, 10 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 11 SAY MPC2 PICTURE '99'
      @ L, 13 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 14 SAY MPC3 PICTURE '9999'
      @ L, 23 SAY ITEM
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+1
   IF P='S'.and.L>=19
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
         CLOSE ALL
         CLEAR ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
******
******************
PROCEDURE ITEMPRT1
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='P'
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.
PAGE= 0
MTITLE= SPACE(30)
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      DO top-prt
      @ 3, 15 SAY CHR(18)
      @ 3, 33 SAY 'COMPLETE ITEM CODES REPORT'
      L= L+1
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY ' Code'
      @ L, 14 SAY 'Item'
      @ L, 45 SAY ' Unit'
      @ L, 58 SAY 'Reord-Lev'
      @ L, 70 SAY 'Reord-Qty'
      @ L, 82 SAY 'Min-Level'
      @ L, 94 SAY 'Max-Level'
      @ L, 112 SAY 'Last-Price'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   IF I3=0
      SKIP
   ENDIF
   IF I3<>0
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 2 SAY MPC1 PICTURE '99'
      @ L, 4 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 5 SAY MPC2 PICTURE '99'
      @ L, 7 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 8 SAY MPC3 PICTURE '9999'
      @ L, 14 SAY ITEM
      @ L, 46 SAY UNIT
      @ L, 60 SAY REORD_LEV
      @ L, 72 SAY REORD_QTY
      @ L, 84 SAY MIN_LEV
      @ L, 94 SAY MAX_LEV
      @ L, 112 SAY LAST_PRICE
      L= L+1
   ENDIF
   SKIP
   IF P='P'.and.L>56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
******
******************
PROCEDURE ITEMPRT2
******************
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE ldgrcode INDEX ac1
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'ITEM  CODES '
         @ 4, 32 SAY '----------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'ITEM  CODES '
         L= L+1
         @ L, 30 SAY '-----------                     Page - '
         @ L, 70 SAY PAGE PICTURE '999'
      ENDIF
      L= L+1
      @ L, 1 SAY REPLICATE('-',72)
      L= L+1
      @ L, 2 SAY 'Code'
      @ L, 28 SAY 'Item'
      L= L+1
      @ L, 1 SAY REPLICATE('-',72)
      L= L+1
      HEADING= .F.
   ENDIF
   IF I3=0
      SKIP
      LOOP
   ENDIF
   IF I3<>0
      SELECT 1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 2 SAY MPC1 PICTURE '99'
      @ L, 4 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 5 SAY MPC2 PICTURE '99'
      @ L, 7 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 8 SAY MPC3 PICTURE '9999'
      @ L, 28 SAY ITEM
      MDC1= DC1
      MDC2= DC2
      MDC3= DC3
      MCC1= CC1
      MCC2= CC2
      MCC3= CC3
      L= L+1
      @ L, 10 SAY 'DR : '
      MPD1= REPLICATE('0',2-LEN(LTRIM(STR(MDC1))))+LTRIM(STR(MDC1))
      MPD2= REPLICATE('0',2-LEN(LTRIM(STR(MDC2))))+LTRIM(STR(MDC2))
      MPD3= REPLICATE('0',3-LEN(LTRIM(STR(MDC3))))+LTRIM(STR(MDC3))
      @ L, 15 SAY MPD1
      @ L, 17 SAY '-'
      @ L, 18 SAY MPD2
      @ L, 20 SAY '-'
      @ L, 21 SAY MPD3
      SELECT 2
      STORE STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3) TO MDCODE
      SEEK MDCODE
      IF FOUND()
         @ L, 28 SAY TITLE
      ENDIF
      L= L+1
      SELECT 1
      @ L, 10 SAY 'CR : '
      MPCC1= REPLICATE('0',2-LEN(LTRIM(STR(MCC1))))+LTRIM(STR(MCC1))
      MPCC2= REPLICATE('0',2-LEN(LTRIM(STR(MCC2))))+LTRIM(STR(MCC2))
      MPCC3= REPLICATE('0',3-LEN(LTRIM(STR(MCC3))))+LTRIM(STR(MCC3))
      @ L, 15 SAY MPCC1
      @ L, 17 SAY '-'
      @ L, 18 SAY MPCC2
      @ L, 20 SAY '-'
      @ L, 21 SAY MPCC3
      SELECT 2
      STORE STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3) TO MCCODE
      SEEK MCCODE
      IF FOUND()
         @ L, 28 SAY TITLE
      ENDIF
   ENDIF
   SELECT 1
   L= L+2
   SKIP
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
*******

******************
PROCEDURE ITEMPRT3
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
USE item INDEX item-idx
STORE 0 TO MI1, MI2
@ 5, 10 SAY 'Item Codes Printing '
@ 10, 10 SAY 'Please Enter First Two Level Of Item Codes '
@ 12, 10 SAY 'Item Code '

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 22 SAY '-'

IF _DOS
SET COLOR TO 7/1
ENDIF

@ 12, 20 GET MI1 PICTURE '99'
@ 12, 23 GET MI2 PICTURE '99'
READ
SET FILTER TO I1=MI1.and.I2=MI2
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'ITEM  CODES '
         @ 4, 32 SAY '----------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'ITEM  CODES '
         L= L+1
         @ L, 30 SAY '-----------                     Page - '
         @ L, 70 SAY PAGE PICTURE '999'
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 23 SAY 'Item'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      HEADING= .F.
   ENDIF
   IF I3=0
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 5 SAY MPC1 PICTURE '99'
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 8 SAY MPC2 PICTURE '99'
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 11 SAY MPC3 PICTURE '9999'
      @ L, 23 SAY ITEM
      MLEN= LEN(RTRIM(ITEM))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
      L= L+1
   ENDIF
   IF I3<>0
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 8 SAY MPC1 PICTURE '99'
      @ L, 10 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 11 SAY MPC2 PICTURE '99'
      @ L, 13 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 14 SAY MPC3 PICTURE '9999'
      @ L, 23 SAY ITEM
      L= L+1
   ENDIF
   SKIP
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
******


*****************
PROCEDURE LOC_MEN
*****************
DO WHILE .T.
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY 'Location Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Location Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Location Code .... Correction '
   @ L+4, M PROMPT ' 3 - Location Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Location Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu       '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
      CASE MCHOICE=4
         CLEAR
         DO loc_prt
      CASE MCHOICE=3
         CLEAR
         DO loc_del
      CASE MCHOICE=2
         CLEAR
         DO loc_cor
      CASE MCHOICE=1
         CLEAR
         DO loc_ent
   ENDCASE
ENDDO
RETURN
******
*****************
PROCEDURE LOC_ENT
*****************
CLEAR
CLOSE ALL
USE loc INDEX loc-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 30 SAY 'Location Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   STORE 0 TO MLOC_CODE, CNT
   STORE SPACE(25) TO MLOC
   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
      @ 9, 36 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SEEK MLOC_CODE
      @ 23, 40 CLEAR TO 23, 63
      IF CNT=0
         IF FOUND()
            @ 23, 20 SAY 'This Code Already Exist ... Please Retry'
            WAIT ''
            @ 23, 19 SAY SPACE(40)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE LOC_CODE WITH MLOC_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 11, 20 SAY 'Location       ' GET MLOC
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY 'Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE LOC WITH MLOC
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
******
*****************
PROCEDURE LOC_COR
******************
CLEAR
CLOSE ALL
USE loc INDEX loc-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Location Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MLOC_CODE= 0
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 63
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Location       ' GET LOC
   READ
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******
*****************
PROCEDURE LOC_DEL
*****************
CLEAR
CLOSE ALL
SET DELETED ON
USE loc INDEX loc-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Location Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MLOC_CODE= 0
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 63
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Location     '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 36 SAY LOC

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 30 SAY ' Are You Sure Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   @ 23, 20 CLEAR TO 23, 78
   IF MYES='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*************

******************
PROCEDURE LOC_PRT
******************
CLEAR
CLOSE ALL

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE loc INDEX loc-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 1
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY ' LOCATION      CODES'
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ L, 22 SAY 'L O C A T I O N       C O D E S'
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 25 SAY 'Code '
      @ L, 40 SAY 'Location '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 25 SAY LOC_CODE
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
   @ L, 25 SAY MPC1
   @ L, 40 SAY LOC
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
****************


******************
PROCEDURE DEPT_MEN
******************
DO WHILE .T.
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY 'Department Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Department Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Department Code .... Correction '
   @ L+4, M PROMPT ' 3 - Department Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Department Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
      CASE MCHOICE=4
         CLEAR
         DO dept_prt
      CASE MCHOICE=3
         CLEAR
         DO dept_del
      CASE MCHOICE=2
         CLEAR
         DO dept_cor
      CASE MCHOICE=1
         CLEAR
         DO dept_ent
   ENDCASE
ENDDO
RETURN
******

******************
PROCEDURE DEPT_ENT
******************
CLEAR
CLOSE ALL
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Department Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   STORE 0 TO MDEPT_CODE, CNT
   STORE SPACE(30) TO MDEPT
   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 9, 20 SAY 'Department Code  ' GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ 9, 38 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDEPT_CODE=0
         LOOP
      ENDIF
      SEEK MDEPT_CODE
      @ 23, 40 CLEAR TO 23, 73
      IF CNT=0
         IF FOUND()
            @ 23, 20 SAY 'This Code Already Exist ---Please Retry'
            WAIT ''
            @ 23, 19 SAY SPACE(40)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DEPT_CODE WITH MDEPT_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 11, 20 SAY 'Department       ' GET MDEPT
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 75
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DEPT WITH MDEPT
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******

******************
PROCEDURE DEPT_COR
******************
CLEAR
CLOSE ALL
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   MDEPT_CODE= 0

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Department Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Department Code  ' GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   SEEK MDEPT_CODE
   @ 23, 40 CLEAR TO 23, 73
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department       ' GET DEPT
   READ
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE DEPT_DEL
******************
CLEAR
CLOSE ALL
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   MDEPT_CODE= 0

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Department Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Department Code  ' GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   SEEK MDEPT_CODE
   @ 23, 40 CLEAR TO 23, 73
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 38 SAY DEPT

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 25 SAY ' Are You Sure - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   @ 23, 20 CLEAR TO 23, 75
   IF MYES='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 23 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE DEPT_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
USE dept INDEX dept-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 1
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY 'DEPARTMENT     CODES'
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ L, 22 SAY 'D E P A R T M E N T     C O D E S'
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 25 SAY 'Code '
      @ L, 40 SAY 'Department '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 25 SAY DEPT_CODE PICTURE '999'
   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
   @ L, 25 SAY MPC1
   @ L, 40 SAY DEPT
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN
*******

*******************
PROCEDURE MACH_MEN
*******************
DO WHILE .T.
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY ' Machine Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Machine Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Machine Code .... Correction '
   @ L+4, M PROMPT ' 3 - Machine Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Machine Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu      '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
      CASE MCHOICE=4
         CLEAR
         DO mach_prt
      CASE MCHOICE=3
         CLEAR
         DO mach_del
      CASE MCHOICE=2
         CLEAR
         DO mach_cor
      CASE MCHOICE=1
         CLEAR
         DO mach_ent
   ENDCASE
ENDDO
RETURN
*******

******************
PROCEDURE MACH_ENT
******************
CLEAR
CLOSE ALL
SELECT 1
USE mach INDEX mach-idx
SELECT 2
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE
   STORE SPACE(25) TO MMACH

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 9, 34 SAY '   -    '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 9, 20 SAY 'Machine Code '
      @ 9, 34 GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ 9, 34 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDEPT_CODE=0
         LOOP
      ENDIF
      @ 23, 40 CLEAR TO 23, 73
      SELECT 2
      SEEK MDEPT_CODE
      IF .not. FOUND()
         @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
         WAIT ''
         @ 23, 19 SAY SPACE(60)
         LOOP
      ENDIF
      @ 11, 20 SAY 'Department  '

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 11, 34 SAY DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 9, 38 GET MMACH_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
      @ 9, 38 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF MMACH_CODE=0
         LOOP
      ENDIF
      SELECT 1
      STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
      SEEK XYZ
      IF FOUND()
         @ 23, 20 SAY 'This Machine Code Already Exist ---Please Retry'
         WAIT ''
         @ 23, 19 SAY SPACE(60)
         LOOP
      ENDIF
      @ 13, 20 SAY 'Machine      '
      @ 13, 34 GET MMACH
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 73
      IF MYES='Y'
         LOOP
      ENDIF
      SELECT 1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE MACH_CODE WITH MMACH_CODE, MACH WITH MMACH, DEPT_CODE WITH MDEPT_CODE
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******

******************
PROCEDURE MACH_COR
******************
CLEAR
CLOSE ALL
SELECT 1
USE mach INDEX mach-idx
SELECT 2
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '   -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 20 SAY 'Machine Code '
   @ 9, 34 GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 73
   SELECT 2
   SEEK MDEPT_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY DEPT

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MMACH_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MMACH_CODE=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
   SEEK XYZ
   IF .not. FOUND()
      @ 23, 20 SAY 'This Machine Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 13, 20 SAY 'Machine      ' GET MACH
   READ
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******

******************
PROCEDURE MACH_DEL
******************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE mach INDEX mach-idx
SELECT 2
USE dept INDEX dept-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '   -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 20 SAY 'Machine Code '
   @ 9, 34 GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 73
   SELECT 2
   SEEK MDEPT_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY DEPT

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MMACH_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MMACH_CODE=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
   SEEK XYZ
   IF .not. FOUND()
      @ 23, 20 SAY 'This Machine Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 13, 20 SAY 'Machine      '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 13, 34 SAY MACH

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' Are You Sure - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   @ 23, 27 CLEAR TO 23, 54
   IF MYES='Y'
      SELECT 1
      DELETE
   ENDIF
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Deletion - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE MACH_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
SELECT 1
USE mach INDEX mach-idx
SELECT 2
USE dept INDEX dept-idx
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      L= 1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY 'MACHINE    CODES'
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ L, 22 SAY 'M A C H I N E     C O D E S'
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 5 SAY '   Code '
      @ L, 22 SAY 'Department'
      @ L, 54 SAY 'Machine'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 8 SAY DEPT_CODE PICTURE '999'
   @ L, 11 SAY '-'
   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
   @ L, 8 SAY MPC1
   SELECT 1
   @ L, 12 SAY MACH_CODE PICTURE '9999'
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MACH_CODE))))+LTRIM(STR(MACH_CODE))
   @ L, 12 SAY MPC1
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 22 SAY DEPT
   ENDIF
   SELECT 1
   @ L, 54 SAY MACH
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN
*******

******************
PROCEDURE ZONE_MEN
******************
DO WHILE .T.
   L= 8
   M= 25

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY '   Zone Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Zone Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Zone Code .... Correction '
   @ L+4, M PROMPT ' 3 - Zone Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Zone Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu   '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         CLEAR
         DO zone_ent
      CASE MCHOICE=2
         CLEAR
         DO zone_cor
      CASE MCHOICE=3
         CLEAR
         DO zone_del
      CASE MCHOICE=4
         CLEAR
         DO zone_prt
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
**********

******************
PROCEDURE ZONE_ENT
******************
CLEAR
CLOSE ALL
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MZONE_CODE, CNT
   STORE SPACE(20) TO MZONE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Zone Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 9, 20 SAY 'Zone Code ' GET MZONE_CODE PICTURE '99'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
      @ 9, 31 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 40 CLEAR TO 23, 64
      IF MZONE_CODE=0
         LOOP
      ENDIF
      SEEK MZONE_CODE
      IF CNT=0
         IF FOUND()
            @ 23, 18 SAY 'This Code Already Exist -- Press any key'
            WAIT ''
            @ 23, 17 CLEAR TO 23, 73
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE ZONE_CODE WITH MZONE_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 11, 20 SAY 'Zone      ' GET MZONE
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE ZONE WITH MZONE
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
******

******************
PROCEDURE ZONE_COR
*******************
CLEAR
CLOSE ALL
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MZONE_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Zone Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Zone Code ' GET MZONE_CODE PICTURE '99'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
   @ 9, 31 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 40 CLEAR TO 23, 64
   IF MZONE_CODE=0
      LOOP
   ENDIF
   SEEK MZONE_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 11, 20 SAY 'Zone      ' GET ZONE
   READ
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE ZONE_DEL
******************
CLEAR
CLOSE ALL
SET DELETED ON
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MZONE_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Zone Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Zone Code ' GET MZONE_CODE PICTURE '99'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
   @ 9, 31 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 40 CLEAR TO 23, 64
   IF MZONE_CODE=0
      LOOP
   ENDIF
   SEEK MZONE_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 11, 20 SAY 'Zone      '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 30 SAY ZONE

IF _DOS
   SET COLOR TO 7/1
ENDIF

   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' Are you Sure - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   @ 23, 25 CLEAR TO 23, 64
   IF P='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 25 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE ZONE_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
USE zone INDEX zone-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 1
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 20 SAY 'Z O N E   C O D E S '
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ 3, 28 SAY ' Z O N E   C O D E S  '
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 25 SAY ' Code  '
      @ L, 40 SAY ' Zone   '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 25 SAY ZONE_CODE PICTURE '99'
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(ZONE_CODE))))+LTRIM(STR(ZONE_CODE))
   @ L, 25 SAY MPC1
   @ L, 40 SAY ZONE
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      L= 1
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN
******

******************
PROCEDURE CITY_MEN
******************
DO WHILE .T.
   L= 8
   M= 25

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY '   City Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - City Code .... Entry      '
   @ L+2, M PROMPT ' 2 - City Code .... Correction '
   @ L+4, M PROMPT ' 3 - City Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - City Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu   '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         CLEAR
         DO city_ent
      CASE MCHOICE=2
         CLEAR
         DO city_cor
      CASE MCHOICE=3
         CLEAR
         DO city_del
      CASE MCHOICE=4
         CLEAR
         DO city_prt
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
******

******************
PROCEDURE CITY_ENT
******************
CLEAR
CLOSE ALL
SELECT 1
USE city INDEX city-idx
SELECT 2
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MCITY_CODE, MZONE_CODE, CNT
   STORE SPACE(20) TO MCITY

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'City Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 9, 34 SAY '  -   '

IF _DOS
      SET COLOR TO 
ENDIF

      @ 9, 20 SAY 'City Code    '
      @ 9, 34 GET MZONE_CODE PICTURE '99'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
      @ 9, 34 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 40 CLEAR TO 23, 64
      IF MZONE_CODE=0
         LOOP
      ENDIF
      SELECT 2
      SEEK MZONE_CODE
      IF .not. FOUND()
         @ 23, 18 SAY 'This Zone Code Does Not Exist -- Press any key'
         WAIT ''
         @ 23, 17 CLEAR TO 23, 73
         LOOP
      ENDIF
      @ 11, 20 SAY 'Zone'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 11, 34 SAY ZONE

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 9, 38 GET MCITY_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
      @ 9, 38 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF MCITY_CODE=0
         LOOP
      ENDIF
      IF CNT=0
         SELECT 1
         SEEK MCITY_CODE
         IF FOUND()
            @ 23, 18 SAY 'This City Code Already Exist -- Press any key'
            WAIT ''
            @ 23, 17 CLEAR TO 23, 73
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE CITY_CODE WITH MCITY_CODE
         REPLACE ZONE_CODE WITH MZONE_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 13, 20 SAY 'City'
      @ 13, 34 GET MCITY
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Corrections - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE CITY WITH MCITY
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
******

*******************
PROCEDURE CITY_COR
*******************
CLEAR
CLOSE ALL
SELECT 1
USE city INDEX city-idx
SELECT 2
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MCITY_CODE, MZONE_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'City Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '  -   '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 9, 20 SAY 'City Code    '
   @ 9, 34 GET MZONE_CODE PICTURE '99'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 40 CLEAR TO 23, 64
   IF MZONE_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MZONE_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This Zone Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 73
      LOOP
   ENDIF
   @ 11, 20 SAY 'Zone'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY ZONE

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MCITY_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MCITY_CODE=0
      LOOP
   ENDIF
   SELECT 1
   SEEK MCITY_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This City Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 73
      LOOP
   ENDIF
   @ 13, 20 SAY 'City'
   @ 13, 34 GET CITY
   READ
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******
*******************
PROCEDURE CITY_DEL
*******************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE city INDEX city-idx
SELECT 2
USE zone INDEX zone-idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MCITY_CODE, MZONE_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'City Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '  -   '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 9, 20 SAY 'City Code    '
   @ 9, 34 GET MZONE_CODE PICTURE '99'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MZONE_CODE))))+LTRIM(STR(MZONE_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 40 CLEAR TO 23, 64
   IF MZONE_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MZONE_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This Zone Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 73
      LOOP
   ENDIF
   @ 11, 20 SAY 'Zone'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY ZONE

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MCITY_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MCITY_CODE=0
      LOOP
   ENDIF
   SELECT 1
   SEEK MCITY_CODE
   IF .not. FOUND()
      @ 23, 18 SAY 'This City Code Does Not Exist -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 73
      LOOP
   ENDIF
   @ 13, 20 SAY 'City'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 13, 34 SAY CITY

IF _DOS
   SET COLOR TO 7/1
ENDIF

   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' Are you Sure - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   @ 23, 25 CLEAR TO 23, 64
   IF P='Y'
      SELECT 1
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 25 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE CITY_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
SELECT 1
USE city INDEX city-idx
SELECT 2
USE zone INDEX zone-idx
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
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
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 1
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 20 SAY 'C I T Y    C O D E S '
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ 3, 28 SAY ' C I T Y    C O D E S  '
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 5 SAY '   Code  '
      @ L, 26 SAY ' Zone    '
      @ L, 54 SAY ' City    '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 8 SAY ZONE_CODE PICTURE '99'
   @ L, 10 SAY '-'
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(ZONE_CODE))))+LTRIM(STR(ZONE_CODE))
   @ L, 8 SAY MPC1
   SELECT 1
   @ L, 11 SAY CITY_CODE PICTURE '999'
   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(CITY_CODE))))+LTRIM(STR(CITY_CODE))
   @ L, 11 SAY MPC1
   SELECT 2
   LOCATE FOR ZONE_CODE= A->ZONE_CODE
   IF FOUND()
      @ L, 26 SAY ZONE
   ENDIF
   SELECT 1
   @ L, 54 SAY CITY
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN
*******

*******************
PROCEDURE DEBT_MEN
*******************
DO WHILE .T.
   L= 8
   M= 25

IF _DOS

IF _DOS
   SET COLOR TO 7/1
ENDIF

ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS

IF _DOS
   SET COLOR TO +7/1
ENDIF

ENDIF

   @ 6, 26 SAY '  Debtors Code File Maintenance'

IF _DOS

IF _DOS
   SET COLOR TO 7/1 
ENDIF

ENDIF

   @ L+0, M PROMPT ' 1 - Debtors Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Debtors Code .... Correction '
   @ L+4, M PROMPT ' 3 - Debtors Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Debtors Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu      '

   MCHOICE= 0

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN

      CASE MCHOICE=4
         CLEAR
         DO debt_prt

      CASE MCHOICE=3
         CLEAR
         DO debt_del

      CASE MCHOICE=2
         CLEAR
         DO debt_cor

      CASE MCHOICE=1
         CLEAR
         DO debt_ent

   ENDCASE

ENDDO

RETURN
*********

******************
PROCEDURE DEBT_ENT
******************
CLEAR

IF _DOS
SET COLOR TO 7/1 
ENDIF


IF _DOS
SET COLOR TO +* w
ENDIF

@ 5, 27 SAY 'W A R N I N G '
@ 6, 27 SAY '------------- '

IF _DOS
SET COLOR TO 7/1  
ENDIF

@ 8, 7 SAY 'If you are using this Inventory Program Integrated with Accounts '
@ 9, 7 SAY "Then create Creditors/Debtors Codes using ' Ledger Heads Maintenance '"
@ 10, 7 SAY 'option in Accounts and DO NOT USE  this option. In case you are using  '
@ 11, 7 SAY 'this Inventory Program independently, please proceed with coding thru'
@ 12, 7 SAY 'this option .......'
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 14, 7 SAY 'Do you want to continue with this option ? - Y/N  ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='N'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
USE ldgrcode INDEX ac1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3, CNT
   STORE SPACE(30) TO MTITLE
   STORE SPACE(25) TO MADD1, MADD2
   STORE SPACE(45) TO MREMARKS

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 5, 25 SAY 'Debtors Codes Entry'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 8, 20 SAY 'Debtors Code  '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 8, 35 GET MC1 PICTURE '99'
      @ 8, 37 SAY '-'
      @ 8, 40 SAY '-'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 8, 38 GET MC2 PICTURE '@z 99'
      @ 8, 41 GET MC3 PICTURE '@z 999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 8, 38 SAY MPC2
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 8, 41 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 18 CLEAR TO 23, 63
      IF CNT=0
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF FOUND()
            @ 23, 18 SAY 'This Code Already Exist  -- Press any key'
            WAIT ''
            @ 23, 17 CLEAR TO 23, 63
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         UNLOCK
         CNT= 1
      ENDIF
      @ 10, 20 SAY 'Debtors      ' GET MTITLE PICTURE '@!'
      READ
      @ 12, 20 SAY 'Address      ' GET MADD1
      @ 14, 20 SAY '             ' GET MADD2
      READ
      @ 16, 20 SAY 'R e m a r k s '
      @ 18, 20 GET MREMARKS
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE TITLE WITH MTITLE
      REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2, REMARKS WITH MREMARKS
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='Y'
         EXIT
      ENDIF
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDDO
ENDDO
RETURN

PROCEDURE DEBT-COR
CLEAR
CLOSE ALL
USE ldgrcode INDEX ac1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 5, 25 SAY 'Debtors Codes Correction'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 8, 20 SAY 'Debtors Code  '

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 35 GET MC1 PICTURE '99'
   @ 8, 37 SAY '-'
   @ 8, 40 SAY '-'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 38 GET MC2 PICTURE '@z 99'
   @ 8, 41 GET MC3 PICTURE '@z 999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 38 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 41 SAY MPC3

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 23, 18 CLEAR TO 23, 63
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist  -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 10, 20 SAY 'Debtors      ' GET TITLE PICTURE '@!'
   @ 12, 20 SAY 'Address      ' GET ADDRESS1
   @ 14, 20 SAY '             ' GET ADDRESS2
   @ 16, 20 SAY 'R e m a r k s '
   @ 18, 20 GET REMARKS
   READ
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Correction Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE DEBT-DEL
CLEAR
CLOSE ALL
SET DELETED ON
USE ldgrcode INDEX ac1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 5, 25 SAY 'Debtors Codes Deletion'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 8, 20 SAY 'Debtors Code  '

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 35 GET MC1 PICTURE '99'
   @ 8, 37 SAY '-'
   @ 8, 40 SAY '-'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 38 GET MC2 PICTURE '@z 99'
   @ 8, 41 GET MC3 PICTURE '@z 999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 38 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 41 SAY MPC3

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 23, 18 CLEAR TO 23, 63
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist  -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 10, 20 SAY 'Debtors      '
   @ 12, 20 SAY 'Address      '
   @ 16, 20 SAY 'R e m a r k s '

IF _DOS
   SET COLOR TO /w     
ENDIF

   @ 10, 35 SAY TITLE PICTURE '@!'
   @ 12, 35 SAY ADDRESS1
   @ 14, 35 SAY ADDRESS2
   @ 18, 20 SAY REMARKS

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 25 SAY ' Are You Sure Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 20 CLEAR TO 23, 55
      @ 23, 23 SAY ' More Data Deletion Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE DEBT-PRT

IF _DOS
SET COLOR TO 7/1 
ENDIF

CLEAR
CLOSE ALL
USE ldgrcode INDEX ac1
MC1= 0
DO WHILE .T.

IF _DOS
   SET COLOR TO w+
ENDIF

   @ 10, 14 SAY 'Please give "First Level Code" of Debtors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   READ
   IF MC1=0
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
GOTO TOP
CLEAR
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   ABC= ' '
   DO WHILE .not. ABC$'CQ'
      @ 12, 25 SAY 'Continue/Quit' GET ABC FUNCTION '!'
      READ
   ENDDO
   IF ABC='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 1
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY '  Debtors  Codes  '
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ 3, 25 SAY '    Debtors    Codes  '
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 6 SAY 'Code  '
      @ L, 25 SAY 'Debtors '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
      L= L+1
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
   @ L, 6 SAY MPC1 PICTURE '99'
   @ L, 8 SAY '-'
   @ L, 9 SAY MPC2 PICTURE '99'
   @ L, 11 SAY '-'
   @ L, 12 SAY MPC3 PICTURE '999'
   @ L, 25 SAY TITLE
   L= L+1
   @ L, 25 SAY ADDRESS1
   L= L+1
   @ L, 25 SAY ADDRESS2
   SKIP
   L= L+2
   IF EOF().and.L>20
      EXIT
   ENDIF
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         L= 1
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
      L= 1
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN


*******************
PROCEDURE CRED_MEN
*******************
DO WHILE .T.

   L= 8
   M= 25

IF _DOS

IF _DOS
   SET COLOR TO 7/1 
ENDIF

ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS

IF _DOS
   SET COLOR TO +7/1 
ENDIF

ENDIF

   @ 6, 26 SAY '  Creditors Code File Maintenance'

IF _DOS

IF _DOS
   SET COLOR TO 7/1 
ENDIF

ENDIF

   @ L+0, M PROMPT ' 1 - Creditors Code .... Entry'
   @ L+2, M PROMPT ' 2 - Creditors Code .... Correction'
   @ L+4, M PROMPT ' 3 - Creditors Code .... Deletion'
   @ L+6, M PROMPT ' 4 - Creditors Code .... Printing'
   @ L+8, M PROMPT ' 9 - Return to Previous Menu'

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN

      CASE MCHOICE=4
         CLEAR
         DO cred_prt

      CASE MCHOICE=3
         CLEAR
         DO cred_del

      CASE MCHOICE=2
         CLEAR
         DO cred_cor

      CASE MCHOICE=1
         CLEAR
         DO cred_ent

   ENDCASE

ENDDO

RETURN
**********

PROCEDURE CRED-ENT
CLEAR

IF _DOS
SET COLOR TO +* w
ENDIF

@ 5, 27 SAY 'W A R N I N G '
@ 6, 27 SAY '------------- '

IF _DOS
SET COLOR TO 7/1
ENDIF

@ 8, 7 SAY 'If you are using this Inventory Program Integrated with Accounts '
@ 9, 7 SAY "Then create Creditors/Debtors Codes using ' Ledger Heads Maintenance '"
@ 10, 7 SAY 'option in Accounts and DO NOT USE  this option. In case you are using  '
@ 11, 7 SAY 'this Inventory Program independently, please proceed with coding thru'
@ 12, 7 SAY 'this option .......'
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 14, 7 SAY 'Do you want to continue with this option ? - Y/N  ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='N'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE SPACE(30) TO MTITLE
   STORE SPACE(25) TO MADD1, MADD2
   STORE SPACE(45) TO MREMARKS
   @ 5, 25 SAY 'Creditors Codes Entry'
   @ 6, 25 TO 6, 45 DOUBLE
   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 8, 20 SAY 'Creditor Code '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 8, 35 GET MC1 PICTURE '99'
      @ 8, 37 SAY '-'
      @ 8, 40 SAY '-'

IF _DOS
      SET COLOR TO 
ENDIF

      @ 8, 38 GET MC2 PICTURE '@z 99'
      @ 8, 41 GET MC3 PICTURE '@z 999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 8, 38 SAY MPC2
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 8, 41 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 18 CLEAR TO 23, 63
      IF CNT=0
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF FOUND()
            @ 23, 18 SAY 'This Code Already Exist  -- Press any key'
            WAIT ''
            @ 23, 17 CLEAR TO 23, 63
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         UNLOCK
         CNT= 1
      ENDIF
      @ 10, 20 SAY 'Cretitor     ' GET MTITLE PICTURE '@!'
      READ
      @ 12, 20 SAY 'Address      ' GET MADD1
      @ 14, 20 SAY '             ' GET MADD2
      READ
      @ 16, 20 SAY 'R e m a r k s '
      @ 18, 20 GET MREMARKS
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE TITLE WITH MTITLE
      REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2, REMARKS WITH MREMARKS
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='Y'
         EXIT
      ENDIF
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDDO
ENDDO
RETURN

PROCEDURE CRED-COR
CLEAR
CLOSE ALL
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3
   @ 5, 25 SAY 'Creditors Codes Correction'
   @ 6, 25 TO 6, 51 DOUBLE
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 8, 20 SAY 'Creditor Code '

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 35 GET MC1 PICTURE '99'
   @ 8, 37 SAY '-'
   @ 8, 40 SAY '-'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 8, 38 GET MC2 PICTURE '@z 99'
   @ 8, 41 GET MC3 PICTURE '@z 999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 38 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 41 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 18 CLEAR TO 23, 63
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist  -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 10, 20 SAY 'Cretitor     ' GET TITLE PICTURE '@!'
   @ 12, 20 SAY 'Address      ' GET ADDRESS1
   @ 14, 20 SAY '             ' GET ADDRESS2
   @ 16, 20 SAY 'R e m a r k s '
   @ 18, 20 GET REMARKS
   READ
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Correction Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE CRED-DEL
CLEAR
CLOSE ALL
SET DELETED ON
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   MC1= 0
   STORE 0 TO MC2, MC3
   DO top
   @ 5, 25 SAY 'Creditors Codes Deletion'
   @ 6, 25 TO 6, 49 DOUBLE
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 8, 20 SAY 'Creditor Code '

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 35 GET MC1 PICTURE '99'
   @ 8, 37 SAY '-'
   @ 8, 40 SAY '-'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 8, 38 GET MC2 PICTURE '@z 99'
   @ 8, 41 GET MC3 PICTURE '@z 999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 8, 38 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 41 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 18 CLEAR TO 23, 63
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 18 SAY 'This Code Does Not Exist  -- Press any key'
      WAIT ''
      @ 23, 17 CLEAR TO 23, 63
      LOOP
   ENDIF
   @ 10, 20 SAY 'Cretitor     '
   @ 12, 20 SAY 'Address      '
   @ 16, 20 SAY 'R e m a r k s '

IF _DOS
   SET COLOR TO /w     
ENDIF

   @ 10, 35 SAY TITLE PICTURE '@!'
   @ 12, 35 SAY ADDRESS1
   @ 14, 35 SAY ADDRESS2
   @ 18, 20 SAY REMARKS

IF _DOS
   SET COLOR TO 
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 25 SAY ' Are You Sure Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 20 CLEAR TO 23, 55
      @ 23, 23 SAY ' More Data Deletion Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE CRED-PRT
CLEAR
CLOSE ALL
USE ldgrcode INDEX ac1
MC1= 0
DO WHILE .T.

IF _DOS
   SET COLOR TO w+
ENDIF

   @ 10, 14 SAY 'Please give "First Level Code" of Creditors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'

IF _DOS
   SET COLOR TO 
ENDIF

   READ
   IF MC1=0
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
GOTO TOP
CLEAR
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   ABC= ' '
   DO WHILE .not. ABC$'CQ'
      @ 12, 25 SAY 'Continue/Quit' GET ABC FUNCTION '!'
      READ
   ENDDO
   IF ABC='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 1
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY 'Creditors Codes  '
         L= L+2
      ELSE
         DO SCREEN
         L= L+2
         @ 3, 25 SAY '    Creditors   Codes  '
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 6 SAY 'Code  '
      @ L, 25 SAY 'Creditors '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
      L= L+1
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
   @ L, 6 SAY MPC1 PICTURE '99'
   @ L, 8 SAY '-'
   @ L, 9 SAY MPC2 PICTURE '99'
   @ L, 11 SAY '-'
   @ L, 12 SAY MPC3 PICTURE '999'
   @ L, 25 SAY TITLE
   L= L+1
   @ L, 25 SAY ADDRESS1
   L= L+1
   @ L, 25 SAY ADDRESS2
   SKIP
   L= L+2
   IF EOF().and.L>20
      EXIT
   ENDIF
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         L= 1
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
      L= 1
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN


******************
PROCEDURE ORD_MEN
******************

DO WHILE .T.

   L= 8
   M= 19

IF _DOS

IF _DOS
   SET COLOR TO 7/1
ENDIF

ENDIF

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG
   
IF _DOS

IF _DOS
   SET COLOR TO +7/1 
ENDIF

ENDIF

   @ 6, 25 SAY '  Orders  File  Maintenance'

IF _DOS

IF _DOS
   SET COLOR TO 7/1
ENDIF

ENDIF

   @ L+0, M PROMPT ' 1 - Orders To Supplier/Creditor . Entry      '
   @ L+1, M PROMPT ' 2 - Orders To Supplier/Creditor . Correction '
   @ L+2, M PROMPT ' 3 - Orders To Supplier/Creditor . Deletion   '
   @ L+3, M PROMPT ' 4 - Orders To Supplier/Creditor . Printing   '
   @ L+4, 16 TO L+4, 64
   @ L+5, M PROMPT ' 5 - Orders From Dealers/Debtors . Entry      '
   @ L+6, M PROMPT ' 6 - Orders From Dealers/Debtors . Correction '
   @ L+7, M PROMPT ' 7 - Orders From Dealers/Debtors . Deletion   '
   @ L+8, M PROMPT ' 8 - Orders From Dealers/Debtors . Printing   '
   @ L+10, M PROMPT ' 9 - Return to Previous Menu                  '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=9
         CLEAR
         CLOSE ALL
         RETURN

      CASE MCHOICE=8
         CLEAR
         DO dord_prt

      CASE MCHOICE=7
         CLEAR
         DO dord_del

      CASE MCHOICE=6
         CLEAR
         DO dord_cor

      CASE MCHOICE=5
         CLEAR
         DO dord_ent

      CASE MCHOICE=4
         CLEAR
         DO cord_prt

      CASE MCHOICE=3
         CLEAR
         DO cord_del

      CASE MCHOICE=2
         CLEAR
         DO cord_cor

      CASE MCHOICE=1
         CLEAR
         DO cord_ent

   ENDCASE

ENDDO
RETURN
*******

PROCEDURE CORD-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TO ord-idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY 'ORDER TO SUPPLIERS / CREDITORS  ENTRY'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MORDER_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY MVCH_NO

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 1
      SEEK MORDER_NO
      IF FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Order No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 
ENDIF

         @ 23, 10 SAY SPACE(75)
         LOOP
      ENDIF
      SELECT 5
      ZAP
      @ R, 28 SAY 'Date ' GET MDATE
      READ
      DO chk
      @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
      READ
      IF MDEADLINE<MDATE
         LOOP
      ENDIF
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 15 SAY '  -  -   '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 15 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 15 SAY MPC1
         @ R, 17 SAY '-'
         @ R, 18 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 18 SAY MPC2
         @ R, 20 SAY '-'
         @ R, 21 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 21 SAY MPC3

IF _DOS
         SET COLOR TO 
ENDIF

         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
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

         @ R, 28 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 3 SAY 'Item Code'
      @ R, 15 SAY 'Item '
      @ R, 47 SAY 'Qty Ordered'
      @ R, 62 SAY 'Rate '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            @ 23, 50 SAY 'Press Ctrl Q To Quit '

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 23, 45 CLEAR TO 23, 78

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
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
            SELECT 2
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

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY_ORD
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
            DO ord-cor
         ENDIF
         SELECT 5
         USE
         SELECT 1
         APPEND FROM ord-work
         SELECT 5
         USE ord-work
         ZAP
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

PROCEDURE CORD-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TO ord-idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 23 SAY 'ORDER TO SUPPLIERS / CREDITORS  CORRECTION'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order INDEX ord-idx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   READ
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 GET MC1 PICTURE '99'
      READ

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
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

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO ord-cor
   SELECT 5
   GOTO TOP
   REPLACE ALL DATE WITH MDATE, DEADLINE WITH MDEADLINE, C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   USE
   SELECT 1
   DELETE ALL FOR ORDER_NO=MORDER_NO
   PACK
   APPEND FROM ord-work
   SELECT 5
   USE ord-work
   ZAP
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

PROCEDURE CORD-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TO ord-idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 23 SAY 'ORDER TO SUPPLIERS / CREDITORS  DELETION '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order INDEX ord-idx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3
      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 SAY MC1 PICTURE '99'

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 SAY MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 SAY MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
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

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   DO ord-dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR ORDER_NO=MORDER_NO
      PACK
      SELECT 5
      ZAP
   ENDIF
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

PROCEDURE CORD-PRT
CLEAR
CLOSE ALL
SELECT 1
USE p-order
INDEX ON ORDER_NO TO ord-idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO
ABC= 0
DO WHILE ABC=0
   @ 3, 30 SAY 'ORDER TO SUPPLIERS/CREDITORS'
   @ 4, 30 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 20 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 10, 33 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order INDEX ord-idx
   SELECT 5
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No Order Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 5
GOTO TOP
MDATE= DATE
MDEADLINE= DEADLINE
MC1= C1
MC2= C2
MC3= C3
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
L= 3
HEADING= .T.
CNT= 0
DO WHILE .not. EOF()
   SELECT 5
   IF HEADING
      IF P='P'
         DO top-prt
         @ 3, 28 SAY 'ORDER TO SUPPLIERS/CREDITORS'
         L= L+1
         @ L, 28 SAY '----------------------------'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 25 SAY 'ORDER TO SUPPLIERS/CREDITORS'
         L= L+1
         @ L, 25 SAY '----------------------------'
         L= L+1
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
      @ L, 1 SAY 'Order No   : '
      @ L, 14 SAY MVCH_NO
      @ L, 51 SAY 'Date              : '+DTOC(DATE)
      L= L+1
      @ L, 1 SAY 'Party Code : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ L, 14 SAY MPC1
      @ L, 16 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ L, 17 SAY MPC2
      @ L, 19 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 20 SAY MPC3
      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 5
      @ L, 51 SAY 'Delivery Deadline : '+DTOC(DEADLINE)
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item '
      @ L, 38 SAY 'Unit'
      @ L, 43 SAY 'Qty Ordered'
      @ L, 62 SAY 'Rate '
      @ L, 73 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 5
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
   @ L, 7 SAY MP3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MUNIT= UNIT
      @ L, 13 SAY ITEM
   ENDIF
   SELECT 5
   MQTY= QTY_ORD
   MRATE= RATE
   MAMT= MQTY*MRATE
   @ L, 38 SAY MUNIT
   @ L, 41 SAY MQTY PICTURE '9999999.9999'
   @ L, 54 SAY MRATE PICTURE '99999999.9999'
   @ L, 67 SAY MAMT PICTURE '999999999.99'
   MTOT= MTOT+MAMT
   L= L+1
   SKIP
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 3
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= 3
      HEADING= .T.
   ENDIF
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 54 SAY 'Total : '
@ L, 67 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 50 SAY REPLICATE('-',30)
L= L+1
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE DORD-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order
INDEX ON ORDER_NO TO sordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS  ENTRY'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MORDER_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY MVCH_NO

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 1
      SEEK MORDER_NO
      IF FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Order No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 
ENDIF

         @ 23, 10 SAY SPACE(75)
         LOOP
      ENDIF
      SELECT 5
      ZAP
      @ R, 28 SAY 'Date ' GET MDATE
      READ
      DO chk
      @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
      READ
      IF MDEADLINE<MDATE
         LOOP
      ENDIF
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 15 SAY '  -  -   '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 15 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 15 SAY MPC1
         @ R, 17 SAY '-'
         @ R, 18 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 18 SAY MPC2
         @ R, 20 SAY '-'
         @ R, 21 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 21 SAY MPC3

IF _DOS
         SET COLOR TO 
ENDIF

         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
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

         @ R, 28 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 3 SAY 'Item Code'
      @ R, 15 SAY 'Item '
      @ R, 47 SAY 'Qty Ordered'
      @ R, 62 SAY 'Rate '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            @ 23, 50 SAY 'Press Ctrl Q To Quit '

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 23, 45 CLEAR TO 23, 78

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
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
            SELECT 2
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

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY_ORD
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
            DO ord-cor
         ENDIF
         SELECT 5
         USE
         SELECT 1
         APPEND FROM ord-work
         SELECT 5
         USE ord-work
         ZAP
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

PROCEDURE DORD-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order
INDEX ON ORDER_NO TO ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS CORRECTION'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE s-order INDEX ordidx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   READ
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 GET MC1 PICTURE '99'
      READ

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
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

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO ord-cor
   SELECT 5
   GOTO TOP
   REPLACE ALL DATE WITH MDATE, DEADLINE WITH MDEADLINE, C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   USE
   SELECT 1
   DELETE ALL FOR ORDER_NO=MORDER_NO
   PACK
   APPEND FROM ord-work
   SELECT 5
   USE ord-work
   ZAP
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

PROCEDURE DORD-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order
INDEX ON ORDER_NO TO ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS DELETION '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE s-order INDEX ordidx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3
      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 SAY MC1 PICTURE '99'

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 SAY MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 SAY MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
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

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   DO ord-dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR ORDER_NO=MORDER_NO
      PACK
      SELECT 5
      ZAP
   ENDIF
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

PROCEDURE DORD-PRT
CLEAR
CLOSE ALL
SELECT 1
USE s-order
INDEX ON ORDER_NO TO ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 5
USE ord-work
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO
ABC= 0
DO WHILE ABC=0
   @ 3, 30 SAY 'ORDER FROM DEALERS/DEBTORS '
   @ 4, 30 SAY '-------------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 20 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 10, 33 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order INDEX ord-idx
   SELECT 5
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No Order Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 5
GOTO TOP
MDATE= DATE
MDEADLINE= DEADLINE
MC1= C1
MC2= C2
MC3= C3
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
L= 3
HEADING= .T.
CNT= 0
DO WHILE .not. EOF()
   SELECT 5
   IF HEADING
      IF P='P'
         DO top-prt
         @ 3, 28 SAY 'ORDER FROM DEALERS/DEBTORS '
         L= L+1
         @ L, 28 SAY '----------------------------'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 25 SAY 'ORDER FROM DEALERS/DEBTORS '
         L= L+1
         @ L, 25 SAY '----------------------------'
         L= L+1
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
      @ L, 1 SAY 'Order No   : '
      @ L, 14 SAY MVCH_NO
      @ L, 51 SAY 'Date              : '+DTOC(DATE)
      L= L+1
      @ L, 1 SAY 'Party Code : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ L, 14 SAY MPC1
      @ L, 16 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ L, 17 SAY MPC2
      @ L, 19 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 20 SAY MPC3
      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 5
      @ L, 51 SAY 'Delivery Deadline : '+DTOC(DEADLINE)
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item '
      @ L, 38 SAY 'Unit'
      @ L, 43 SAY 'Qty Ordered'
      @ L, 62 SAY 'Rate '
      @ L, 73 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 5
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
   @ L, 7 SAY MP3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MUNIT= UNIT
      @ L, 13 SAY ITEM
   ENDIF
   SELECT 5
   MQTY= QTY_ORD
   MRATE= RATE
   MAMT= MQTY*MRATE
   @ L, 38 SAY MUNIT
   @ L, 41 SAY MQTY PICTURE '9999999.9999'
   @ L, 54 SAY MRATE PICTURE '99999999.9999'
   @ L, 67 SAY MAMT PICTURE '999999999.99'
   MTOT= MTOT+MAMT
   L= L+1
   SKIP
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 3
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= 3
      HEADING= .T.
   ENDIF
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 54 SAY 'Total : '
@ L, 67 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 50 SAY REPLICATE('-',30)
L= L+1
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE ORD-COR
SELECT 5
GOTO TOP
DO ord-dsp
SELECT 5
GOTO TOP
R= 7
CK= 0
DO WHILE .T.
   R= R+1
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 5
      DELETE
      PACK
      IF EOF()
         EXIT
      ENDIF
   ENDIF
   IF MQES='N'
      SELECT 5
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 5
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MRATE, MAMOUNT, MQTY_ORD
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
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
            SELECT 2
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

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            DO WHILE .T.
               @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
               READ
               IF MQTY_ORD=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 61 GET MRATE PICTURE '999999999.9999'
            READ
            EXIT
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 5
         MI1= I1
         MI2= I2
         MI3= I3
         MORDER_NO= ORDER_NO
         MQTY_ORD= QTY_ORD
         MRATE= RATE

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 3 SAY '  -  -    '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 3 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 3 SAY MP1
         @ R, 5 SAY '-'
         @ R, 6 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 6 SAY MP2
         @ R, 8 SAY '-'
         @ R, 9 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 9 SAY MP3

IF _DOS
         SET COLOR TO 
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
         SELECT 2
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

         @ R, 15 SAY ITEM

IF _DOS
         SET COLOR TO 
ENDIF

         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         EXIT
      ENDDO
      SELECT 5
      REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
   ENDIF
   SELECT 5
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      SKIP
   ENDIF
   IF MANS='N'
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE ORD-DSP
R= 8
@ R, 0 CLEAR TO 24, 79
SELECT 5
GOTO TOP
DO WHILE .not. EOF()
   SELECT 5
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY_ORD= QTY_ORD
   MRATE= RATE

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 3 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 3 SAY MP1
   @ R, 5 SAY '-'
   @ R, 6 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 6 SAY MP2
   @ R, 8 SAY '-'
   @ R, 9 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 9 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
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

   @ R, 15 SAY ITEM
   SELECT 5
   @ R, 47 SAY MQTY_ORD PICTURE '9999999.9999'
   @ R, 61 SAY MRATE PICTURE '999999999.9999'

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+1
   SKIP
ENDDO
RETURN

PROCEDURE POST
CLEAR
CLOSE ALL
SET DATE brit

IF _DOS
SET COLOR TO 7/1
ENDIF

SELECT 1
CLEAR
USE st-tran
SORT ON DOC_TYPE, DOC_NO, SR_NO TO abc
USE
SELECT 1
USE abc
COPY TO st-tran
RUN del abc.dbf
SELECT 1
USE st-tran
INDEX ON DATE TO stdate
SELECT 2
USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO ordidx
SELECT 3
USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-bal
SELECT 4
USE stock
SELECT 5
USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO sordidx
SELECT 1
GOTO BOTTOM
STORE DATE TO MDATE, MPDATE
@ 1, 2 SAY 'Post Transactions To Ledger:'
@ 3, 2 SAY 'This option will post all un-posted transactions to their respective'
@ 4, 2 SAY 'ledger heads Upto Given Date....'
@ 13, 14 SAY 'Transactions upto            exist in file '

IF _DOS
SET COLOR TO *+g
ENDIF

@ 13, 32 SAY MDATE

IF _DOS
SET COLOR TO 7/1     
ENDIF

@ 14, 14 SAY 'Date upto which you want transactions to be posted ' GET MPDATE
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
SELECT 1
CNT= 0
GOTO TOP
SET FILTER TO FLAG=' '.and.DATE<=MPDATE
GOTO TOP
MSR_NO= 0
DO WHILE .not. EOF()
   MDATE= DATE
   MDOC_NO= DOC_NO
   MDOC_TYPE= DOC_TYPE
   MLOC_CODE= LOC_CODE
   MORDER_NO= ORDER_NO
   MDEPT_CODE= DEPT_CODE
   MCITY_CODE= CITY_CODE
   MZONE_CODE= ZONE_CODE
   MI1= I1
   MI2= I2
   MI3= I3
   MC1= C_C1
   MC2= C_C2
   MC3= C_C3
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   MQTY= T_QTY
   MRATE= RATE
   MAMOUNT= T_AMOUNT
   MPSR_UPRIC= PSR_UPRICE
   MPSR_TPRIC= PSR_TPRICE
   MF_T= F_T
   IF MDOC_TYPE=1
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         VAL_TOT= 0
         VAL_TOT= QTY*AVG_PRICE
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MVAL_TOT= 0
         MVAL_TOT= VAL_TOT+MAMOUNT
         MAVG= 0
         IF MTOT_QTY<>0
            MAVG= MVAL_TOT/MTOT_QTY
         ENDIF
         REPLACE QTY WITH MTOT_QTY
         REPLACE AVG_PRICE WITH MAVG
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 2
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=2
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 2, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE D_D1 WITH MD1, D_D2 WITH MD2, D_D3 WITH MD3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 2
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=3
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 3, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
      REPLACE DEPT_CODE WITH MDEPT_CODE, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=4
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 4, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
      REPLACE DEPT_CODE WITH MDEPT_CODE, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=5
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, DEPT_CODE WITH MDEPT_CODE
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=6
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 6, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE D_D1 WITH MD1, D_D2 WITH MD2, D_D3 WITH MD3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
      SELECT 5
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=7
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 7, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 5
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD+MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=8
      IF MF_T='F'
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            MTOT_QTY= 0
            MTOT_QTY= QTY-MQTY
            MAVG= AVG_PRICE
            REPLACE QTY WITH MTOT_QTY
            MSR_NO= SR_NO+1
            REPLACE SR_NO WITH MSR_NO
            SELECT 4
            APPEND BLANK
            REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
            REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
            REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'F'
            REPLACE QTY_BAL WITH MTOT_QTY, AVG_PRICE WITH MAVG
         ENDIF
      ENDIF
      IF MF_T='T'
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            MTOT_QTY= 0
            MTOT_QTY= QTY+MQTY
            MAVG= AVG_PRICE
            REPLACE QTY WITH MTOT_QTY
            MSR_NO= SR_NO+1
            REPLACE SR_NO WITH MSR_NO
            SELECT 4
            APPEND BLANK
            REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
            REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
            REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'T'
            REPLACE QTY_BAL WITH MTOT_QTY, AVG_PRICE WITH MAVG
         ENDIF
      ENDIF
   ENDIF
   SELECT 1
   REPLACE FLAG WITH 'Y'
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
CLEAR
CLOSE ALL
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TO itemidx1
INDEX ON DOC_NO TO itemidx2
INDEX ON DATE TO stk-idx
CLOSE ALL
RETURN

PROCEDURE REP-MEN
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 6, 25 SAY '  R E P O R T S   M E N U '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 25 PROMPT ' 1 - Purchase Report       '
   @ 9, 25 PROMPT ' 2 - Pending Orders Report '
   @ 10, 25 PROMPT ' 3 - Sale Report           '
   @ 11, 25 PROMPT ' 4 - Current Status Report '
   @ 12, 25 PROMPT ' 5 - Consumption Reports   '
   @ 13, 25 PROMPT ' 6 - Production Reports    '
   @ 14, 25 PROMPT ' 7 - Sale Return Report (Party Wise) '
   @ 15, 25 PROMPT ' 8 - Purchase Return Report (Party Wise) '
   @ 16, 25 PROMPT ' 9 - Stock Transfer Report (Item Wise)   '
   @ 18, 25 PROMPT '10 - Return to Main Menu   '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-men1
      CASE MCHOICE=2
         DO rep-men2
      CASE MCHOICE=3
         DO rep-men3
      CASE MCHOICE=4
         DO rep-men4
      CASE MCHOICE=5
         DO rep-men5
      CASE MCHOICE=6
         DO rep-men6
      CASE MCHOICE=7
         DO rep-salr
      CASE MCHOICE=8
         DO rep-purr
      CASE MCHOICE=9
         DO rep-tran
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-PURR
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 58 DOUBLE
   @ 2, 21 SAY 'PURCHASE RETURN REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=2
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
IF P='P'
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF P='P'
         DO top-prt
      ELSE
         DO SCREEN
      ENDIF
      @ 3, 25 SAY 'PURCHASE RETURN REPORT (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY ' Qty  '
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         IF P='P'
            DO top-prt
         ELSE
            DO SCREEN
         ENDIF
         @ 3, 25 SAY 'PURCHASE RETURN REPORT (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY ' Qty '
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='S'.and.L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         L= 6
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-TRAN
CLOSE ALL
CLEAR
HEADING= .T.
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx1
SELECT 3
USE loc INDEX loc-idx
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'STOCK TRANSFER REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=8
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3, FLOC, TLOC, PG
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      PG= PG+1
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 10 SAY 'Stock TRansfer (Item Wise)'
      ELSE
         DO screen
         @ 3, 27 SAY 'Stock Transfer (Item Wise)'
      ENDIF
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 5, 1 SAY 'To    :'
      @ 5, 9 SAY TDATE
      @ 6, 1 SAY 'Code  : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 6, 9 SAY MPC1
      @ 6, 11 SAY '-'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 6, 12 SAY MPC1
      @ 6, 14 SAY '-'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 6, 15 SAY MPC1
      @ 6, 22 SAY MITEM
      @ 6, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Date'
      @ L, 11 SAY 'From Location '
      @ L, 38 SAY 'To Location '
      @ L, 65 SAY 'Qty Sold'
      L= L+1
      @ L, 1 SAY REPLICATE('=',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   IF F_T='F'
      FLOC= LOC_CODE
      SELECT 3
      SEEK FLOC
      IF FOUND()
         @ L, 11 SAY LOC
      ELSE
         @ L, 11 SAY ' '
      ENDIF
   ELSE
      TLOC= LOC_CODE
      SELECT 3
      SEEK TLOC
      IF FOUND()
         @ L, 38 SAY LOC
      ELSE
         @ L, 38 SAY ' '
      ENDIF
   ENDIF
   SELECT 2
   MQTY= T_QTY
   @ L, 65 SAY MQTY PICTURE '9999999.9999'
   MTOT= MTOT+MQTY
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 55 SAY 'TOTAL'
@ L, 65 SAY MTOT/2 PICTURE '9999999.9999'
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY 'P U R C H A S E    R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 22 PROMPT ' 1 - Purchase Report (Date Wise)          '
   @ 9, 22 PROMPT ' 2 - Purchase Report (Party Wise)         '
   @ 10, 22 PROMPT ' 3 - Purchase Report (Item Wise)          '
   @ 11, 22 PROMPT ' 4 - Purchase Report Display              '
   @ 12, 22 PROMPT ' 5 - Purchase Report (Party Wise Summary) '
   @ 13, 22 PROMPT ' 6 - Purchase Report (Item Wise Summary)  '
   @ 15, 22 PROMPT ' 9 - Return to Main Menu                  '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-pur1
      CASE MCHOICE=2
         DO rep-pur2
      CASE MCHOICE=3
         DO rep-pur3
      CASE MCHOICE=4
         DO rep-pur4
      CASE MCHOICE=5
         DO rep-pur5
      CASE MCHOICE=6
         DO rep-pur6
      CASE MCHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-MEN2
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY '   O R D E R S     R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 25 SAY '        P U R C H A S E S '
   @ 9, 25 PROMPT ' 1 - Pending Orders (Date Wise) '
   @ 10, 25 PROMPT ' 2 - Pending Orders (Party Wise) '
   @ 11, 25 PROMPT ' 3 - Pending Orders (Item Wise)  '
   @ 12, 25 SAY '            S A L E S '
   @ 13, 25 PROMPT ' 4 - Pending Orders (Date Wise)  '
   @ 14, 25 PROMPT ' 5 - Pending Orders (Party Wise) '
   @ 15, 25 PROMPT ' 6 - Pending Orders (Item Wise)  '
   @ 17, 25 PROMPT ' 9 - Return to Main Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-ord1
      CASE MCHOICE=2
         DO rep-ord2
      CASE MCHOICE=3
         DO rep-ord3
      CASE MCHOICE=4
         DO rep-ord4
      CASE MCHOICE=5
         DO rep-ord5
      CASE MCHOICE=6
         DO rep-ord6
      CASE MCHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-PUR1
CLEAR
CLOSE ALL
SET DATE brit
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX stk-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
WAIT
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'GRN #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY ' Qty '
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   @ L, 25 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 25 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 28 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 31 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY ITEM
      @ L, 75 SAY UNIT
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= T_AMOUNT
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY RATE PICTURE '9999999999.9999'
   @ L, 116 SAY MSVALUE PICTURE '9999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+T_AMOUNT
   IF P='P'.and.L>54
      L= 6
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',55)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 116 SAY MTOT1 PICTURE '9999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',55)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PUR2
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE  REPORT  (PARTY WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY 'Qty '
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 7
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 7 SAY 'PURCHASE REPORT  (PARTY WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  : '
         @ 7, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 7, 9 SAY MP1
         @ 7, 11 SAY '-'
         @ 7, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 7, 12 SAY MP2
         @ 7, 14 SAY '-'
         @ 7, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 7, 15 SAY MP3
         @ 7, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 13 SAY 'Invoice #'
         @ L, 25 SAY 'Item Code'
         @ L, 39 SAY 'Descripition'
         @ L, 75 SAY 'Unit'
         @ L, 85 SAY 'Qty '
         @ L, 108 SAY 'Rate'
         @ L, 123 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY MDOC
      @ L, 25 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 25 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 28 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 31 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 39 SAY ITEM
         @ L, 75 SAY UNIT
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 85 SAY MQTY PICTURE '9999999.9999'
      @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 115 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='P'.and.L>56
         L= 7
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
   @ L, 75 SAY 'Total '
   @ L, 85 SAY MTOT PICTURE '9999999.9999'
   @ L, 115 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 70 SAY 'Grand Total '
@ L, 85 SAY GTOT PICTURE '9999999.9999'
@ L, 115 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 70 SAY REPLICATE('-',60)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PUR3
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 39 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
L= 7
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE REPORT (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MI1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MI2 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MI3 PICTURE '9999'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'GRN #'
      @ L, 25 SAY 'Party Code'
      @ L, 39 SAY 'Descripition'
      @ L, 85 SAY 'Qty Purchased'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MC1= C_C1
   MC2= C_C2
   MC3= C_C3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ L, 25 SAY MPC1
   @ L, 27 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ L, 28 SAY MPC2
   @ L, 30 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ L, 31 SAY MPC3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= T_AMOUNT
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY RATE PICTURE '999999999.9999'
   @ L, 116 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+MSVALUE
   IF P='P'.and.L>56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
      L= 7
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',58)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '999999999.9999'
@ L, 116 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',58)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PUR4
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'SQ'
   @ 23, 50 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      DO SCREEN
      @ 3, 28 SAY 'PURCHASE REPORT  (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty '
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         DO SCREEN
         @ 3, 28 SAY 'PURCHASE REPORT  (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty '
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PUR5
CLEAR
CLOSE ALL
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 36 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MC1.and.C_C2=MC2.and.C_C3=MC3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PURCHASE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'PURCHASE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MC1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MC2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MC3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Purchased'
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PUR6
CLEAR
CLOSE ALL
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PURCHASE REPORT (ITEM WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'PURCHASE REPORT (ITEM WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Purchased'
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 66 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD1
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE p-order INDEX prod-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO TDATE, FDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 6
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD2
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE p-order INDEX prod-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 12, 3 SAY 'Party '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY '  -  -   '
   @ 12, 13 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 16 GET MC2 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 12, 16 SAY MPC2
   @ 12, 19 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 12, 19 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   LOCATE FOR C1=MC1.and.C2=MC2.and.C3=MC3
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 23 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO C1=MC1.and.C2=MC2.and.C3=MC3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Party : '+MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit'
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD3
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE p-order INDEX pord-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE 0 TO MITEM_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 55 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY 'Item  '
   @ 12, 25 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY MPC1
   @ 12, 28 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MPC2
   @ 12, 31 GET MI3 PICTURE '9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 36 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Item  : '+MITEM
      @ 6, 50 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Party '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 92 SAY 'Rate'
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '99999'
   SELECT 3
   LOCATE FOR C1= A->C1.and.C2= A->C2.and.C3= A->C3
   IF FOUND()
      @ L, 25 SAY TITLE
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 60 SAY REPLICATE('-',70)
L= L+1
@ L, 65 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 60 SAY REPLICATE('-',70)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD4
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE s-order INDEX sord-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO TDATE, FDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 6
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD5
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE s-order INDEX sord-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 12, 3 SAY 'Party '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY '  -  -   '
   @ 12, 13 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 16 GET MC2 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 12, 16 SAY MPC2
   @ 12, 19 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 12, 19 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   LOCATE FOR C1=MC1.and.C2=MC2.and.C3=MC3
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 23 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO C1=MC1.and.C2=MC2.and.C3=MC3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Party : '+MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit'
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ORD6
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item INDEX item-idx
SELECT 1
USE s-order INDEX sord-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE 0 TO MITEM_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 55 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY 'Item  '
   @ 12, 25 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY MPC1
   @ 12, 28 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MPC2
   @ 12, 31 GET MI3 PICTURE '9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 36 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Item  : '+MITEM
      @ 6, 50 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Party '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 92 SAY 'Rate'
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '99999'
   SELECT 3
   LOCATE FOR C1= A->C1.and.C2= A->C2.and.C3= A->C3
   IF FOUND()
      @ L, 25 SAY TITLE
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 60 SAY REPLICATE('-',70)
L= L+1
@ L, 65 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 60 SAY REPLICATE('-',70)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN3
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY '    S A L E S    R E P O R T S '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 7, 25 SAY '       Printing/Display'
   @ 8, 25 PROMPT ' 1 - Sale  Report   (Date Wise)  '
   @ 9, 25 PROMPT ' 2 - Sale  Report   (Party Wise) '
   @ 10, 25 PROMPT ' 3 - Sale  Report   (Item Wise)  '
   @ 11, 25 PROMPT ' 4 - Sale  Report (Party Wise Summary) '
   @ 12, 25 PROMPT ' 5 - Sale  Report (Item Wise Summary)  '
   @ 13, 25 PROMPT ' 6 - Sale  Display  (Date Wise)  '
   @ 14, 25 PROMPT ' 7 - Sale  Display  (Party Wise) '
   @ 15, 25 PROMPT ' 8 - Sale  Display  (Item Wise)  '
   @ 16, 25 PROMPT ' 9 - Sale  Display  (City Wise)  '
   @ 17, 25 PROMPT '10 - Sale  Display  (Zone Wise)  '
   @ 19, 25 PROMPT '11 - Return to Main Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-sal1
      CASE MCHOICE=2
         DO rep-sal2
      CASE MCHOICE=3
         DO rep-sal3
      CASE MCHOICE=4
         DO rep-sal7
      CASE MCHOICE=5
         DO rep-sal8
      CASE MCHOICE=6
         DO rep-sal4
      CASE MCHOICE=7
         DO rep-sal5
      CASE MCHOICE=8
         DO rep-sal6
      CASE MCHOICE=9
         DO rep-city
      CASE MCHOICE=10
         DO rep-zone
      CASE MCHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-SALR
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  RETURN REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=7
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
IF P='P'
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF P='P'
         DO top-prt
      ELSE
         DO SCREEN
      ENDIF
      @ 3, 25 SAY 'SALE RETURN REPORT (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         IF P='P'
            DO top-prt
         ELSE
            DO SCREEN
         ENDIF
         @ 3, 25 SAY 'SALE RETURN REPORT (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty Sold'
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='S'.and.L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         L= 6
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL1
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx1
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 89 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   @ L, 25 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 25 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 28 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 31 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY ITEM
      @ L, 75 SAY UNIT
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
   @ L, 115 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+PSR_TPRICE
   IF P='P'.and.L>56
      L= 6
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',55)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '9999999.9999'
@ L, 115 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',55)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL2
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (PARTY WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 7
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 7 SAY 'SALE  REPORT  (PARTY WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  : '
         @ 7, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 7, 9 SAY MP1
         @ 7, 11 SAY '-'
         @ 7, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 7, 12 SAY MP2
         @ 7, 14 SAY '-'
         @ 7, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 7, 15 SAY MP3
         @ 7, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 13 SAY 'Invoice #'
         @ L, 25 SAY 'Item Code'
         @ L, 39 SAY 'Descripition'
         @ L, 75 SAY 'Unit'
         @ L, 85 SAY 'Qty Sold'
         @ L, 108 SAY 'Rate'
         @ L, 123 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY MDOC
      @ L, 25 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 25 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 28 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 31 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 39 SAY ITEM
         @ L, 75 SAY UNIT
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 85 SAY MQTY PICTURE '9999999.9999'
      @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 115 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='P'.and.L>56
         L= 7
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
   @ L, 75 SAY 'Total '
   @ L, 85 SAY MTOT PICTURE '9999999.9999'
   @ L, 115 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 70 SAY 'Grand Total '
@ L, 85 SAY GTOT PICTURE '9999999.9999'
@ L, 115 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 70 SAY REPLICATE('-',60)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL3
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx1
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Party Code'
      @ L, 39 SAY 'Descripition'
      @ L, 85 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ L, 25 SAY MPC1
   @ L, 27 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ L, 28 SAY MPC2
   @ L, 30 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ L, 31 SAY MPC3
   STORE STR(MD1,2)+STR(MD2,2)+STR(MD3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
   @ L, 115 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+PSR_TPRICE
   IF L>56
      L= 7
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',58)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '9999999.9999'
@ L, 115 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',58)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL4
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx1
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'SQ'
   @ 5, 4 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      DO SCREEN
      L= 6
      @ 3, 28 SAY 'SALE  REPORT  (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 10 SAY MDOC_NO
   @ L, 18 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 18 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 21 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 24 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 29 SAY LEFT(ITEM,18)
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MSVALUE= PSR_TPRICE
   @ L, 47 SAY MQTY PICTURE '99999.9999'
   @ L, 58 SAY MRATE PICTURE '9999999.99'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+PSR_TPRICE
   IF L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL'
@ L, 45 SAY MTOT PICTURE '9999999.9999'
@ L, 68 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
SET FILTER TO
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL5
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx2
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'SQ'
   @ 23, 50 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      DO SCREEN
      @ 3, 28 SAY 'SALE  REPORT  (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         DO SCREEN
         @ 3, 28 SAY 'SALE  REPORT  (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty Sold'
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL7
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+PSR_TPRICE
      MTOT= MTOT+PSR_TPRICE
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PG= PG+1
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL8
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX std-idx
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item   '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY ITEM
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MPC2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MPC3
      @ 7, 24 SAY MITEM
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MC1= D_D1
   MC2= D_D2
   MC3= D_D3
   STORE 0 TO MQTY, MAMT
   DO WHILE D_D1=MC1.and.D_D2=MC2.and.D_D3=MC3
      IF D_D1<>MC1.and.D_D2<>MC2.and.D_D3<>MC3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+PSR_TPRICE
      MTOT= MTOT+PSR_TPRICE
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ L, 7 SAY MP3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SELECT 3
   SEEK MCODE
   IF FOUND()
      @ L, 12 SAY TITLE
   ELSE
      @ L, 12 SAY '  '
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PG= PG+1
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-SAL6
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX itemidx1
SELECT 3
USE ldgrcode INDEX ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'SQ'
   @ 5, 4 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
GOTO TOP
SELECT 2
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO SCREEN
      @ 3, 30 SAY 'SALE REPORT (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MI1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MI2 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MI3 PICTURE '9999'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Party Code'
      @ L, 29 SAY 'Descripition'
      @ L, 58 SAY 'Qty Sold'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 10 SAY MDOC_NO
   @ L, 18 SAY '  -  -   '
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ L, 18 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ L, 21 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ L, 24 SAY MPC3
   STORE STR(MD1,2)+STR(MD2,2)+STR(MD3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 29 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 54 SAY MQTY PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+MSVALUE
   IF L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 40 SAY 'TOTAL'
@ L, 54 SAY MTOT PICTURE '9999999.9999'
@ L, 68 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
SET FILTER TO
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-CITY
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX cstck
SELECT 3
USE city INDEX city-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (CITY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (CITY WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'SALE REPORT (CITY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MCITY_CODE= CITY_CODE
   SELECT 3
   SEEK MCITY_CODE
   IF FOUND()
      MLOC= CITY
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'City : '+STR(MCITY_CODE,3)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',15+MLEN)
   L= L+1
   SELECT 2
   DO WHILE CITY_CODE=MCITY_CODE
      IF HEADING
         L= 6
         IF P='P'
            DO top-prt
            @ 3, 1 SAY CHR(14)
            @ 3, 5 SAY 'SALE REPORT (CITY WISE SUMMARY)'
         ELSE
            DO SCREEN
            @ 3, 24 SAY 'SALE REPORT (CITY WISE SUMMARY)'
         ENDIF
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Descripition'
         @ L, 42 SAY 'Unit'
         @ L, 52 SAY 'Qty Sold '
         @ L, 70 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF CITY_CODE<>MCITY_CODE
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      STORE 0 TO MQTY, MAMT
      DO WHILE MCITY_CODE=CITY_CODE.and.MI1=I1.and.MI2=I2.and.MI3=I3
         IF MCITY_CODE<>CITY_CODE.and.MI1<>I1.and.MI2<>I2.and.MI3<>I3
            EXIT
         ENDIF
         MQTY= MQTY+T_QTY
         MAMT= MAMT+PSR_TPRICE
         MTOT= MTOT+PSR_TPRICE
         MTQTY= MTQTY+T_QTY
         IF .not. EOF()
            SKIP
         ENDIF
      ENDDO
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 7 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 12 SAY ITEM
         @ L, 42 SAY UNIT
      ENDIF
      SELECT 2
      @ L, 52 SAY MQTY PICTURE '9999999.9999'
      @ L, 65 SAY MAMT PICTURE '9999999999.99'
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF P='S'.and.L>21
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         HEADING= .T.
         PG= PG+1
      ENDIF
   ENDDO
   L= L+1
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-ZONE
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item INDEX item-idx
SELECT 2
USE stock INDEX zstck
SELECT 3
USE zone
INDEX ON ZONE_CODE TO zone-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ZONE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
      ELSE
         DO SCREEN
         @ 3, 24 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MZONE_CODE= ZONE_CODE
   SELECT 3
   SEEK MZONE_CODE
   IF FOUND()
      MLOC= ZONE
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'Zone : '+STR(MZONE_CODE,2)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',15+MLEN)
   L= L+1
   SELECT 2
   DO WHILE ZONE_CODE=MZONE_CODE
      IF HEADING
         L= 6
         IF P='P'
            DO top-prt
            @ 3, 1 SAY CHR(14)
            @ 3, 5 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
         ELSE
            DO SCREEN
            @ 3, 24 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
         ENDIF
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Descripition'
         @ L, 42 SAY 'Unit'
         @ L, 52 SAY 'Qty Sold '
         @ L, 70 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF ZONE_CODE<>MZONE_CODE
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      STORE 0 TO MQTY, MAMT
      DO WHILE MZONE_CODE=ZONE_CODE.and.MI1=I1.and.MI2=I2.and.MI3=I3
         IF MZONE_CODE<>ZONE_CODE.and.MI1<>I1.and.MI2<>I2.and.MI3<>I3
            EXIT
         ENDIF
         MQTY= MQTY+T_QTY
         MAMT= MAMT+PSR_TPRICE
         MTOT= MTOT+PSR_TPRICE
         MTQTY= MTQTY+T_QTY
         IF .not. EOF()
            SKIP
         ENDIF
      ENDDO
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 7 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 12 SAY ITEM
         @ L, 42 SAY UNIT
      ENDIF
      SELECT 2
      @ L, 52 SAY MQTY PICTURE '9999999.9999'
      @ L, 65 SAY MAMT PICTURE '9999999999.99'
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF P='S'.and.L>21
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         HEADING= .T.
         PG= PG+1
      ENDIF
   ENDDO
   L= L+1
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN4
DO WHILE .T.
   CLEAR

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 26 SAY ' Current Status Reports  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 22 PROMPT ' 1 - Present Stock Status Report          '
   @ 9, 22 PROMPT ' 2 - Present Stock Status (Location Wise) '
   @ 10, 22 PROMPT ' 3 - Present Stock Status (Item Wise)     '
   @ 11, 22 PROMPT ' 4 - Present Stock Status (Location & Item Wise) '
   @ 12, 22 PROMPT ' 5 - Stock Ledger Display                 '
   @ 13, 22 PROMPT ' 6 - Stock Ledger Printing                '
   @ 14, 22 PROMPT ' 7 - Stock Below Minimum Level            '
   @ 15, 22 PROMPT ' 8 - Stock Below Reording Level           '
   @ 16, 22 PROMPT ' 9 - Stock Above Maximum Level            '
   @ 18, 22 PROMPT '10 - Return to Main Menu                  '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-stat
      CASE MCHOICE=2
         DO stat-loc
      CASE MCHOICE=3
         DO stat-itm
      CASE MCHOICE=4
         DO statitem
      CASE MCHOICE=5
         DO rep-ledg
      CASE MCHOICE=6
         DO repledg
      CASE MCHOICE=7
         DO rep-min
      CASE MCHOICE=8
         DO re-ord
      CASE MCHOICE=9
         DO rep-max
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-STAT
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal INDEX itm-bal
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
CLEAR
NO= ' '
DO WHILE .not. NO$'YN'

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 2, 2 SAY 'Present Stock Status Printing/Display :'
   @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
   READ

IF _DOS
   SET COLOR TO 7/1
ENDIF

ENDDO
ABC= 0
DO WHILE ABC=0
   SELECT 1
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MAMT, MI1, MI2, MI3, MFI1, MFI2, MFI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   @ 12, 15 SAY 'From Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 29 GET MFI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY MP1
   @ 12, 31 SAY '-'
   @ 12, 32 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 32 SAY MP2
   @ 12, 34 SAY '-'
   @ 12, 35 GET MFI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MP3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO FITEM_CODE
   SELECT 2
   SEEK FITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   MI1= MFI1
   MI2= MFI2
   SELECT 1
   @ 14, 15 SAY 'To Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 SAY MI1 PICTURE '99'
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MP1
   @ 14, 31 SAY '-'
   @ 14, 32 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MP2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MP3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO EITEM_CODE
   SELECT 2
   SEEK EITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 40 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   COPY TO abc FOR LOC_CODE=MLOC_CODE.and.I1=MFI1.and.I2=MFI2.and.I3>=MFI3.and.I3<=MI3
   SELECT 1
   USE abc
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      IF NO='Y'
         @ L, 47 SAY 'P.Qty'
         @ L, 57 SAY 'Avg Price '
         @ L, 75 SAY 'Value'
      ELSE
         @ L, 60 SAY 'P.Qty'
      ENDIF
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 40 SAY UNIT
   ENDIF
   SELECT 1
   IF NO='Y'
      @ L, 46 SAY QTY PICTURE '9999999.9999'
      @ L, 59 SAY AVG_PRICE PICTURE '9999.9999'
      MTOT= 0
      MTOT= QTY*AVG_PRICE
      @ L, 68 SAY MTOT PICTURE '999999999.99'
      MAMT= MAMT+MTOT
   ELSE
      @ L, 58 SAY QTY PICTURE '9999999.9999'
   ENDIF
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
IF NO='Y'
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 50 SAY 'T O T A L '
   @ L, 65 SAY MAMT PICTURE '999999999999.99'
ENDIF
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN

PROCEDURE STAT-LOC
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal INDEX itm-bal
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
CLEAR
ABC= 0
DO WHILE ABC=0
   SELECT 1
   NO= ' '
   DO WHILE .not. NO$'YN'

IF _DOS
      SET COLOR TO +7/1
ENDIF

      @ 2, 2 SAY 'Present Stock Status Printing/Display :'
      @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
      READ

IF _DOS
      SET COLOR TO 7/1
ENDIF

   ENDDO
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MLOC_CODE, MAMT
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   COPY TO abc FOR LOC_CODE=MLOC_CODE
   SELECT 1
   USE abc
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      @ L, 47 SAY 'P.Qty'
      IF NO='Y'
         @ L, 57 SAY 'Avg Price '
         @ L, 75 SAY 'Value'
      ENDIF
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 40 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 46 SAY QTY PICTURE '9999999.9999'
   IF NO='Y'
      @ L, 59 SAY AVG_PRICE PICTURE '999999.99'
      MTOT= 0
      MTOT= QTY*AVG_PRICE
      @ L, 68 SAY MTOT PICTURE '999999999.99'
      MAMT= MAMT+MTOT
   ENDIF
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
IF NO='Y'
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 50 SAY 'T O T A L '
   @ L, 65 SAY MAMT PICTURE '999999999999.99'
ENDIF
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN

PROCEDURE STAT-ITM
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal INDEX itm-bal
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 5, 15 SAY ' PRESENT STOCK STATUS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MI1, MI2, MI3, TOT, MAMT
   @ 12, 15 SAY 'From Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 29 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY MP1
   @ 12, 31 SAY '-'
   @ 12, 32 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 32 SAY MP2
   @ 12, 34 SAY '-'
   @ 12, 35 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO FITEM_CODE
   SELECT 2
   SEEK FITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PRESENT STOCK STATUS REPORT (ITEM WISE)'
      ELSE
         DO screen
         @ 3, 25 SAY 'PRESENT STOCK STATUS REPORT (ITEM WISE) '
      ENDIF
      @ L, 2 SAY 'Item : '
      @ L, 9 SAY MP1 PICTURE '99'
      @ L, 11 SAY '-'
      @ L, 12 SAY MP2 PICTURE '99'
      @ L, 14 SAY '-'
      @ L, 15 SAY MP3 PICTURE '9999'
      @ L, 20 SAY MITEM
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 9 SAY 'Location '
      @ L, 40 SAY 'P.Qty'
      @ L, 57 SAY 'Avg Price '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MLOC_CODE= LOC_CODE
   MLC= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ L, 1 SAY MLC PICTURE '9999'
   SELECT 3
   SEEK MLOC_CODE
   IF FOUND()
      MLOC= LOC
   ELSE
      MLOC= ' '
   ENDIF
   SELECT 1
   @ L, 9 SAY MLOC
   @ L, 40 SAY QTY PICTURE '9999999.9999'
   @ L, 57 SAY AVG_PRICE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*AVG_PRICE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MAMT= MAMT+MTOT
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 50 SAY 'T O T A L '
@ L, 65 SAY MAMT PICTURE '999999999999.99'
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
CLOSE ALL
RETURN

PROCEDURE STATITEM
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal INDEX itm-bal
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
CLEAR
ABC= 0
DO WHILE ABC=0
   SELECT 1
   @ 1, 25 TO 3, 56 DOUBLE
   @ 2, 27 SAY ' PRESENT STOCK STATUS REPORT'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MAMT, MI1, MI2, MI3, MFI1, MFI2, MFI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   ABC= 1
ENDDO
@ 12, 15 SAY 'Item Code '

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 30 SAY '  -  '

IF _DOS
SET COLOR TO 7/1 
ENDIF

@ 12, 30 GET MI1 PICTURE '99'
READ
IF READKEY()=12
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
@ 23, 45 CLEAR TO 23, 78
MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 30 SAY MP1
@ 12, 32 SAY '-'
@ 12, 33 GET MI2 PICTURE '99'
READ
MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 33 SAY MP2

IF _DOS
SET COLOR TO 
ENDIF

SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE.and.I1=MI1.and.I2=MI2
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
SELECT 1
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      @ L, 47 SAY 'P.Qty'
      @ L, 57 SAY 'Avg Price '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 40 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 46 SAY QTY PICTURE '9999999.9999'
   @ L, 59 SAY AVG_PRICE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*AVG_PRICE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MAMT= MAMT+MTOT
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 50 SAY 'T O T A L '
@ L, 65 SAY MAMT PICTURE '999999999999.99'
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN

PROCEDURE REP-LEDG
CLEAR
CLEAR ALL
SET DATE brit
SELECT 1
USE stock
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE dept INDEX dept-idx
SELECT 4
USE item INDEX item-idx
SELECT 5
USE loc INDEX loc-idx
DO WHILE .T.
   SELECT 1
   CLEAR
   @ 2, 20 SAY '     STOCK  LEGDER  REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE
   @ 8, 20 SAY 'From Date    (dd/mm/yy) :' GET FDATE
   @ 10, 20 SAY 'To   Date    (dd/mm/yy) :' GET TDATE
   READ
   IF TDATE<FDATE
      @ 23, 25 SAY 'To Date Should be greater than From Date'
      WAIT ''
      @ 23, 25 SAY '                                        '
      LOOP
   ENDIF
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 12, 20 SAY 'Item  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY '  -  -    '
   @ 12, 28 GET MI1 PICTURE '99'
   READ
   MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 12, 28 SAY MCP1
   @ 12, 30 SAY '-'
   @ 12, 31 GET MI2 PICTURE '99'
   READ
   MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 12, 31 SAY MCP2
   @ 12, 33 SAY '-'
   @ 12, 34 GET MI3 PICTURE '9999'
   READ
   MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 34 SAY MCP3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 50 CLEAR TO 23, 78
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 4
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 25 SAY 'Item Code Does not Exist --- Re-try '
      WAIT ''
      @ 23, 23 CLEAR TO 23, 78
      LOOP
   ENDIF
   MITEM= ITEM

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   MT= 0
   DO WHILE MT=0
      @ 14, 20 SAY 'Location ' GET MLOC_CODE PICTURE '9999'
      READ
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 23, 25 SAY 'Location Code Does not Exist --- Re-try '
         WAIT ''
         @ 23, 23 CLEAR TO 23, 78
         LOOP
      ENDIF
      MLOC= LOC

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 40 SAY MLOC

IF _DOS
      SET COLOR TO 
ENDIF

      MT= 1
   ENDDO
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.LOC_CODE=MLOC_CODE
   GOTO TOP
   P= ' '
   DO WHILE .not. P$'S/P/Q'
      @ 23, 40 SAY 'Screen/Printing/Quit  -  S/P/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF P='P'
      @ 11, 20 SAY 'Please wait ... Printing in progress '
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   CLEAR
   L= 3
   HEADING= .T.
   PAGE= 0
   STORE 0 TO MTOT, MTOT1, MBAL
   SELECT 1
   DO WHILE .not. EOF()
      IF HEADING
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
         ELSE
            DO screen
         ENDIF
         @ 3, 25 SAY '  STOCK  LEDGER  DISPLAY'
         @ 4, 1 SAY 'From Date : '+DTOC(FDATE)
         @ 5, 1 SAY 'To   Date : '+DTOC(TDATE)
         @ 6, 1 SAY 'Item      : '
         MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 6, 13 SAY MCP1
         @ 6, 15 SAY '-'
         MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 6, 16 SAY MCP2
         @ 6, 18 SAY '-'
         MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 6, 19 SAY MCP3
         @ 6, 26 SAY MITEM
         @ 7, 1 SAY 'Location  : '+STR(MLOC_CODE,4)
         @ 7, 26 SAY MLOC
         @ 7, 68 SAY 'Page : '+STR(PAGE,2)
         @ 8, 1 SAY REPLICATE('-',79)
         @ 9, 1 SAY 'Date'
         @ 9, 10 SAY 'Doc #'
         @ 9, 20 SAY 'Description'
         @ 9, 38 SAY 'Inward'
         @ 9, 49 SAY 'Outward'
         @ 9, 61 SAY 'Balance'
         @ 9, 73 SAY 'Average'
         @ 10, 1 SAY REPLICATE('-',79)
         L= 11
         HEADING= .F.
      ENDIF
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MD1= D_D1
      MD2= D_D2
      MD3= D_D3
      MDEPT_CODE= DEPT_CODE
      @ L, 1 SAY DATE
      T= '    '
      IF DOC_TYPE=1
         T= 'GRN-'
      ENDIF
      IF DOC_TYPE=2
         T= 'PRN-'
      ENDIF
      IF DOC_TYPE=3
         T= 'IPN-'
      ENDIF
      IF DOC_TYPE=4
         T= 'IPR-'
      ENDIF
      IF DOC_TYPE=5
         T= 'PRD-'
      ENDIF
      IF DOC_TYPE=6
         T= 'INV-'
      ENDIF
      IF DOC_TYPE=7
         T= 'SRN-'
      ENDIF
      IF DOC_TYPE=8
         T= 'TRN-'
      ENDIF
      IF SR_NO=1
         @ L, 22 SAY '*** Opening Balance ***'
      ENDIF
      IF SR_NO<>1
         MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
         @ L, 10 SAY RTRIM(T)+MDOC_NO
         IF DOC_TYPE=8
            MF_T= F_T
            IF MF_T='F'
               MDES= 'From Location '
            ELSE
               MDES= 'To Location   '
            ENDIF
            @ L, 22 SAY MDES
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=7
            SELECT 2
            SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,15)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=2.or.DOC_TYPE=6
            SELECT 2
            SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,15)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=3.or.DOC_TYPE=4.or.DOC_TYPE=5
            SELECT 3
            SEEK MDEPT_CODE
            IF FOUND()
               MDEPT= LEFT(DEPT,15)
               @ L, 22 SAY MDEPT
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=8
            IF MF_T='T'
               @ L, 36 SAY T_QTY
               MTOT= MTOT+T_QTY
            ELSE
               @ L, 44 SAY T_QTY
               MTOT1= MTOT1+T_QTY
            ENDIF
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=4.or.DOC_TYPE=5.or.DOC_TYPE=7
            @ L, 36 SAY T_QTY
            MTOT= MTOT+T_QTY
         ENDIF
         IF DOC_TYPE=2.or.DOC_TYPE=3.or.DOC_TYPE=6
            @ L, 44 SAY T_QTY
            MTOT1= MTOT1+T_QTY
         ENDIF
      ENDIF
      @ L, 58 SAY QTY_BAL PICTURE '9999999.99'
      @ L, 70 SAY AVG_PRICE PICTURE '9999999.99'
      L= L+1
      SKIP
      IF P='P'.and.L>56
         HEADING= .T.
      ENDIF
      IF P='S'.and.L>21
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         L= 3
         HEADING= .T.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
   ENDDO
   @ L, 30 SAY REPLICATE('-',40)
   L= L+1
   @ L, 36 SAY MTOT PICTURE '99999999'
   @ L, 44 SAY MTOT1 PICTURE '99999999'
   IF P='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
      WAIT ''
   ENDIF
   IF P='P'
      EJECT
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
   SELECT 1
   SET FILTER TO
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REPLEDG
CLEAR
CLEAR ALL
SET DATE brit
SELECT 1
USE stock
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE dept INDEX dept-idx
SELECT 4
USE item INDEX item-idx
SELECT 5
USE loc INDEX loc-idx
DO WHILE .T.
   CLEAR
   SELECT 1
   @ 2, 20 SAY ' COMPLETE STOCK LEGDER REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE
   @ 8, 20 SAY 'From Date    (dd/mm/yy) :' GET FDATE
   @ 10, 20 SAY 'To   Date    (dd/mm/yy) :' GET TDATE
   READ
   IF TDATE<FDATE
      @ 23, 25 SAY 'To Date Should be greater than From Date'
      WAIT ''
      @ 23, 25 SAY '                                        '
      LOOP
   ENDIF
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 12, 20 SAY 'Item  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY '  -  -    '
   @ 12, 28 GET MI1 PICTURE '99'
   READ
   MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 12, 28 SAY MCP1
   @ 12, 30 SAY '-'
   @ 12, 31 GET MI2 PICTURE '99'
   READ
   MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 12, 31 SAY MCP2
   @ 12, 33 SAY '-'
   @ 12, 34 GET MI3 PICTURE '9999'
   READ
   MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 34 SAY MCP3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 50 CLEAR TO 23, 78
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 4
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 25 SAY 'Item Code Does not Exist --- Re-try '
      WAIT ''
      @ 23, 23 CLEAR TO 23, 78
      LOOP
   ENDIF
   MITEM= ITEM

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM
   MUNIT= UNIT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   MT= 0
   DO WHILE MT=0
      @ 14, 20 SAY 'Location ' GET MLOC_CODE PICTURE '9999'
      READ
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 23, 25 SAY 'Location Code Does not Exist --- Re-try '
         WAIT ''
         @ 23, 23 CLEAR TO 23, 78
         LOOP
      ENDIF
      MLOC= LOC

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 40 SAY MLOC

IF _DOS
      SET COLOR TO 
ENDIF

      MT= 1
   ENDDO
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.LOC_CODE=MLOC_CODE
   GOTO TOP
   P= ' '
   DO WHILE .not. P$'PQ'
      @ 23, 40 SAY 'Printing/Quit  -  P/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF P='P'
      @ 11, 20 SAY 'Please wait ... Printing in progress '
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   CLEAR
   L= 3
   HEADING= .T.
   PAGE= 0
   STORE 0 TO MTOT, MTOT1, MBAL
   SELECT 1
   DO WHILE .not. EOF()
      IF HEADING
         PAGE= PAGE+1
         DO top-prt
         @ 3, 25 SAY '  STOCK  LEDGER  DISPLAY'
         @ 4, 1 SAY 'From Date : '+DTOC(FDATE)
         @ 5, 1 SAY 'To   Date : '+DTOC(TDATE)
         @ 6, 1 SAY 'Item      : '
         MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 6, 13 SAY MCP1
         @ 6, 15 SAY '-'
         MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 6, 16 SAY MCP2
         @ 6, 18 SAY '-'
         MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 6, 19 SAY MCP3
         @ 6, 26 SAY MITEM
         @ 6, 60 SAY 'Unit : '
         @ 6, 67 SAY MUNIT
         @ 7, 1 SAY 'Location  : '+STR(MLOC_CODE,4)
         @ 7, 26 SAY MLOC
         @ 7, 68 SAY 'Page : '+STR(PAGE,2)
         L= 8
         @ L, 0 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 9 SAY 'Doc #'
         @ L, 22 SAY 'Description'
         @ L, 65 SAY 'Inward'
         @ L, 75 SAY 'Outward'
         @ L, 90 SAY 'Qty Bal'
         @ L, 104 SAY 'Average'
         @ L, 117 SAY 'Value (Rs.)'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MD1= D_D1
      MD2= D_D2
      MD3= D_D3
      MDEPT_CODE= DEPT_CODE
      @ L, 1 SAY DATE
      T= '    '
      IF DOC_TYPE=1
         T= 'GRN-'
      ENDIF
      IF DOC_TYPE=2
         T= 'PRN-'
      ENDIF
      IF DOC_TYPE=3
         T= 'IPN-'
      ENDIF
      IF DOC_TYPE=4
         T= 'IPR-'
      ENDIF
      IF DOC_TYPE=5
         T= 'PRD-'
      ENDIF
      IF DOC_TYPE=6
         T= 'INV-'
      ENDIF
      IF DOC_TYPE=7
         T= 'SRN-'
      ENDIF
      IF DOC_TYPE=8
         T= 'TRN-'
      ENDIF
      IF SR_NO=1
         @ L, 22 SAY '*** Opening Balance ***'
      ENDIF
      IF SR_NO<>1
         MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
         @ L, 10 SAY RTRIM(T)+MDOC_NO
         IF DOC_TYPE=8
            MF_T= F_T
            IF MF_T='F'
               MDES= 'From Location '
            ELSE
               MDES= 'To Location   '
            ENDIF
            @ L, 22 SAY MDES
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=7
            SELECT 2
            SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,35)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=2.or.DOC_TYPE=6
            SELECT 2
            SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,35)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=3.or.DOC_TYPE=4.or.DOC_TYPE=5
            SELECT 3
            SEEK MDEPT_CODE
            IF FOUND()
               MDEPT= LEFT(DEPT,15)
               @ L, 22 SAY MDEPT
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=8
            IF MF_T='T'
               @ L, 62 SAY T_QTY
               MTOT= MTOT+T_QTY
            ELSE
               @ L, 72 SAY T_QTY
               MTOT1= MTOT1+T_QTY
            ENDIF
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=4.or.DOC_TYPE=5.or.DOC_TYPE=7
            @ L, 62 SAY T_QTY
            MTOT= MTOT+T_QTY
         ENDIF
         IF DOC_TYPE=2.or.DOC_TYPE=3.or.DOC_TYPE=6
            @ L, 72 SAY T_QTY
            MTOT1= MTOT1+T_QTY
         ENDIF
      ENDIF
      @ L, 90 SAY QTY_BAL PICTURE '9999999.99'
      @ L, 105 SAY AVG_PRICE PICTURE '9999999.99'
      MVAL= 0
      MVAL= QTY_BAL*AVG_PRICE
      @ L, 119 SAY MVAL PICTURE '9999999.99'
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 0, 0 SAY CHR(18)
         HEADING= .T.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
   ENDDO
   @ L, 60 SAY REPLICATE('-',30)
   L= L+1
   @ L, 63 SAY MTOT PICTURE '99999999'
   @ L, 73 SAY MTOT1 PICTURE '99999999'
   L= L+1
   @ L, 60 SAY REPLICATE('-',30)
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   SELECT 1
   SET FILTER TO
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MIN
CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TO open-idx
SELECT 3
USE loc
INDEX ON LOC_CODE TO loc-idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK BELOW MINIMUM LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK BELOW MINIMUM LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 13 SAY 'Item'
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min'
      @ L, 71 SAY 'Max'
      @ L, 84 SAY 'Present'
      @ L, 95 SAY 'Reorder'
      @ L, 105 SAY 'Reorder'
      @ L, 117 SAY 'Last'
      L= L+1
      @ L, 61 SAY 'Level'
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty'
      @ L, 95 SAY 'Level'
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= QTY
   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      IF MQTY<MIN_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ELSE
         SELECT 2
         IF .not. EOF()
            SKIP
         ENDIF
         LOOP
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE RE-ORD
CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TO open-idx
SELECT 3
USE loc
INDEX ON LOC_CODE TO loc-idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK BELOW REORDING LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK BELOW REORDING LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Code '
      @ L, 13 SAY 'Item '
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min  '
      @ L, 71 SAY 'Max '
      @ L, 84 SAY 'Present  '
      @ L, 95 SAY 'Reorder  '
      @ L, 105 SAY 'Reorder  '
      @ L, 117 SAY 'Last '
      L= L+1
      @ L, 61 SAY 'Level '
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty '
      @ L, 95 SAY 'Level '
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= QTY
   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      IF MQTY<REORD_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ELSE
         SELECT 2
         IF .not. EOF()
            SKIP
         ENDIF
         LOOP
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MAX
CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TO open-idx
SELECT 3
USE loc
INDEX ON LOC_CODE TO loc-idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK ABOVE MAXIMUM LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK ABOVE MAXIMUM LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 13 SAY 'Descripition'
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min'
      @ L, 71 SAY 'Max'
      @ L, 84 SAY 'Present'
      @ L, 95 SAY 'Reorder'
      @ L, 105 SAY 'Reorder'
      @ L, 117 SAY 'Last'
      L= L+1
      @ L, 61 SAY 'Level'
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty'
      @ L, 95 SAY 'Level'
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(MLOC_CODE,4) TO XYZ
   SELECT 2
   SEEK XYZ
   MQTY= QTY
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO XYZ
   SELECT 1
   SEEK XYZ
   IF FOUND()
      IF MQTY>MAX_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   SKIP
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN5
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 23 SAY 'C O N S U M P T I O N   R E P O R T S '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 20 PROMPT ' 1 - Consumption Report (Item Wise) '
   @ 9, 20 PROMPT ' 2 - Consumption Report (Department Wise) '
   @ 10, 20 PROMPT ' 3 - Consumption Report (Date Wise) '
   @ 11, 20 PROMPT ' 4 - Consumption Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' 5 - Consumption Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' 9 - Return to Main Menu'
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-con
      CASE MCHOICE=2
         DO rep-con1
      CASE MCHOICE=3
         DO rep-con2
      CASE MCHOICE=4
         DO rep-con3
      CASE MCHOICE=5
         DO rep-con4
      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-CON
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MQTY, MTP, MI1, MI2, MI3
   MITEM= SPACE(30)
   @ 1, 22 TO 3, 59 DOUBLE
   @ 2, 24 SAY 'C O N S U M P T I O N   R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   @ 14, 15 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MPC1
   @ 14, 31 SAY '-'
   @ 14, 32 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MPC2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MPC3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 40 SAY ITEM
   MUNIT= UNIT
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
MTOT= 0
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'CONSUMPTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Item  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ELSE
         DO SCREEN
         @ 3, 8 SAY 'CONSUMPTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 11 SAY 'Department'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 62 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 11 SAY DEPT
   ENDIF
   @ L, 35 SAY MUNIT
   SELECT 1
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   MTOT= T_QTY*RATE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MQTY= MQTY+T_QTY
   MTP= MTP+MTOT
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 7
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL :'
@ L, 42 SAY MQTY PICTURE '9999999.9999'
@ L, 68 SAY MTP PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-CON1
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 4 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen
         @ 3, 22 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 63 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
@ L, 68 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-CON2
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 6
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'CONSUMPTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ELSE
         DO screen
         @ 3, 24 SAY 'CONSUMPTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 63 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 6
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 6
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
@ L, 68 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 3, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-CON3
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'CONSUMPTION REPORT (DEPT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen
         @ 3, 19 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 66 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-CON4
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'CONSUMPTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ELSE
         DO screen
         @ 3, 19 SAY 'CONSUMPTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN6
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1   
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 23 SAY '      PRODUCTION REPORTS  '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 8, 20 PROMPT ' 1 - Production Report (Item Wise) '
   @ 9, 20 PROMPT ' 2 - Production Report (Department Wise) '
   @ 10, 20 PROMPT ' 3 - Production Report (Date Wise) '
   @ 11, 20 PROMPT ' 4 - Production Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' 5 - Production Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' 9 - Return to Main Menu'
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep-pro
      CASE MCHOICE=2
         DO rep-pro1
      CASE MCHOICE=3
         DO rep-pro2
      CASE MCHOICE=4
         DO rep-pro3
      CASE MCHOICE=5
         DO rep-pro4
      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE REP-PRO
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MQTY, MTP, MI1, MI2, MI3
   MITEM= SPACE(30)
   @ 1, 22 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   @ 14, 15 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MPC1
   @ 14, 31 SAY '-'
   @ 14, 32 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MPC2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MPC3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 40 SAY ITEM
   MUNIT= UNIT
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
MTOT= 0
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'PRODUCTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Item  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ELSE
         DO SCREEN
         @ 3, 8 SAY 'PRODUCTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 11 SAY 'Department'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 11 SAY DEPT
   ENDIF
   @ L, 35 SAY MUNIT
   SELECT 1
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   MQTY= MQTY+T_QTY
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 7
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL :'
@ L, 42 SAY MQTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PRO1
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 4 SAY 'PRODUCTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen
         @ 3, 22 SAY 'PRODUCTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   QTY= QTY+T_QTY
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PRO2
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 6
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'PRODUCTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ELSE
         DO screen
         @ 3, 24 SAY 'PRODUCTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 6
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 6
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 3, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PRO3
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TO dept-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'PRODUCTION REPORT (DEPT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen
         @ 3, 19 SAY 'PRODUCTION REPORT (DEPARTMENT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-PRO4
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF ANS='P'
         DO top-prt
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'PRODUCTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ELSE
         DO screen
         @ 3, 19 SAY 'PRODUCTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE DENIED
CLEAR
ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)

IF _DOS
   SET COLOR TO *w
ENDIF

   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO

IF _DOS
SET COLOR TO 
ENDIF


CLEAR
RETURN

PROCEDURE CHK
SET DATE brit
SET CENT ON
IF MDATE>CTOD('07/07/2009')
   SET ESCAPE OFF
   CLOSE ALL
   CLEAR
   QUIT
ENDIF
RETURN
 
