* PROGRAM : ITEMPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* THIS REVISION: 15-12-2019
*************************************

******************
PROCEDURE ITEM_ENT
******************

CLEAR
CLOSE DATA

SELECT 1
USE item ORDER item_idx
SELECT 2
USE ldgrcode EXCLUSIVE
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1
STORE 0 TO MI1, MI2, MI3, CNT

********************************
* Main Loop
********************************

DO WHILE MI3 <= 9999

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
   
   ***************************************
   * Sub Loop
   ***************************************

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
         MUNIT = "PCS"
         @ 5, 10 SAY 'Unit          ' GET MUNIT FUNCTION '!'
         MREO_LEV = 10
         @ 7, 10 SAY 'Reording Level' GET MREO_LEV PICTURE '9999999'
         MREO_QTY = 10
         @ 9, 10 SAY 'Reording Qty  ' GET MREO_QTY PICTURE '9999999'
         READ

         MMIN_LEV = 10
         @ 7, 42 SAY 'Minimum Level.      ' GET MMIN_LEV PICTURE '9999999'
         READ

         MT= 0
         DO WHILE MT=0
            MMAX_LEV = 9999999
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

            MC1 = 51
            @ 11, 45 GET MC1 PICTURE '99'
            @ 11, 47 SAY '-'
            @ 11, 50 SAY '-'

IF _DOS
            SET COLOR TO 7/1
ENDIF
            MC2 = 1
            @ 11, 48 GET MC2 PICTURE '@z 99'
            MC3 = 1
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
            @ 15, 45 GET MLAST_PRIC PICTURE '999999999999.99'
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

               MCC1 = 51
               @ R, 16 GET MCC1 PICTURE '@z 99'

IF _DOS
               SET COLOR TO /w
ENDIF

               MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MCC1))))+LTRIM(STR(MCC1))
               @ R, 16 SAY MPC1
               MCC2 = 1
               @ R, 19 GET MCC2 PICTURE '@z 99'
               READ

               MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MCC2))))+LTRIM(STR(MCC2))
               @ R, 19 SAY MPC2
               MCC3 = 1
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

               MDC1 = 51
               @ R, 16 GET MDC1 PICTURE '@z 99'

IF _DOS
               SET COLOR TO /w
ENDIF

               MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MDC1))))+LTRIM(STR(MDC1))
               @ R, 16 SAY MPC1
               MDC2 = 1
               @ R, 19 GET MDC2 PICTURE '@z 99'
               READ

               MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MDC2))))+LTRIM(STR(MDC2))
               @ R, 19 SAY MPC2
               MDC3 = 1
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
      
* Updated on 12-12-2019      
      
      P= "N"
      
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

*Updated on 12-12-2019

      P= "Y"

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
   ************************************
   * End of Sub Loop
   ************************************
   MI3= MI3+1
ENDDO
*************************************
* End of Main Loop
*************************************


RETURN
******

******************
PROCEDURE ITEM_COR
******************
CLEAR
CLOSE DATA
SET EXCLUSIVE ON

SELECT 1
USE item ORDER item_idx
SELECT 2
USE ldgrcode ORDER ac1

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
	STORE SPACE(50) TO MNOTE1,MNOTE2,MNOTE3,MNOTE4


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

* updated on 13-12-2019

   IF MI3<>0
      @ 7, 10 SAY 'Unit          ' GET UNIT FUNCTION '!'
      @ 9, 10 SAY 'Reording Level' GET REORD_LEV PICTURE '9999999'
      @ 11, 10 SAY 'Reording Qty  ' GET REORD_QTY PICTURE '9999999'
      READ
      @ 9, 42 SAY 'Minimum Level.      ' GET MIN_LEV PICTURE '9999999'
      READ
      MT= 0
      DO WHILE MT=0
         @ 11, 42 SAY 'Maximum Level.      ' GET MAX_LEV PICTURE '9999999'
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
         @ 12, 10 SAY 'Last Purchased From (Party Code)    '

IF _DOS
         SET COLOR TO /w 
ENDIF

* updated on 13-12-2019

         @ 12, 45 GET MC1 PICTURE '99'
         @ 12, 47 SAY '-'
         @ 12, 50 SAY '-'

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ 12, 48 GET MC2 PICTURE '@z 99'
         @ 12, 51 GET MC3 PICTURE '@z 999'
         READ
         IF MC1=0.and.MC2=0.and.MC3=0
            EXIT
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 12, 45 SAY MPC1
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))

IF _DOS
         SET COLOR TO /w 
ENDIF

         @ 12, 48 SAY MPC2
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 12, 51 SAY MPC3

IF _DOS
         SET COLOR TO 7/1
ENDIF
		SET EXCLUSIVE ON
         SELECT 2
		 INDEX ON STR(c1,2)+STR(c2,2)+STR(C3,3) TAG ac1
		 SET ORDER TO ac1
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
         @ 15, 45 GET LAST_PRICE PICTURE '999999999999.99'
         READ
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         MT= 1
      ENDDO
      IF INT=1
         R= 16
         @ R, 1 TO R, 78
         R= R+1
         * R = 17
         @ R, 5 SAY 'Credit Code (For Sale Transaction) '
         R= R+1
         * R = 18
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
         * R = 19
         @ R, 5 SAY 'Debit Code (For Purchase Transaction) '
         R= R+1
         * R = 20
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

*updated 14-12-2019

	SELECT 1
	@ 17,60 SAY "Notes:"
	@ 18,60 GET NOTE1 SIZE 1,19
	@ 19,60 GET NOTE2 SIZE 1,19
	@ 20,60 GET NOTE3 SIZE 1,19
	@ 21,60 GET NOTE4 SIZE 1,19
	READ

*updated 15-12-2019
	
	SELECT 1
	@ 22,60 SAY "Memo Notes:"
	@ 23,60 GET notes
	READ

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
USE item ORDER item_idx
SELECT 2
USE ldgrcode ORDER ac1
SELECT 3
USE item-bal ORDER itmd_idx
SELECT 4
USE stock ORDER itemidx
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
CLOSE DATA

* AMENDED ON 06-06-2005

SELECT 1
USE item ALIAS item EXCLUSIVE
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG code
SET ORDER TO code

SELECT 2
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG code
SET ORDER TO code

SELECT 1
SET RELATION TO STR(i1,2)+STR(i2,2)+STR(i3,4) INTO item_bal ADDITIVE

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

         DO top_prt IN TBL_PROC.PRG

		IF _DOS
         @ 3, 15 SAY CHR(18)
		ENDIF

         @ 3, 33 SAY 'ITEM  CODES '
         @ 4, 32 SAY '----------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '999'
         L= L+1
      
      ELSE
         
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY 'ITEM  CODES '
         L= L+1
         @ L, 30 SAY '-----------  '
         CNT= 0
         MREC2= RECNO()
      
      ENDIF
      
      L= L+1
      @ L, 0 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 15 SAY 'Item'
      @ L, 55 SAY 'Unit'
      @ L, 60 SAY 'On-Hand'
      L= L+1
      @ L, 0 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   
   IF I3=0
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      @ L, 15 SAY ITEM
      MLEN= LEN(RTRIM(ITEM))
      L= L+1
      @ L, 1 SAY REPLICATE('-',15+MLEN)
   
   ENDIF
   
   IF I3<>0
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 3 SAY MPC1 PICTURE '99'
      @ L, 5 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 6 SAY MPC2 PICTURE '99'
      @ L, 8 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 9 SAY MPC3 PICTURE '9999'
   
* updated 14-12-2019

      @ L, 15 SAY ITEM SIZE 1,30
      @ L, 55 SAY UNIT
      @ L, 60 SAY item_bal.qty
   
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


IF _DOS
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
ENDIF

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
USE item ORDER item_idx
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
USE item ORDER item_idx
SELECT 2
USE ldgrcode ORDER ac1
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
         DO SCREEN IN TBL_PROC.PRG
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
USE item ORDER item_idx
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
         DO SCREEN IN TBL_PROC.PRG
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
