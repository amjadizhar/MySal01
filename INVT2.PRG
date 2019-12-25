

PROCEDURE TOP
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
@ 1, 1 SAY 'Gold Coin Plus Inventory '
@ 2, 0 TO 2, 79
@ 4, 0 TO 4, 79
@ 3, 18 SAY '         ASIA SNACKS (PVT) LIMITED. '
RETURN

PROCEDURE BOT
@ 22, 3 SAY 'Designed & Developed by:'
@ 23, 0 TO 23, 79 DOUBLE
@ 24, 3 SAY 'Logical Systems (Pvt) Limited, Lahore - Pakistan. Tel: (042) 872987, 879298 '
RETURN

PROCEDURE SCREEN
@ 1, 25 SAY ' ASIA SNACKS (PVT) LIMITED. '
@ 2, 25 SAY '--------------------------- '
RETURN

PROCEDURE TOP-PRT
@ 1, 0 SAY CHR(14)
@ 1, 6 SAY ' ASIA SNACKS (PVT) LIMITED. '
RETURN


********************
PROCEDURE TRAN_MEN
********************
DO WHILE .T.
   INT= 1
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY ' Daily Transaction Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Purchase / Purchase Returns    '
   @ L+1, M PROMPT ' 2 - Issue To Production / Returns  '
   @ L+2, M PROMPT ' 3 - Production Receipts            '
   @ L+3, M PROMPT ' 4 - Sales / Sales Returns          '
   @ L+4, M PROMPT ' 5 - Stock Transfer                 '
   @ L+5, M PROMPT ' 6 - Production Targets & Budgeting '
   @ L+7, M PROMPT ' 9 - Return to Previous Menu        '

   MCHOICE= 0

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1
         CLEAR
         DO purr_men

      CASE MCHOICE=2
         CLEAR
         DO issu_men

      CASE MCHOICE=3
         CLEAR
         DO prod_men

      CASE MCHOICE=4
         CLEAR
         DO sale_men

      CASE MCHOICE=5
         CLEAR
         DO stck_men

      CASE MCHOICE=6
         CLEAR
         DO ptag_men

      CASE MCHOICE=7
         CLEAR
         CLOSE ALL
         RETURN

   ENDCASE

ENDDO


RETURN


******************
PROCEDURE OPEN_MEN
******************

DO WHILE .T.

   L= 8
   M= 24

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY '      Opening Balance Menu   '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ L+0, M PROMPT ' 1 - Opening Balance .... Entry      '
   @ L+1, M PROMPT ' 2 - Opening Balance .... Correction '
   @ L+2, M PROMPT ' 3 - Opening Balance .... Deletion   '
   @ L+3, M PROMPT ' 4 - Opening Balance .... Printing   '
   @ L+4, M PROMPT ' 5 - Opening Balance (Location Wise) '
   @ L+5, M PROMPT ' 6 - Opening Balance (Location Wise) '
   @ L+6, M SAY '      Alphabetically Sorted Printing  '
   @ L+7, M PROMPT ' 7 - Opening Balance (Item Wise)     '
   @ L+8, M PROMPT ' 8 - Opening Balance (Location & Item Wise) '
   @ L+10, M PROMPT ' 9 - Return to Previous Menu         '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR
         DO open_ent

      CASE MCHOICE=2
         CLEAR
         DO open_cor

      CASE MCHOICE=3
         CLEAR
         DO open_del

      CASE MCHOICE=4
         CLEAR
         DO open_prt

      CASE MCHOICE=5
         CLEAR
         DO openprt

      CASE MCHOICE=6
         CLEAR
         DO openprt1

      CASE MCHOICE=7
         CLEAR
         DO openprt2

      CASE MCHOICE=8
         CLEAR
         DO openprt3

      CASE MCHOICE=9
         CLEAR
         CLOSE ALL
         RETURN

   ENDCASE

ENDDO


RETURN
***********************

PROCEDURE OPEN-ENT
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE loc INDEX loc-idx
SELECT 3
USE item-bal INDEX itm-bal
SELECT 4
USE stock
STORE 0 TO MI1, MI2, MI3
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   SET COLOR TO +7/1 
   @ 4, 27 SAY 'Opening Balances Entry '
   SET COLOR TO 7/1 
   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MQTY, CNT
   STORE DATE() TO MDATE
   DO WHILE .T.
      @ 21, 50 SAY 'Press Ctrl Q to Return'
      SET COLOR TO /w
      @ 8, 23 SAY '  -  -    '
      SET COLOR TO 7/1 
      @ 8, 10 SAY 'Item        '
      @ 8, 23 GET MI1 PICTURE '99'
      READ
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      SET COLOR TO /w 
      @ 8, 23 SAY MP1 PICTURE '99'
      @ 8, 25 SAY '-'
      @ 8, 26 GET MI2 PICTURE '99'
      READ
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      SET COLOR TO /w
      @ 8, 26 SAY MP2 PICTURE '99'
      @ 8, 28 SAY '-'
      @ 8, 29 GET MI3 PICTURE '9999'
      READ
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 8, 29 SAY MP3 PICTURE '9999'
      SET COLOR TO 7/1 
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 21, 49 CLEAR TO 21, 73
      IF MI1=0.and.MI2=0.and.MI3=0
         LOOP
      ENDIF
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
      SELECT 1
      SEEK MITEM_CODE
      IF .not. FOUND()
         @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 21, 19 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ 8, 35 SAY ITEM
      SET COLOR TO 7/1 
      IF MI3=0
         @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
         @ 13, 8 SAY '          will make it a consolidation head i.e you'
         @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
         @ 15, 8 SAY '          directly into this head.'
         @ 16, 8 SAY '          Hit any key to continue ..........'
         WAIT ''
         @ 12, 7 CLEAR TO 16, 63
         LOOP
      ENDIF
      @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 2
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 21, 19 SAY SPACE(52)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ 10, 35 SAY LOC
      SET COLOR TO 7/1 
      IF CNT=0
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            @ 21, 2 SAY 'Opening Balance For This Item At This Location Already Exists ...Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
         UNLOCK
         SELECT 4
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 12, 10 SAY 'Opening Balance Qty     ' GET MQTY PICTURE '9999999.9999'
      READ
      MT= 0
      DO WHILE MT=0
         @ 14, 10 SAY 'Average Price           ' GET MAVG_PRICE PICTURE '9999.9999'
         READ
         IF MAVG_PRICE<0
            LOOP
         ENDIF
         MT= 1
      ENDDO
      SET COLOR TO *+7/1 
      @ 16, 10 SAY 'Opening Balance Date '
      SET COLOR TO 7/1 
      @ 16, 35 GET MDATE
      READ
      DO CHK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 30 SAY ' Any Corrections - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      @ 23, 25 CLEAR TO 23, 72
      IF P='Y'
         LOOP
      ENDIF
      IF MQTY=0
         MAVG_PRICE= 0
      ENDIF
      SELECT 3
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE SR_NO WITH 1, QTY WITH MQTY, AVG_PRICE WITH MAVG_PRICE
      REPLACE OPEN_BAL WITH MQTY, OPEN_AVG WITH MAVG_PRICE
      UNLOCK
      SELECT 4
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE SR_NO WITH 1, DATE WITH MDATE
      REPLACE QTY_BAL WITH MQTY, AVG_PRICE WITH MAVG_PRICE
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

PROCEDURE OPEN-COR
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE loc INDEX loc-idx
SELECT 3
USE item-bal INDEX itm-bal
SELECT 4
USE stock INDEX stck-idx
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   SET COLOR TO +7/1 
   @ 4, 27 SAY 'Opening Balances Correction'
   SET COLOR TO 7/1 
   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MI1, MI2, MI3, MQTY
   STORE DATE() TO MDATE
   @ 21, 50 SAY 'Press Ctrl Q to Return'
   SET COLOR TO /w
   @ 8, 23 SAY '  -  -    '
   SET COLOR TO 7/1 
   @ 8, 10 SAY 'Item        '
   @ 8, 23 GET MI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   SET COLOR TO /w 
   @ 8, 23 SAY MP1 PICTURE '99'
   @ 8, 25 SAY '-'
   @ 8, 26 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   SET COLOR TO /w
   @ 8, 26 SAY MP2 PICTURE '99'
   @ 8, 28 SAY '-'
   @ 8, 29 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 8, 29 SAY MP3 PICTURE '9999'
   SET COLOR TO 7/1 
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 21, 49 CLEAR TO 21, 73
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   SELECT 1
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 8, 35 SAY ITEM
   SET COLOR TO 7/1 
   IF MI3=0
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
      LOOP
   ENDIF
   @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
   READ
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(52)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 10, 35 SAY LOC
   SET COLOR TO 7/1 
   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
   SELECT 3
   SEEK XYZ
   IF FOUND()
      MSR_NO= SR_NO
      IF MSR_NO<>1
         @ 14, 20 SAY 'Transactions  already  exist  for  this Item  at  '
         @ 15, 20 SAY 'this location.  Therefore  you  cannot  make any  '
         @ 16, 20 SAY 'change in its " Opening Balance ".  If you still  '
         @ 17, 20 SAY 'want to change the  Opening Balance of this item  '
         @ 18, 20 SAY 'please use UN-POST Option first.  '
         @ 20, 20 SAY 'Press any key to return ..... '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ELSE
      @ 21, 9 SAY 'Opening Balance For This Item At This Location Does Not Exist.'
      @ 22, 9 SAY 'Please use " Opening Balance - Entry "  Option .......'
      WAIT ''
      @ 14, 20 CLEAR TO 18, 63
      @ 21, 1 CLEAR TO 22, 79
      LOOP
   ENDIF
   MQTY= QTY
   @ 12, 10 SAY 'Opening Balance Qty     ' GET MQTY PICTURE '9999999.9999'
   READ
   MAVG_PRICE= AVG_PRICE
   @ 14, 10 SAY 'Average Price           ' GET MAVG_PRICE PICTURE '9999.9999'
   READ
   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ABC
   SELECT 4
   SEEK ABC
   IF FOUND()
      SET COLOR TO *+7/1 
      @ 16, 10 SAY 'Opening Balance Date '
      SET COLOR TO 7/1 
      SELECT 4
      MDATE= DATE
      @ 16, 35 GET MDATE
      READ
   ENDIF
   DO CHK
   IF MQTY=0
      MAVG_PRICE= 0
   ENDIF
   SELECT 3
   IF .not. RLOCK()
      rlock()
   ENDIF
   REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
   REPLACE SR_NO WITH 1, QTY WITH MQTY, AVG_PRICE WITH MAVG_PRICE
   REPLACE OPEN_BAL WITH MQTY, OPEN_AVG WITH MAVG_PRICE
   UNLOCK
   SELECT 4
   IF .not. RLOCK()
      rlock()
   ENDIF
   REPLACE SR_NO WITH 1, DATE WITH MDATE
   REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
   REPLACE QTY_BAL WITH MQTY, AVG_PRICE WITH MAVG_PRICE
   UNLOCK
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 23 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE OPEN-DEL
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE item INDEX item-idx
SELECT 2
USE loc INDEX loc-idx
SELECT 3
USE item-bal INDEX itm-bal
SELECT 4
USE stock INDEX stck-idx
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   SET COLOR TO +7/1 
   @ 4, 27 SAY 'Opening Balances Deletion'
   SET COLOR TO 7/1 
   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MI1, MI2, MI3, MQTY
   STORE DATE() TO MDATE
   @ 21, 50 SAY 'Press Ctrl Q to Return'
   SET COLOR TO /w
   @ 8, 23 SAY '  -  -    '
   SET COLOR TO 7/1 
   @ 8, 10 SAY 'Item        '
   @ 8, 23 GET MI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   SET COLOR TO /w 
   @ 8, 23 SAY MP1 PICTURE '99'
   @ 8, 25 SAY '-'
   @ 8, 26 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   SET COLOR TO /w
   @ 8, 26 SAY MP2 PICTURE '99'
   @ 8, 28 SAY '-'
   @ 8, 29 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 8, 29 SAY MP3 PICTURE '9999'
   SET COLOR TO 7/1 
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 21, 49 CLEAR TO 21, 73
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   SELECT 1
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 8, 35 SAY ITEM
   SET COLOR TO 7/1 
   IF MI3=0
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
      LOOP
   ENDIF
   @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
   READ
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(52)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 10, 35 SAY LOC
   SET COLOR TO 7/1 
   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
   SELECT 3
   SEEK XYZ
   IF FOUND()
      MSR_NO= SR_NO
      IF MSR_NO<>1
         @ 14, 20 SAY 'Transactions already exist  for  this Item at  '
         @ 15, 20 SAY 'this  location.  Therefore, you cannot Delete  '
         @ 16, 20 SAY 'its " Opening Balance " for this location. If  '
         @ 17, 20 SAY 'you still want to delete the  Opening Balance  '
         @ 18, 20 SAY 'of this item please use UN-POST Option first.  '
         @ 20, 20 SAY 'Press any key to return ..... '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ELSE
      @ 21, 9 SAY 'Opening Balance For This Item At This Location Does Not Exist.'
      @ 22, 9 SAY 'Please use " Opening Balance - Entry "  Option .......'
      WAIT ''
      @ 14, 20 CLEAR TO 18, 63
      @ 21, 1 CLEAR TO 22, 79
      LOOP
   ENDIF
   @ 12, 10 SAY 'Opening Balance Qty     '
   @ 14, 10 SAY 'Average Price           '
   SET COLOR TO /w
   @ 12, 35 SAY QTY PICTURE '9999999.9999'
   @ 14, 35 SAY AVG_PRICE PICTURE '9999.9999'
   SET COLOR TO 7/1 
   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ABC
   SELECT 4
   SEEK ABC
   IF FOUND()
      SET COLOR TO *+7/1 
      @ 16, 10 SAY 'Opening Balance Date '
      SET COLOR TO 7/1 
      SET COLOR TO /w
      @ 16, 35 SAY DATE
      SET COLOR TO 7/1 
   ENDIF
   DO CHK
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 30 SAY ' Are You Sure - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   @ 23, 29 CLEAR TO 23, 66
   IF P='Y'
      SELECT 4
      DELETE
      SELECT 3
      DELETE
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

PROCEDURE OPEN-PRT
SET COLOR TO 7/1 
CLEAR
CLOSE ALL
SELECT 3
USE loc INDEX loc-idx
SELECT 2
USE item INDEX item-idx
SELECT 1
USE item-bal INDEX itm-bal
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
NO= ' '
DO WHILE .not. NO$'YN'
   SET COLOR TO +7/1
   @ 2, 2 SAY 'Opening Balance Printing/Display :'
   @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
   READ
   SET COLOR TO 7/1
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
STORE 0 TO PAGE, CNT, MTOT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      IF NO='Y'
         @ L, 45 SAY 'Opening Qty'
         @ L, 60 SAY 'Avg Price'
         @ L, 74 SAY 'Amount'
      ELSE
         @ L, 55 SAY 'Opening Qty'
      ENDIF
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MLOC_CODE= LOC_CODE
   SELECT 3
   SEEK MLOC_CODE
   IF FOUND()
      MLOC= LOC
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',17+MLEN)
   L= L+1
   SELECT 1
   DO WHILE LOC_CODE=MLOC_CODE
      IF HEADING
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 2 SAY 'Item Code'
         @ L, 14 SAY 'Item'
         IF NO='Y'
            @ L, 45 SAY 'Opening Qty'
            @ L, 60 SAY 'Avg Price'
            @ L, 74 SAY 'Amount'
         ELSE
            @ L, 55 SAY 'Opening Qty'
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF LOC_CODE<>MLOC_CODE
         EXIT
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 2 SAY MPC1 PICTURE '99'
      @ L, 4 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 5 SAY MPC2 PICTURE '99'
      @ L, 7 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 8 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         MITEM= ITEM
      ENDIF
      SELECT 1
      MTOT= OPEN_BAL*OPEN_AVG
      @ L, 14 SAY LEFT(MITEM,20)
      IF NO='Y'
         @ L, 42 SAY OPEN_BAL PICTURE '9999999.9999'
         @ L, 57 SAY OPEN_AVG PICTURE '9999.99'
         @ L, 67 SAY MTOT PICTURE '9999999999.99'
      ELSE
         @ L, 52 SAY OPEN_BAL PICTURE '9999999.9999'
      ENDIF
      L= L+1
      SKIP
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
         L= 3
         HEADING= .T.
      ENDIF
      IF P='P'.and.L>56
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
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
      L= 2
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= 2
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

PROCEDURE OPENPRT
SET COLOR TO 7/1 
CLEAR
CLOSE ALL
SELECT 3
USE loc INDEX loc-idx
SELECT 2
USE item INDEX item-idx
SELECT 1
USE item-bal INDEX itm-bal
MT= 0
DO WHILE MT=0
   SET COLOR TO +7/1 
   @ 5, 20 SAY 'Opening Balance Printing '
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ
   SET COLOR TO /w
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1
   SET COLOR TO 7/1 
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 11, 20 SAY LOC
   MLOC= LOC
   SET COLOR TO 7/1 
   MT= 1
ENDDO
SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 45 SAY 'Opening Qty'
      @ L, 58 SAY 'Avg Price'
      @ L, 70 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 35 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 64 SAY MNET PICTURE '999999999999.99'
         L= L+1
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 45 SAY 'Opening Qty'
         @ L, 58 SAY 'Avg Price'
         @ L, 70 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 64 SAY MNET PICTURE '999999999999.99'
            L= L+1
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         MITEM= ITEM
      ENDIF
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(MITEM,20)
      @ L, 13 SAY MTM
      @ L, 45 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 58 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 67 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>14
         EXIT
      ENDIF
      IF P='P'.and.L>50
         EXIT
      ENDIF
   ENDDO
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   L= L+1
   @ L, 45 SAY 'Total Amount '
   @ L, 64 SAY MTOT PICTURE '999999999999.99'
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   IF P='S'.and.L>14
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 1 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>50
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 64 SAY PGTOT PICTURE '999999999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 64 SAY MNET PICTURE '999999999999.99'
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

PROCEDURE OPENPRT1
SET COLOR TO 7/1 
CLEAR
CLOSE ALL
SELECT 3
USE loc INDEX loc-idx
SELECT 2
USE item INDEX item-idx
SELECT 1
USE item-bal INDEX itm-bal
MT= 0
DO WHILE MT=0
   SET COLOR TO +7/1 
   @ 5, 20 SAY 'Opening Balance Printing '
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ
   SET COLOR TO /w
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1
   SET COLOR TO 7/1 
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 11, 20 SAY LOC
   MLOC= LOC
   SET COLOR TO 7/1 
   MT= 1
ENDDO
SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE
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
USE
SELECT 1
USE alpha
ZAP
APPEND FROM item-bal FOR LOC_CODE=MLOC_CODE
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   MI1= I1
   MI2= I2
   MI3= I3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MITEM= ITEM
   ENDIF
   SELECT 1
   REPLACE ITEM WITH MITEM
   SKIP
ENDDO
SELECT 1
INDEX ON ITEM TO itemidx
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 37 SAY 'Opening Qty'
      @ L, 50 SAY 'Avg Price'
      @ L, 62 SAY 'Amount'
      @ L, 72 SAY 'T.Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 45 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 63 SAY MNET PICTURE '9999999999999.99'
         L= L+1
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 37 SAY 'Opening Qty'
         @ L, 50 SAY 'Avg Price'
         @ L, 62 SAY 'Amount'
         @ L, 72 SAY 'T.Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 68 SAY MNET PICTURE '99999999.99'
            L= L+1
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(ITEM,20)
      @ L, 13 SAY MTM
      @ L, 35 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 49 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 57 SAY MAMT PICTURE '99999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>19
         EXIT
      ENDIF
      IF P='P'.and.L>56
         EXIT
      ENDIF
   ENDDO
   @ L, 68 SAY MTOT PICTURE '99999999.99'
   IF P='S'.and.L>19
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 70 SAY PGTOT PICTURE '999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 63 SAY MNET PICTURE '9999999999999.99'
      PGTOT= 0
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
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 68 SAY PGTOT PICTURE '99999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 68 SAY MNET PICTURE '99999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'.and.L>19
   L= 1
ENDIF
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 68 SAY PGTOT PICTURE '99999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 68 SAY MNET PICTURE '99999999.99'
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SELECT 1
ZAP
CLEAR
CLOSE ALL
RETURN

PROCEDURE OPENPRT2
SET COLOR TO 7/1 
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal INDEX itmd-idx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
ABC= 0
DO WHILE ABC=0
   CLEAR
   SET COLOR TO +7/1 
   @ 2, 27 SAY ' OPENING BALANCE (ITEM WISE)'
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MI1, MI2, MI3, TOT
   @ 12, 15 SAY 'Two Level Item '
   SET COLOR TO /w
   @ 12, 30 SAY '  -  '
   SET COLOR TO 7/1 
   @ 12, 30 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   SET COLOR TO /w
   @ 12, 30 SAY MP1
   @ 12, 32 SAY '-'
   @ 12, 33 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   SET COLOR TO /w
   @ 12, 33 SAY MP2
   SET COLOR TO 7/1
   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
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
L= 3
HEADING= .T.
STORE 0 TO PAGE
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT (ITEM WISE)'
      ELSE
         DO SCREEN
         @ 3, 25 SAY 'OPENfING BALANCE REPORT (ITEM WISE)'
      ENDIF
      @ L, 69 SAY 'Page -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Code '
      @ L, 9 SAY 'Location '
      @ L, 38 SAY 'Opening Qty'
      @ L, 53 SAY 'Avg Price'
      @ L, 67 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      MITEM= ITEM
   ELSE
      MITEM= ' '
   ENDIF
   SELECT 1
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   @ L, 7 SAY MP3
   @ L, 12 SAY MITEM
   MLEN= LEN(RTRIM(MITEM))
   L= L+1
   @ L, 1 SAY REPLICATE('-',20+MLEN)
   L= L+1
   DO WHILE I1=MI1.and.I2=MI2.and.I3=MI3
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT (ITEM WISE)'
         ELSE
            DO SCREEN
            @ 3, 25 SAY 'OPENfING BALANCE REPORT (ITEM WISE)'
         ENDIF
         @ L, 69 SAY 'Page -'
         @ L, 77 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Code '
         @ L, 9 SAY 'Location '
         @ L, 38 SAY 'Opening Qty'
         @ L, 53 SAY 'Avg Price'
         @ L, 67 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2.and.I3<>MI3
         EXIT
      ENDIF
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
      @ L, 38 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 53 SAY OPEN_AVG PICTURE '9999.99'
      MTOT= 0
      MTOT= OPEN_BAL*OPEN_AVG
      @ L, 67 SAY MTOT PICTURE '99999999.99'
      TOT= TOT+MTOT
      SKIP
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
ENDDO
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 67 SAY TOT PICTURE '99999999.99'
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

PROCEDURE OPENPRT3
SET COLOR TO 7/1 
CLEAR
CLOSE ALL
SELECT 3
USE loc INDEX loc-idx
SELECT 2
USE item INDEX item-idx
SELECT 1
USE item-bal INDEX itm-bal
MT= 0
DO WHILE MT=0
   SET COLOR TO +7/1 
   @ 5, 20 SAY 'Opening Balance Printing '
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT, MI1, MI2
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ
   SET COLOR TO /w
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1
   SET COLOR TO 7/1 
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ 11, 20 SAY LOC
   MLOC= LOC
   SET COLOR TO 7/1 
   MT= 1
ENDDO
@ 13, 20 SAY 'Item Code '
SET COLOR TO /w
@ 13, 36 SAY '  -  '
SET COLOR TO 7/1 
@ 13, 36 GET MI1 PICTURE '99'
READ
IF READKEY()=12
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
@ 23, 45 CLEAR TO 23, 78
MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
SET COLOR TO /w
@ 13, 36 SAY MP1
@ 13, 38 SAY '-'
@ 13, 39 GET MI2 PICTURE '99'
READ
MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
SET COLOR TO /w
@ 13, 39 SAY MP2
SET COLOR TO 
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
GOTO TOP
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 45 SAY 'Opening Qty'
      @ L, 58 SAY 'Avg Price'
      @ L, 70 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 35 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 64 SAY MNET PICTURE '999999999999.99'
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 45 SAY 'Opening Qty'
         @ L, 58 SAY 'Avg Price'
         @ L, 70 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 64 SAY MNET PICTURE '999999999999.99'
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         MITEM= ITEM
      ENDIF
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(MITEM,20)
      @ L, 13 SAY MTM
      @ L, 45 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 58 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 67 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>14
         EXIT
      ENDIF
      IF P='P'.and.L>50
         EXIT
      ENDIF
   ENDDO
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   L= L+1
   @ L, 45 SAY 'Total Amount '
   @ L, 64 SAY MTOT PICTURE '999999999999.99'
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   IF P='S'.and.L>14
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 1 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>50
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 64 SAY PGTOT PICTURE '999999999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 64 SAY MNET PICTURE '999999999999.99'
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

********************
PROCEDURE PURR_MEN
********************

DO WHILE .T.

   SET DELETED ON

   IF INT=1

      USE tran-key INDEX key-idx

      DELETE ALL FOR FLAG=' '

      CLEAR
      CLOSE ALL

   ENDIF


   USE st-key INDEX st-idx
   DELETE ALL FOR FLAG=' '

   CLEAR
   CLOSE ALL

   L= 9
   M= 23

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 23 SAY ' Purchase /  Purchase Return  Menu  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L-1, M PROMPT ' 1 - Purchase ............ Entry      '
   @ L+0, M PROMPT ' 2 - Purchase ............ Correction '
   @ L+1, M PROMPT ' 3 - Purchase ............ Deletion   '
   @ L+2, M PROMPT ' 4 - GRN      ............ Printing   '
   @ L+3, M PROMPT ' 5 - Verification Voucher  Printing   '
   @ L+4, M TO L+4, 61
   @ L+5, M PROMPT ' 6 - Purchase Return ..... Entry      '
   @ L+6, M PROMPT ' 7 - Purchase Return ..... Correction '
   @ L+7, M PROMPT ' 8 - Purchase Return ..... Deletion   '
   @ L+8, M PROMPT ' 9 - PR.NO    ............ Printing   '
   @ L+9, M PROMPT '10 - Verification Voucher  Printing   '
   @ L+11, M PROMPT '11 - Return to Previous Menu          '

   MCHOICE= 0


   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR
         DO pur_ent

      CASE MCHOICE=2
         CLEAR
         DO pur_cor

      CASE MCHOICE=3
         CLEAR
         DO pur_del

      CASE MCHOICE=4
         CLEAR
         DO pur_prt

      CASE MCHOICE=5
         CLEAR
         DO pur_vch

      CASE MCHOICE=6
         CLEAR
         DO purr_ent

      CASE MCHOICE=7
         CLEAR
         DO purr_cor

      CASE MCHOICE=8
         CLEAR
         DO purr_del

      CASE MCHOICE=9
         CLEAR
         DO purrprt

      CASE MCHOICE=10
         CLEAR
         DO purrvch

      CASE MCHOICE=11
         CLEAR
         CLOSE ALL
         RETURN

   ENDCASE

ENDDO


RETURN
*****************

PROCEDURE PUR-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 1
   MVCH_TYPE= 0
   T= '  '
   MVCH1= '  '
   MVCH2= '     '
   SET COLOR TO +7/1 
   @ 1, 30 SAY 'PURCHASE ENTRY '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MVCH_NO1, MVCH_N2
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MTOT1, MSR, MSR_NO
   DO WHILE .T.
      @ 24, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
      SET COLOR TO /w
      @ R, 15 SAY MDOC
      SET COLOR TO 7/1 
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This GRN No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      ABC= 0
      DO WHILE ABC=0
         IF INT=1
            @ 21, 1 CLEAR TO 24, 79
            @ 21, 1 SAY 'Voucher Types:'
            @ 21, 17 SAY '1 - '
            @ 21, 21 SAY 'Cash Receipt - '
            SET COLOR TO /w 
            @ 21, 36 SAY 'CR'
            SET COLOR TO 7/1 
            @ 22, 17 SAY '2 - '
            @ 22, 21 SAY 'Cash Payment - '
            SET COLOR TO /w 
            @ 22, 36 SAY 'CP'
            SET COLOR TO 7/1 
            @ 21, 58 SAY '3 - '
            @ 21, 62 SAY 'Bank Receipt - '
            SET COLOR TO /w 
            @ 21, 77 SAY 'BR'
            SET COLOR TO 7/1 
            @ 22, 58 SAY '4 - '
            @ 22, 62 SAY 'Bank Payment - '
            SET COLOR TO /w 
            @ 22, 77 SAY 'BP'
            SET COLOR TO 7/1 
            @ 23, 17 SAY '5 - '
            @ 23, 21 SAY 'Journal      - '
            SET COLOR TO /w 
            @ 23, 36 SAY 'JV'
            SET COLOR TO 7/1 
            @ 23, 40 SAY '6 - '
            @ 23, 44 SAY 'Sales - '
            SET COLOR TO /w 
            @ 23, 52 SAY 'SV'
            SET COLOR TO 7/1 
            @ 23, 58 SAY '7 - '
            @ 23, 62 SAY 'Purchase     - '
            SET COLOR TO /w 
            @ 23, 77 SAY 'PV'
            SET COLOR TO 7/1 
            @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
            READ
            IF MVCH_TYPE=0
               LOOP
            ENDIF
            IF MVCH_TYPE=1
               T= 'CR-'
            ENDIF
            IF MVCH_TYPE=2
               T= 'CP-'
            ENDIF
            IF MVCH_TYPE=3
               T= 'BR-'
            ENDIF
            IF MVCH_TYPE=4
               T= 'BP-'
            ENDIF
            IF MVCH_TYPE=5
               T= 'JV-'
            ENDIF
            IF MVCH_TYPE=6
               T= 'SV-'
            ENDIF
            IF MVCH_TYPE=7
               T= 'PV-'
            ENDIF
            SET COLOR TO /w
            @ R, 45 SAY T
            @ R, 48 SAY '  -     '
            SET COLOR TO 7/1
            @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MVCH_NO1=0
               LOOP
            ENDIF
            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
            SET COLOR TO /w
            @ R, 48 SAY MVCH1
            SET COLOR TO 7/1   
            SELECT 1
            USE tran-key INDEX key-idx
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1
            GOTO BOTTOM
            MVCH_NO2= VCH_NO2+1
            @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
            READ
            IF MVCH_NO2=0
               LOOP
            ENDIF
            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
            SET COLOR TO /w
            @ R, 51 SAY MVCH2
            SET COLOR TO 7/1
            SELECT 1
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               ?? CHR(7)
               @ 24, 1 SAY SPACE(78)
               @ 24, 15 SAY 'This Voucher Already Exists  -- Press any key'
               WAIT ''
               @ 24, 10 SAY SPACE(75)
               LOOP
            ELSE
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_TYPE WITH MVCH_TYPE
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
               UNLOCK
            ENDIF
            @ 21, 1 CLEAR TO 24, 79
            SELECT 1
            USE
         ENDIF
         ABC= 1
      ENDDO
      @ R, 60 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Credit / Party Code'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -   '
         SET COLOR TO 7/1 
         @ R, 23 GET MC1 PICTURE '99'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3
         SET COLOR TO 7/1 
         SELECT 4
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY TITLE
         MTITLE= TITLE
         SET COLOR TO 7/1 
         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
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
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1 
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
            SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
            IF INT=1
               IF MDC1=0.and.MDC2=0.and.MDC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /w
            @ R, 30 SAY MORD
            SET COLOR TO 7/1 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '999999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 51 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 65 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR= MSR+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         IF INT=1
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MTOT
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT, SR_NO WITH MSR
         UNLOCK
         USE
         MTOT1= MTOT1+MTOT
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
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            DO p-cor
         ENDIF
         IF INT=1
            SELECT 2
            USE tran INDEX tranent
            SELECT 1
            USE st-work
            GOTO TOP
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            GOTO TOP
            MC1= C_C1
            MC2= C_C2
            MC3= C_C3
            STORE 0 TO MSR_NO, MTOT1
            DO WHILE .not. EOF()
               MI1= I1
               MI2= I2
               MI3= I3
               MTOT= T_AMOUNT
               SELECT 3
               SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
               IF FOUND()
                  MDC1= DC1
                  MDC2= DC2
                  MDC3= DC3
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
               SELECT 2
               MSR_NO= MSR_NO+1
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
               REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
               REPLACE DR_CR WITH 'DR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
               REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
               UNLOCK
               MTOT1= MTOT1+MTOT
               SELECT 1
               SKIP
            ENDDO
            SELECT 1
            USE
            SELECT 2
            MSR_NO= MSR_NO+1
            APPEND BLANK
            IF .not. RLOCK()
               rlock()
            ENDIF
            REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DR_CR WITH 'CR', SR_NO WITH MSR_NO
            REPLACE DESCRIP WITH 'Purchase ', AMOUNT WITH MTOT1
            UNLOCK
            USE
            SELECT 1
            USE tran-key INDEX key-idx
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               REPLACE FLAG WITH 'Y'
            ENDIF
            USE
         ENDIF
         SELECT 2
         USE st-tran INDEX stidx, stidx1
         APPEND FROM st-work FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
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

PROCEDURE PUR-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
USE st-tran INDEX stidx1
USE st-tran INDEX stidx, stidx1
SET FILTER TO DOC_TYPE=1
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 1
   MVCH_TYPE= 7
   SET COLOR TO +7/1 
   @ 1, 30 SAY 'PURCHASE CORRECTION '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SET ORDER TO 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   IF INT=1
      @ 21, 1 CLEAR TO 24, 79
      @ 21, 1 SAY 'Voucher Types:'
      @ 21, 17 SAY '1 - '
      @ 21, 21 SAY 'Cash Receipt - '
      SET COLOR TO /w 
      @ 21, 36 SAY 'CR'
      SET COLOR TO 7/1 
      @ 22, 17 SAY '2 - '
      @ 22, 21 SAY 'Cash Payment - '
      SET COLOR TO /w 
      @ 22, 36 SAY 'CP'
      SET COLOR TO 7/1 
      @ 21, 58 SAY '3 - '
      @ 21, 62 SAY 'Bank Receipt - '
      SET COLOR TO /w 
      @ 21, 77 SAY 'BR'
      SET COLOR TO 7/1 
      @ 22, 58 SAY '4 - '
      @ 22, 62 SAY 'Bank Payment - '
      SET COLOR TO /w 
      @ 22, 77 SAY 'BP'
      SET COLOR TO 7/1 
      @ 23, 17 SAY '5 - '
      @ 23, 21 SAY 'Journal      - '
      SET COLOR TO /w 
      @ 23, 36 SAY 'JV'
      SET COLOR TO 7/1 
      @ 23, 40 SAY '6 - '
      @ 23, 44 SAY 'Sales - '
      SET COLOR TO /w 
      @ 23, 52 SAY 'SV'
      SET COLOR TO 7/1 
      @ 23, 58 SAY '7 - '
      @ 23, 62 SAY 'Purchase     - '
      SET COLOR TO /w 
      @ 23, 77 SAY 'PV'
      SET COLOR TO 7/1 
      @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
      READ
      IF MVCH_TYPE=0
         LOOP
      ENDIF
      IF MVCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF MVCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF MVCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF MVCH_TYPE=4
         T= 'BP-'
      ENDIF
      IF MVCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF MVCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF MVCH_TYPE=7
         T= 'PV-'
      ENDIF
      SET COLOR TO /w
      @ R, 45 SAY T
      @ R, 48 SAY '  -     '
      SET COLOR TO 7/1
      @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      SET COLOR TO /w
      @ R, 48 SAY MVCH1
      SET COLOR TO 7/1   
      @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      SET COLOR TO /w
      @ R, 51 SAY MVCH2
      SET COLOR TO 7/1
      SELECT 2
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 13 SAY 'This GRN No & Voucher No Does Not Exists ... Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 23, 10 SAY SPACE(80)
         LOOP
      ENDIF
      USE
      SELECT 2
      USE tran INDEX tranent
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 23, 10 SAY SPACE(75)
         MV_NO= 0
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
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=1
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Credit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO p-cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 DATE WITH MDATE
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C1 WITH MC1
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C2 WITH MC2
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C3 WITH MC3
   USE
   IF INT=1
      SELECT 2
      USE tran INDEX tranent
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 1
      USE st-cor
      GOTO TOP
      SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 PSR_TPRICE TO MTOT1
      GOTO TOP
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      STORE 0 TO MSR_NO, MTOT1
      DO WHILE .not. EOF()
         MI1= I1
         MI2= I2
         MI3= I3
         MTOT= T_AMOUNT
         SELECT 3
         SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
         IF FOUND()
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
         ENDIF
         SELECT 4
         SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
         IF FOUND()
            MDTITLE= TITLE
         ENDIF
         SELECT 2
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
         REPLACE DR_CR WITH 'DR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
         REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
         UNLOCK
         MTOT1= MTOT1+MTOT
         SELECT 1
         SKIP
      ENDDO
      SELECT 2
      MSR_NO= MSR_NO+1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE DR_CR WITH 'CR', SR_NO WITH MSR_NO
      REPLACE DESCRIP WITH 'Purchase ', AMOUNT WITH MTOT1
      UNLOCK
      USE
      SELECT 1
      USE
   ENDIF
   SELECT 2
   USE st-tran INDEX stidx, stidx1
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   APPEND FROM st-cor FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   SET FILTER TO DOC_TYPE=1
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 23, 50 SAY 'Total '
   @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
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

PROCEDURE PUR-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
USE st-tran INDEX stidx1
USE st-tran INDEX stidx, stidx1
SET FILTER TO DOC_TYPE=1
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 1
   MVCH_TYPE= 7
   SET COLOR TO +7/1 
   @ 1, 30 SAY 'PURCHASE DELETION '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SET ORDER TO 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   IF INT=1
      @ 21, 1 CLEAR TO 24, 79
      @ 21, 1 SAY 'Voucher Types:'
      @ 21, 17 SAY '1 - '
      @ 21, 21 SAY 'Cash Receipt - '
      SET COLOR TO /w 
      @ 21, 36 SAY 'CR'
      SET COLOR TO 7/1 
      @ 22, 17 SAY '2 - '
      @ 22, 21 SAY 'Cash Payment - '
      SET COLOR TO /w 
      @ 22, 36 SAY 'CP'
      SET COLOR TO 7/1 
      @ 21, 58 SAY '3 - '
      @ 21, 62 SAY 'Bank Receipt - '
      SET COLOR TO /w 
      @ 21, 77 SAY 'BR'
      SET COLOR TO 7/1 
      @ 22, 58 SAY '4 - '
      @ 22, 62 SAY 'Bank Payment - '
      SET COLOR TO /w 
      @ 22, 77 SAY 'BP'
      SET COLOR TO 7/1 
      @ 23, 17 SAY '5 - '
      @ 23, 21 SAY 'Journal      - '
      SET COLOR TO /w 
      @ 23, 36 SAY 'JV'
      SET COLOR TO 7/1 
      @ 23, 40 SAY '6 - '
      @ 23, 44 SAY 'Sales - '
      SET COLOR TO /w 
      @ 23, 52 SAY 'SV'
      SET COLOR TO 7/1 
      @ 23, 58 SAY '7 - '
      @ 23, 62 SAY 'Purchase     - '
      SET COLOR TO /w 
      @ 23, 77 SAY 'PV'
      SET COLOR TO 7/1 
      @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
      READ
      IF MVCH_TYPE=0
         LOOP
      ENDIF
      IF MVCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF MVCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF MVCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF MVCH_TYPE=4
         T= 'BP-'
      ENDIF
      IF MVCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF MVCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF MVCH_TYPE=7
         T= 'PV-'
      ENDIF
      SET COLOR TO /w
      @ R, 45 SAY T
      @ R, 48 SAY '  -     '
      SET COLOR TO 7/1
      @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      SET COLOR TO /w
      @ R, 48 SAY MVCH1
      SET COLOR TO 7/1   
      @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      SET COLOR TO /w
      @ R, 51 SAY MVCH2
      SET COLOR TO 7/1
      SELECT 2
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 13 SAY 'This GRN No & Voucher No Does Not Exists ... Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 23, 10 SAY SPACE(80)
         LOOP
      ENDIF
      USE
      SELECT 2
      USE tran INDEX tranent
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 23, 10 SAY SPACE(75)
         MV_NO= 0
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
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=1
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Credit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MORDER_NO= ORDER_NO
      MQTY= T_QTY
      MRATE= RATE
      MAMOUNT= T_AMOUNT
      SET COLOR TO /w
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
      SET COLOR TO 7/1 
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
      @ R, 13 SAY ITEM
      SELECT 1
      SET COLOR TO /w
      @ R, 30 SAY MORDER_NO PICTURE '99999'
      @ R, 36 SAY MQTY PICTURE '999999999.9999'
      @ R, 51 SAY MRATE PICTURE '99999999.9999'
      @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'
      SET COLOR TO 7/1 
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
   IF ANS='Y'
      IF INT=1
         SELECT 2
         USE tran INDEX tranent
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE tran-key INDEX key-idx
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
      ENDIF
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
      SELECT 2
      USE st-tran INDEX stidx, stidx1
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SET FILTER TO DOC_TYPE=1
      GOTO TOP
   ENDIF
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 24, 20 CLEAR TO 24, 78
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

PROCEDURE P-COR
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pur-dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT, MSR
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1 
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
            @ R, 13 SAY ITEM
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
            IF INT=1
               IF MDC1=0.and.MDC2=0.and.MDC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
               SET COLOR TO /w
               @ R, 30 SAY MORD
               SET COLOR TO 7/1 
               SELECT 6
               STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '999999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 51 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 65 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         IF INT=1
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= RATE
         MAMOUNT= T_AMOUNT
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 7/1 
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO /w
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
         SET COLOR TO 7/1 
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
         @ R, 13 SAY ITEM
         MDC1= DC1
         MDC2= DC2
         MDC3= DC3
         IF INT=1
            IF MDC1=0.and.MDC2=0.and.MDC3=0
               @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
               @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
               WAIT ''
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            SELECT 4
            SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
            IF FOUND()
               MDTITLE= TITLE
            ENDIF
         ENDIF
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /w
            @ R, 30 SAY MORD
            SET COLOR TO 7/1 
            SELECT 6
            STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '999999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 51 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 65 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      IF INT=1
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE PUR-DSP
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= RATE
   MAMOUNT= T_AMOUNT
   SET COLOR TO /w
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
   SET COLOR TO 7/1 
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
   @ R, 13 SAY ITEM
   SELECT 1
   SET COLOR TO /w
   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '999999999.9999'
   @ R, 51 SAY MRATE PICTURE '99999999.9999'
   @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'
   SET COLOR TO 7/1 
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN

PROCEDURE PUR-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Goods Receipt Note '
   @ 4, 33 SAY '-------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From GRN No : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   GRN No : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=1
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
            DO top-prt
            @ 3, 30 SAY 'Goods Receipt Note '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ELSE
            DO screen
            @ 3, 30 SAY 'Goods Receipt Note '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
         @ L, 1 SAY 'GRN No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 30 SAY 'Voucher No : '
         @ L, 43 SAY 'PV-'
         @ L, 46 SAY MVCH1
         @ L, 48 SAY '-'
         @ L, 49 SAY MVCH2
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MC1= C_C1
         MC2= C_C2
         MC3= C_C3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Credit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 4
         SEEK MCODE
         IF FOUND()
            @ L, 30 SAY TITLE
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
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
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
      MRATE= RATE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE PUR-VCH
CLEAR
CLOSE ALL
SELECT 1
USE st-tran INDEX stidx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   SET COLOR TO 7/1 
   CLEAR
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=1
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
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
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      L= 3
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 10 SAY 'Vch # '
      @ L, 20 SAY 'GRN # '
      @ L, 30 SAY 'Party'
      @ L, 59 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 10 SAY 'Vch # '
         @ L, 23 SAY 'GRN # '
         @ L, 30 SAY 'Party'
         @ L, 60 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MVCH1= '  '
      MVCH2= '     '
      MDOCNO= '       '
      MDOCNO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ L, 10 SAY 'PV-'
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MDOCNO
      MC_C1= C_C1
      MC_C2= C_C2
      MC_C3= C_C3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MC_C1,2)+STR(MC_C2,2)+STR(MC_C3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 30 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 60 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= RATE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 120 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE PURR-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 2
   SET COLOR TO +7/1 
   @ 1, 28 SAY 'PURCHASE RETURN ENTRY '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MSR_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MAMT
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Purchase Return No  ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1 
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This P.R. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Debit / Party Code'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -   '
         SET COLOR TO 7/1 
         @ R, 23 GET MC1 PICTURE '99'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3
         SET COLOR TO 7/1 
         SELECT 4
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY TITLE
         MTITLE= TITLE
         SET COLOR TO 7/1 
         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
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
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1 
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
            SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /w
            @ R, 30 SAY MORD
            SET COLOR TO 7/1 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
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
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE SR_NO WITH MSR_NO
         UNLOCK
         USE
         MAMT= MAMT+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MAMT PICTURE '9999999999.99'
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
            DO pr-cor
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
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

PROCEDURE PURR-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=2
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 2
   SET COLOR TO +7/1 
   @ 1, 28 SAY 'PURCHASE RETURN CORRECTION'
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'P.R. No     ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Debit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO pr-cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D1 WITH MC1
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D2 WITH MC2
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D3 WITH MC3
   USE
   SELECT 2
   USE st-tran INDEX stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=2
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

PROCEDURE PURR-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=2
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 2
   SET COLOR TO +7/1 
   @ 1, 28 SAY 'PURCHASE RETURN DELETION'
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'P.R. No     ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Debit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   MREC2= RECNO()
   DO pr-dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran INDEX stidx
   IF ANS='Y'
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=2
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE PR-COR
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pr-dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO sal-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1
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
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
               SET COLOR TO /w
               @ R, 30 SAY MORD
               SET COLOR TO 7/1 
               SELECT 6
               STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 7/1
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO /w
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
         SET COLOR TO 7/1
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
         @ R, 13 SAY ITEM
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /w
            @ R, 30 SAY MORD
            SET COLOR TO 7/1
            SELECT 6
            STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE PR-DSP
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE
   SET COLOR TO /w
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
   SET COLOR TO 7/1
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
   @ R, 13 SAY ITEM
   SELECT 1
   SET COLOR TO /w
   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'
   SET COLOR TO 7/1 
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN

PROCEDURE PURRPRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Purchase Return Note '
   @ 4, 33 SAY '-------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 2
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From P.R.#  : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   P.R.#  : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=MDOC_TYPE
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
            DO top-prt
            @ 3, 30 SAY 'Purchase Return Note '
            L= L+1
            @ L, 30 SAY '-------------------- '
            L= L+1
         ELSE
            DO screen
            @ 3, 30 SAY 'Purchase Return Note '
            L= L+1
            @ L, 30 SAY '-------------------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'P.R. No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MC1= D_D1
         MC2= D_D2
         MC3= D_D3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Debit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 4
         SEEK MCODE
         IF FOUND()
            @ L, 30 SAY TITLE
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
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
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
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE PURRVCH
CLEAR
CLOSE ALL
SELECT 1
USE st-tran INDEX stidx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   SET COLOR TO 7/1
   CLEAR
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=2
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
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
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      L= 3
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'PR. # '
      @ L, 26 SAY 'Party'
      @ L, 57 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 3
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'PR. # '
         @ L, 26 SAY 'Party'
         @ L, 57 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MDOCNO= REPLICATE('0',5-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY 'PR-'
      @ L, 16 SAY MDOCNO
      MD_D1= D_D1
      MD_D2= D_D2
      MD_D3= D_D3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MD_D1,2)+STR(MD_D2,2)+STR(MD_D3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 57 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 119 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE ISSU-MEN
DO WHILE .T.
   SET DELETED ON
   USE st-key INDEX st-idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 19
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1 
   @ 6, 25 SAY ' Issue To Production / Returns '
   SET COLOR TO 7/1
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
         DO issu-ent
      CASE MCHOICE=2
         CLEAR
         DO issu-cor
      CASE MCHOICE=3
         CLEAR
         DO issu-del
      CASE MCHOICE=4
         CLEAR
         DO issu-prt
      CASE MCHOICE=5
         CLEAR
         DO issuent
      CASE MCHOICE=6
         CLEAR
         DO ispr-ent
      CASE MCHOICE=7
         CLEAR
         DO ispr-cor
      CASE MCHOICE=8
         CLEAR
         DO ispr-del
      CASE MCHOICE=9
         CLEAR
         DO ispr-prt
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE ISSU-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 3
   SET COLOR TO +7/1
   @ 1, 28 SAY 'ISSUE TO PRODUCTION ENTRY '
   SET COLOR TO 7/1
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
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
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1
         SET COLOR TO 7/1
         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY DEPT
         MDEPT= DEPT
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 33 SAY LOC
         MLOC= LOC
         SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 13 SAY ITEM
            SET COLOR TO 7/1
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
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
            DO p-cor1
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
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

PROCEDURE ISSU-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=3
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 3
   SET COLOR TO +7/1
   @ 1, 28 SAY 'ISSUE TO PRODUCTION CORRECTION '
   SET COLOR TO 7/1
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /W
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
   USE
   SELECT 2
   USE st-tran INDEX stidx
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

PROCEDURE ISSU-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=3
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 3
   SET COLOR TO +7/1
   @ 1, 28 SAY 'ISSUE TO PRODUCTION DELETION '
   SET COLOR TO 7/1
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
      SET COLOR TO /w
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
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 13 SAY ITEM
      SET COLOR TO 7/1
      SELECT 1
      SET COLOR TO /w
      @ R, 45 SAY MQTY PICTURE '9999999.9999'
      SET COLOR TO 7/1
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
   USE st-tran INDEX stidx
   IF ANS='Y'
      SELECT 2
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key INDEX st-idx
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

PROCEDURE ISSU-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
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
            DO top-prt
            @ 3, 30 SAY 'Issue Note '
            L= L+1
            @ L, 30 SAY '---------- '
            L= L+1
         ELSE
            DO screen
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
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE ISSUENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
HEAD= .T.
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   MDOC_TYPE= 3
   SET COLOR TO +7/1
   @ 1, 28 SAY 'ISSUE TO PRODUCTION ENTRY '
   SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
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
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1
         SET COLOR TO 7/1
         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY DEPT
         MDEPT= DEPT
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 33 SAY LOC
         MLOC= LOC
         SET COLOR TO 7/1
         ABC= 1
      ENDDO
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ 23, 50 SAY 'Press Ctrl Q To Quit '
         @ R, 2 SAY 'Finished Item No ..'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -    '
         SET COLOR TO 7/1
         @ R, 23 GET MFI1 PICTURE '99'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         SET COLOR TO /w
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
            SET COLOR TO 7/1
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
         @ R, 29 SAY MP3
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 38 SAY ITEM
         SET COLOR TO 7/1
         MUNIT= UNIT
         SELECT 8
         USE formula INDEX for-idx1
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
         SET COLOR TO /w
         @ R, 44 SAY MUNIT
         SET COLOR TO 7/1
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
               SET COLOR TO +7/1
               @ R, 10 SAY 'Location Code'
               @ R, 30 SAY 'Item Code'
               SET COLOR TO 7/1
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
         USE
         SELECT 6
         SKIP
      ENDDO
      SELECT 2
      USE st-tran INDEX stidx
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      USE
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
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

PROCEDURE P-COR1
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pur-dsp1
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp1
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
      IF EOF()
         EXIT
      ENDIF
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 13 SAY ITEM
            SET COLOR TO 7/1
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            DO WHILE .T.
               @ R, 45 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 7/1
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO /w
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
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 13 SAY ITEM
         SET COLOR TO 7/1
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         DO WHILE .T.
            @ R, 45 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, DATE WITH MDATE
      REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      SKIP
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE PUR-DSP1
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= T_QTY
   SET COLOR TO /w
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
   SET COLOR TO 7/1
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
   SET COLOR TO /w
   @ R, 13 SAY ITEM
   SET COLOR TO 7/1
   SELECT 1
   SET COLOR TO /w
   @ R, 45 SAY MQTY PICTURE '9999999.9999'
   SET COLOR TO 7/1
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN

PROCEDURE ISPR-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 4
   SET COLOR TO +7/1
   @ 1, 28 SAY 'ISSUE TO PRODUCTION RETURN ENTRY '
   SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 10 SAY 'This Production Return No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1
         SET COLOR TO 7/1
         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY DEPT
         MDEPT= DEPT
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 33 SAY LOC
         MLOC= LOC
         SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 13 SAY ITEM
            SET COLOR TO 7/1
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
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
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
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

PROCEDURE ISPR-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=4
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 4
   SET COLOR TO +7/1
   @ 1, 25 SAY 'ISSUE TO PRODUCTION RETURN CORRECTION '
   SET COLOR TO 7/1
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /W
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
   USE
   SELECT 2
   USE st-tran INDEX stidx
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

PROCEDURE ISPR-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=4
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 4
   SET COLOR TO +7/1
   @ 1, 25 SAY 'ISSUE TO PRODUCTION RETURN DELETION '
   SET COLOR TO 7/1
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This Issue No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
      SET COLOR TO /w
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
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 13 SAY ITEM
      SET COLOR TO 7/1
      SELECT 1
      SET COLOR TO /w
      @ R, 45 SAY MQTY PICTURE '9999999.9999'
      SET COLOR TO 7/1
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
   USE st-tran INDEX stidx
   IF ANS='Y'
      SELECT 2
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key INDEX st-idx
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

PROCEDURE ISPR-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This I. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
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
            DO top-prt
            @ 3, 30 SAY 'Issue Return Note '
            L= L+1
            @ L, 30 SAY '----------------- '
            L= L+1
         ELSE
            DO screen
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
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE PROD-MEN
DO WHILE .T.
   SET DELETED ON
   USE st-key INDEX st-idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 23
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 28 SAY '  Production  Receipts  '
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 - Production Receipts .. Entry      '
   @ L+1, M PROMPT ' 2 - Production Receipts .. Correction '
   @ L+2, M PROMPT ' 3 - Production Receipts .. Deletion   '
   @ L+3, M PROMPT ' 4 - Production Receipts .. Printing   '
   @ L+4, M PROMPT ' 5 - Production Receipts &             '
   @ L+5, M SAY '      Issue to Production with Formula  '
   @ L+7, M PROMPT ' 9 - Return to Previous Menu           '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=6
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=5
         CLEAR
         DO prodent
      CASE MCHOICE=4
         CLEAR
         DO prod-prt
      CASE MCHOICE=3
         CLEAR
         DO prod-del
      CASE MCHOICE=2
         CLEAR
         DO prod-cor
      CASE MCHOICE=1
         CLEAR
         DO prod-ent
   ENDCASE
ENDDO
RETURN

PROCEDURE PROD-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 5
   SET COLOR TO +7/1
   @ 1, 28 SAY 'PRODUCTION RECEIPTS ENTRY '
   SET COLOR TO 7/1
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This P.R. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1
         SET COLOR TO 7/1
         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY DEPT
         MDEPT= DEPT
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 33 SAY LOC
         MLOC= LOC
         SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 7/1
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO /w
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
            SET COLOR TO 7/1
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
            SET COLOR TO /w
            @ R, 13 SAY ITEM
            SET COLOR TO 7/1
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
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
            @ R, 78 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
         UNLOCK
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
            SELECT 1
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO p-cor1
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
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

PROCEDURE PROD-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=5
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 5
   SET COLOR TO +7/1
   @ 1, 26 SAY 'PRODUCTION RECEIPTS CORRECTION '
   SET COLOR TO 7/1
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MNAVG
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This P.R. No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /W
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
   USE
   SELECT 2
   USE st-tran INDEX stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=5
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

PROCEDURE PROD-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=5
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 5
   SET COLOR TO +7/1
   @ 1, 28 SAY 'PRODUCTION RECEIPTS DELETION '
   SET COLOR TO 7/1
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
   SET COLOR TO /w
   @ R, 23 SAY MVCH_NO
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This P.R. No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1
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
   DO chk
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1
      SET COLOR TO 7/1
      SELECT 4
      SEEK MDEPT_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY DEPT
      MDEPT= DEPT
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 33 SAY LOC
      MLOC= LOC
      SET COLOR TO 7/1
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
      SET COLOR TO /w
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
      SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ R, 13 SAY ITEM
      SET COLOR TO 7/1
      SELECT 1
      SET COLOR TO /w
      @ R, 45 SAY MQTY PICTURE '9999999.9999'
      SET COLOR TO 7/1
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
   USE st-tran INDEX stidx
   IF ANS='Y'
      SELECT 2
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=5
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

PROCEDURE PROD-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Production Receipt Note '
   @ 4, 33 SAY '----------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 5
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From P.R. Note : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   P.R. Note : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. Note No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=5
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
            DO top-prt
            @ 3, 30 SAY 'Production Receipt '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ELSE
            DO screen
            @ 3, 30 SAY 'Production Receipt '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'P.R. Note No  : '
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
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE PRODENT
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE dept INDEX dept-idx
SELECT 5
USE loc INDEX loc-idx
SELECT 7
USE item-bal INDEX itm-bal
HEAD= .T.
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 3
   MDOC_TYPE= 5
   SET COLOR TO +7/1
   @ 1, 28 SAY 'PRODUCTION RECEIPTS ENTRY '
   SET COLOR TO 7/1
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   STORE 0 TO MFI1, MFI2, MFI3, FMQTY, Q, MDOC_NO1
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
      SET COLOR TO /w
      @ R, 23 SAY MVCH_NO
      SET COLOR TO 7/1
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This P.R. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 53 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, 2 SAY 'Issue To Production ' GET MDOC_NO1 PICTURE '9999999'
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
         SET COLOR TO /w
         @ R, 23 SAY MVCH_NO1
         SET COLOR TO 7/1
         MDOC_TYPE1= 3
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO1,7)+STR(MDOC_TYPE1,1)
         IF FOUND()
            @ 24, 1 SAY SPACE(78)
            @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
            WAIT ''
            SET COLOR TO 7/1 
            @ 24, 10 SAY SPACE(75)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO1, DOC_TYPE WITH MDOC_TYPE1
         UNLOCK
         SELECT 2
         USE
         MT= 1
      ENDDO
      MDOC_TYPE= 5
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1
         SET COLOR TO 7/1
         SELECT 4
         SEEK MDEPT_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY DEPT
         MDEPT= DEPT
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 33 SAY LOC
         MLOC= LOC
         SET COLOR TO 7/1
         ABC= 1
      ENDDO
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ 23, 50 SAY 'Press Ctrl Q To Quit '
         @ R, 2 SAY 'Finished Item No ..'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -    '
         SET COLOR TO 7/1
         @ R, 23 GET MFI1 PICTURE '99'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         SET COLOR TO /w
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
            SET COLOR TO 7/1
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
         @ R, 29 SAY MP3
         SET COLOR TO 7/1
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
         SET COLOR TO /w
         @ R, 38 SAY ITEM
         SET COLOR TO 7/1
         MUNIT= UNIT
         SELECT 8
         USE formula INDEX for-idx1
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
         SET COLOR TO /w
         @ R, 44 SAY MUNIT
         SET COLOR TO 7/1
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
      DELETE ALL
      APPEND FROM formula FOR FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
      GOTO TOP
      STORE 0 TO MSR_NO
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
               SET COLOR TO +7/1
               @ R, 10 SAY 'Location Code'
               @ R, 30 SAY 'Item Code'
               SET COLOR TO 7/1
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
      SELECT 1
      USE st-work
      MSR_NO= 1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
      REPLACE DEPT_CODE WITH MDEPT_CODE, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MFI1, I2 WITH MFI2, I3 WITH MFI3
      REPLACE T_QTY WITH FMQTY, SR_NO WITH MSR_NO
      UNLOCK
      USE
      SELECT 6
      GOTO TOP
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
         REPLACE DOC_NO WITH MDOC_NO1, DOC_TYPE WITH 3, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         REPLACE T_QTY WITH MQTY*FMQTY, SR_NO WITH MSR_NO
         UNLOCK
         USE
         SELECT 6
         SKIP
      ENDDO
      SELECT 2
      USE st-tran INDEX stidx
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE1.and.DOC_NO=MDOC_NO
      USE
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
      ENDIF
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE1,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
      ENDIF
      USE
      SELECT 1
      USE st-work
      DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      DELETE ALL FOR DOC_TYPE=MDOC_TYPE1.and.DOC_NO=MDOC_NO
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
         RUN del for-work.dbf
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN

PROCEDURE SALE-MEN
DO WHILE .T.
   SET DELETED ON
   IF INT=1
      USE tran-key INDEX key-idx
      DELETE ALL FOR FLAG=' '
      CLEAR
      CLOSE ALL
   ENDIF
   USE st-key INDEX st-idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 23
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY '    Sales / Sale Returns  '
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 - Sales ............ Entry      '
   @ L+1, M PROMPT ' 2 - Sales ............ Correction '
   @ L+2, M PROMPT ' 3 - Sales ............ Deletion   '
   @ L+3, M PROMPT ' 4 - Invoice .......... Printing   '
   @ L+4, M PROMPT ' 5 - Verification Voucher Printing '
   @ L+5, M TO L+5, 61
   @ L+6, M PROMPT ' 6 - Sales Return ..... Entry      '
   @ L+7, M PROMPT ' 7 - Sales Return ..... Correction '
   @ L+8, M PROMPT ' 8 - Sales Return ..... Deletion   '
   @ L+9, M PROMPT ' 9 - SR. NO  .......... Printing   '
   @ L+10, M PROMPT '10 - Verification Voucher Printing '
   @ L+12, M PROMPT '11 - Return to Previous Menu       '
   DO bot
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=11
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=10
         CLEAR
         DO salr-vch
      CASE MCHOICE=9
         CLEAR
         DO salr-prt
      CASE MCHOICE=8
         CLEAR
         DO salr-del
      CASE MCHOICE=7
         CLEAR
         DO salr-cor
      CASE MCHOICE=6
         CLEAR
         DO salr-ent
      CASE MCHOICE=5
         CLEAR
         DO sale-vch
      CASE MCHOICE=4
         CLEAR
         DO sale-prt
      CASE MCHOICE=3
         CLEAR
         DO sale-del
      CASE MCHOICE=2
         CLEAR
         DO sale-cor
      CASE MCHOICE=1
         CLEAR
         DO sale-ent
   ENDCASE
ENDDO
RETURN

PROCEDURE SALE-ENT
CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE s-order INDEX sord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   CLEAR
   R= 3
   T= '  '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 6
   MVCH_TYPE= 6
   @ 1, 30 SAY 'S A L E   E N T R Y '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO, MCITY_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MTOT1, MZONE_CODE
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Invoice No  ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
      SET COLOR TO /g
      @ R, 15 SAY MDOC
      SET COLOR TO 
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This INV No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      ABC= 0
      DO WHILE ABC=0
         IF INT=1
            @ R, 28 SAY 'Voucher No .... '
            T= 'SV-'
            SET COLOR TO /w
            @ R, 45 SAY T
            @ R, 48 SAY '  -     '
            SET COLOR TO 7/1
            @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MVCH_NO1=0
               LOOP
            ENDIF
            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
            SET COLOR TO /w
            @ R, 48 SAY MVCH1
            SET COLOR TO 7/1   
            SELECT 2
            USE tran-key INDEX key-idx
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1
            GOTO BOTTOM
            MVCH_NO2= VCH_NO2+1
            @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
            READ
            IF MVCH_NO2=0
               LOOP
            ENDIF
            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
            SET COLOR TO /w
            @ R, 51 SAY MVCH2
            SET COLOR TO 7/1
            SELECT 2
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               ?? CHR(7)
               @ 24, 1 SAY SPACE(78)
               @ 24, 15 SAY 'This Voucher Already Exists  -- Press any key'
               WAIT ''
               @ 24, 10 SAY SPACE(75)
               LOOP
            ELSE
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_TYPE WITH MVCH_TYPE
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
               UNLOCK
            ENDIF
            SELECT 2
            USE
         ENDIF
         ABC= 1
      ENDDO
      @ R, 60 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Debit  / Party Code'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -   '
         SET COLOR TO 
         @ R, 23 GET MC1 PICTURE '99'
         READ
         SET COLOR TO n/g
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3
         SET COLOR TO 
         SELECT 4
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY TITLE
         MTITLE= TITLE
         SET COLOR TO 
         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
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
         @ R, 25 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      ABC= 0
      DO WHILE ABC=0
         @ R, 57 SAY 'City '
         @ R, 62 GET MCITY_CODE PICTURE '999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         SET COLOR TO /w
         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
         @ R, 62 SAY MPC1
         SET COLOR TO 
         IF MCITY_CODE=0
            LOOP
         ENDIF
         SELECT 1
         USE city INDEX city-idx
         SEEK MCITY_CODE
         IF .not. FOUND()
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 14 CLEAR TO 23, 73
            LOOP
         ENDIF
         @ R, 66 SAY CITY
         MZONE_CODE= ZONE_CODE
         ABC= 1
      ENDDO
      SELECT 1
      USE
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
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
            @ R, 13 SAY ITEM
            MCC1= CC1
            MCC2= CC2
            MCC3= CC3
            IF INT=1
               IF MCC1=0.and.MCC2=0.and.MCC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
               IF FOUND()
                  MCTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /g
            @ R, 30 SAY MORD
            SET COLOR TO 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 2
         USE st-work
         MSR= MSR+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 6, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE, SR_NO WITH MSR
         REPLACE RATE WITH MRATE, T_AMOUNT WITH MTOT
         UNLOCK
         USE
         MTOT1= MTOT1+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            IF R>=21
               @ 9, 0 CLEAR TO 24, 79
               R= 9
            ENDIF
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            DO S-cor
         ENDIF
         IF INT=1
            SELECT 2
            USE tran INDEX tranent
            SELECT 1
            USE st-work
            GOTO TOP
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 T_AMOUNT TO MTOT1
            GOTO TOP
            MC1= D_D1
            MC2= D_D2
            MC3= D_D3
            SELECT 2
            MSR_NO= 1
            APPEND BLANK
            IF .not. RLOCK()
               rlock()
            ENDIF
            REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DR_CR WITH 'DR', SR_NO WITH MSR_NO
            REPLACE DESCRIP WITH 'Sales ', AMOUNT WITH MTOT1
            UNLOCK
            SELECT 1
            DO WHILE .not. EOF()
               MI1= I1
               MI2= I2
               MI3= I3
               MTOT= T_AMOUNT
               SELECT 3
               SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
               IF FOUND()
                  MDC1= CC1
                  MDC2= CC2
                  MDC3= CC3
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
               SELECT 2
               MSR_NO= MSR_NO+1
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
               REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
               REPLACE DR_CR WITH 'CR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
               REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
               UNLOCK
               SELECT 1
               SKIP
            ENDDO
            SELECT 2
            USE
            SELECT 1
            USE
            SELECT 1
            USE tran-key INDEX key-idx
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               REPLACE FLAG WITH 'Y'
            ENDIF
            USE
         ENDIF
         SELECT 2
         USE st-tran INDEX stidx, stidx1
         APPEND FROM st-work FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
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

PROCEDURE SALE-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
USE st-tran INDEX stidx1
USE st-tran INDEX stidx, stidx1
SET FILTER TO DOC_TYPE=6
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 6
   MVCH_TYPE= 6
   SET COLOR TO +7/1 
   @ 1, 28 SAY 'S A L E   C O R R E C T I O N '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'INV No      ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SET ORDER TO 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   T= 'SV-'
   SET COLOR TO /w
   @ R, 45 SAY T
   @ R, 48 SAY '  -     '
   SET COLOR TO 7/1
   @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ R, 48 SAY MVCH1
   SET COLOR TO 7/1   
   @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ R, 51 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 13 SAY 'This INV No & Voucher No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(80)
      LOOP
   ENDIF
   SELECT 2
   USE
   SELECT 2
   USE tran INDEX tranent
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
      MV_NO= 0
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
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE
      MCITY_CODE= CITY_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Credit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
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
      @ R, 25 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   ABC= 0
   DO WHILE ABC=0
      @ R, 57 SAY 'City '
      @ R, 62 GET MCITY_CODE PICTURE '999'
      READ
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
      @ R, 62 SAY MPC1
      SET COLOR TO 
      IF MCITY_CODE=0
         LOOP
      ENDIF
      SELECT 2
      USE city INDEX city-idx
      SEEK MCITY_CODE
      IF .not. FOUND()
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 14 CLEAR TO 23, 73
         LOOP
      ENDIF
      @ R, 66 SAY CITY
      MZONE_CODE= ZONE_CODE
      ABC= 1
   ENDDO
   SELECT 2
   USE
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO S-cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 DATE WITH MDATE
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D1 WITH MC1
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D2 WITH MC2
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D3 WITH MC3
   USE
   IF INT=1
      SELECT 2
      USE tran INDEX tranent
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 1
      USE st-cor
      GOTO TOP
      SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 PSR_TPRICE TO MTOT1
      GOTO TOP
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      SELECT 2
      MSR_NO= 1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE DR_CR WITH 'DR', SR_NO WITH MSR_NO
      REPLACE DESCRIP WITH 'Sales ', AMOUNT WITH MTOT1
      UNLOCK
      SELECT 1
      DO WHILE .not. EOF()
         MI1= I1
         MI2= I2
         MI3= I3
         MTOT= PSR_TPRICE
         SELECT 3
         SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
         IF FOUND()
            MDC1= CC1
            MDC2= CC2
            MDC3= CC3
         ENDIF
         SELECT 4
         SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
         IF FOUND()
            MDTITLE= TITLE
         ENDIF
         SELECT 2
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
         REPLACE DR_CR WITH 'CR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
         REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
         UNLOCK
         SELECT 1
         SKIP
      ENDDO
      SELECT 2
      USE
      SELECT 1
      USE
   ENDIF
   SELECT 2
   USE st-tran INDEX stidx, stidx1
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   APPEND FROM st-cor FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   SET FILTER TO DOC_TYPE=6
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 23, 50 SAY 'Total '
   @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
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

PROCEDURE SALE-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
USE st-tran INDEX stidx1
USE st-tran INDEX stidx, stidx1
SET FILTER TO DOC_TYPE=6
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 6
   MVCH_TYPE= 6
   SET COLOR TO +7/1 
   @ 1, 28 SAY 'S A L E   D E L E T I O N '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'INV No      ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SET ORDER TO 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   T= 'SV-'
   SET COLOR TO /w
   @ R, 45 SAY T
   @ R, 48 SAY '  -     '
   SET COLOR TO 7/1
   @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ R, 48 SAY MVCH1
   SET COLOR TO 7/1   
   @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ R, 51 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 13 SAY 'This INV No & Voucher No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(80)
      LOOP
   ENDIF
   SELECT 2
   USE
   SELECT 2
   USE tran INDEX tranent
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
      MV_NO= 0
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
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE
      MCITY_CODE= CITY_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Credit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
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
      @ R, 25 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   ABC= 0
   DO WHILE ABC=0
      @ R, 57 SAY 'City '
      @ R, 62 GET MCITY_CODE PICTURE '999'
      READ
      SET COLOR TO /w
      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
      @ R, 62 SAY MPC1
      SET COLOR TO 
      IF MCITY_CODE=0
         LOOP
      ENDIF
      SELECT 2
      USE city INDEX city-idx
      SEEK MCITY_CODE
      IF .not. FOUND()
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 14 CLEAR TO 23, 73
         LOOP
      ENDIF
      @ R, 66 SAY CITY
      MZONE_CODE= ZONE_CODE
      ABC= 1
   ENDDO
   SELECT 2
   USE
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MORDER_NO= ORDER_NO
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMOUNT= PSR_TPRICE
      SET COLOR TO /w
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
      SET COLOR TO 7/1 
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
      @ R, 13 SAY ITEM
      SELECT 1
      SET COLOR TO /w
      @ R, 30 SAY MORDER_NO PICTURE '99999'
      @ R, 36 SAY MQTY PICTURE '999999999.9999'
      @ R, 51 SAY MRATE PICTURE '99999999.9999'
      @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'
      SET COLOR TO 7/1 
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
   IF ANS='Y'
      IF INT=1
         SELECT 2
         USE tran INDEX tranent
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE tran-key INDEX key-idx
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
      ENDIF
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
      SELECT 2
      USE st-tran INDEX stidx, stidx1
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SET FILTER TO DOC_TYPE=6
      GOTO TOP
   ENDIF
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 24, 20 CLEAR TO 24, 78
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

PROCEDURE S-COR
SELECT 1
GOTO TOP
MREC2= RECNO()
DO sal-dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO sal-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
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
            @ R, 13 SAY ITEM
            MCC1= CC1
            MCC2= CC2
            MCC3= CC3
            IF INT=1
               IF MCC1=0.and.MCC2=0.and.MCC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
               IF FOUND()
                  MCTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
               SET COLOR TO /g
               @ R, 30 SAY MORD
               SET COLOR TO 
               SELECT 6
               STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
         REPLACE RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO n/g
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
         SET COLOR TO 
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
         @ R, 13 SAY ITEM
         MCC1= CC1
         MCC2= CC2
         MCC3= CC3
         IF INT=1
            IF MCC1=0.and.MCC2=0.and.MCC3=0
               @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
               @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
               WAIT ''
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            SELECT 4
            SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
            IF FOUND()
               MCTITLE= TITLE
            ENDIF
         ENDIF
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /g
            @ R, 30 SAY MORD
            SET COLOR TO 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
      REPLACE RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE SAL-DSP
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE
   SET COLOR TO /w
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
   SET COLOR TO 
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
   @ R, 13 SAY ITEM
   SELECT 1
   SET COLOR TO /w
   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'
   SET COLOR TO 
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN

PROCEDURE SALE-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Invoice Printing Range '
   @ 4, 33 SAY '---------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 6
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From INV No : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   INV No : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   MNO= 1
   @ 14, 20 SAY 'No Of Copies ... ' GET MNO PICTURE '9'
   READ
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=6
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
MCOPY= 1
DO WHILE MCOPY<=MNO
   HEAD= .T.
   CLEAR
   SELECT 1
   GOTO TOP
   DO WHILE .not. EOF()
      MDOC_NO= DOC_NO
      STORE 0 TO MTOT
      DO WHILE DOC_NO=MDOC_NO
         IF HEAD
            L= 3
            IF P='P'
               DO top-prt
               @ 3, 35 SAY 'INVOICE '
               L= L+1
               @ L, 35 SAY '------- '
               L= L+1
            ELSE
               DO screen
               @ 3, 30 SAY 'I N V O I C E '
               L= L+1
               @ L, 30 SAY '------------- '
               L= L+1
               CNT= 0
               MREC2= RECNO()
            ENDIF
            MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
            @ L, 1 SAY 'INV No       : '
            @ L, 18 SAY MVCH_NO
            @ L, 30 SAY 'Voucher No : '
            @ L, 43 SAY 'SV-'
            @ L, 46 SAY MVCH1
            @ L, 48 SAY '-'
            @ L, 49 SAY MVCH2
            @ L, 65 SAY 'Date : '+DTOC(DATE)
            L= L+1
            MC1= D_D1
            MC2= D_D2
            MC3= D_D3
            MLOC_CODE= LOC_CODE
            @ L, 1 SAY 'Credit/Party : '
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            @ L, 18 SAY MPC1
            @ L, 20 SAY '-'
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            @ L, 21 SAY MPC2
            @ L, 23 SAY '-'
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
            @ L, 24 SAY MPC3
            STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
            SELECT 4
            SEEK MCODE
            IF FOUND()
               @ L, 30 SAY TITLE
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
            @ L, 31 SAY 'Unit'
            @ L, 37 SAY 'Order #'
            @ L, 51 SAY 'Qty '
            @ L, 59 SAY 'Rate '
            @ L, 74 SAY 'Amount'
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
         MRATE= RATE
         MAMT= MQTY*MRATE
         @ L, 33 SAY MUNIT
         @ L, 37 SAY ORDER_NO
         @ L, 41 SAY MQTY PICTURE '9999999.9999'
         @ L, 53 SAY MRATE PICTURE '9999999.9999'
         @ L, 66 SAY MAMT PICTURE '999999999.99'
         MTOT= MTOT+MAMT
         L= L+1
         SKIP
         CNT= CNT+1
         IF P='S'.and.L>20
            @ 23, 1 SAY REPLICATE('-',79)
            @ 24, 5 SAY 'For Next Page Press :'
            @ 24, 30 SAY 'For Previous Page :'
            @ 24, 60 SAY 'Quit :'
            SET COLOR TO /w
            @ 24, 27 SAY CHR(25)
            @ 24, 50 SAY CHR(24)
            @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
            SET COLOR TO 7/1
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
      @ L, 50 SAY REPLICATE('-',30)
      L= L+1
      @ L, 54 SAY 'Total : '
      @ L, 65 SAY MTOT PICTURE '999999999.99'
      L= L+1
      @ L, 50 SAY REPLICATE('-',30)
      L= L+1
      IF P='S'
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
      SELECT 1
   ENDDO
   MCOPY= MCOPY+1
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

PROCEDURE SALE-VCH
CLEAR
CLOSE ALL
SELECT 1
USE st-tran INDEX stidx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=6
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
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
L= 3
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 10 SAY 'Vch # '
      @ L, 20 SAY 'Inv # '
      @ L, 30 SAY 'Party'
      @ L, 59 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 10 SAY 'Vch # '
         @ L, 20 SAY 'Inv # '
         @ L, 30 SAY 'Party'
         @ L, 59 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      MVCH1= '  '
      MVCH2= '     '
      MDOCNO= '       '
      MDOCNO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ L, 1 SAY DATE
      @ L, 10 SAY 'SV-'
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MDOCNO
      MD_D1= D_D1
      MD_D2= D_D2
      MD_D3= D_D3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MD_D1,2)+STR(MD_D2,2)+STR(MD_D3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 30 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 59 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 120 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE SALR-ENT
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE s-order INDEX sord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN ENTRY '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MAMT
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'S.R No    '
      @ R, 15 GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
      SET COLOR TO /g
      @ R, 15 SAY MVCH_NO
      SET COLOR TO 
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This S.R. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Credit / Party Code'
         SET COLOR TO /w
         @ R, 23 SAY '  -  -   '
         SET COLOR TO 
         @ R, 23 GET MC1 PICTURE '99'
         READ
         SET COLOR TO n/g
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3
         SET COLOR TO 
         SELECT 4
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w 
         @ R, 33 SAY TITLE
         MTITLE= TITLE
         SET COLOR TO 
         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
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
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 45 SAY 'Qty '
      @ R, 55 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
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
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /g
            @ R, 30 SAY MORD
            SET COLOR TO 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
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
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 7, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE SR_NO WITH MSR_NO
         UNLOCK
         USE
         MAMT= MAMT+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            IF R>=21
               @ 9, 0 CLEAR TO 24, 79
               R= 9
            ENDIF
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MAMT PICTURE '9999999999.99'
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
            DO sr-cor
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
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

PROCEDURE SALR-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=7
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE s-order INDEX sord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN CORRECTION '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'S.R. No     ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Debit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO sr-cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C1 WITH MC1
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C2 WITH MC2
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C3 WITH MC3
   USE
   SELECT 2
   USE st-tran INDEX stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=7
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

PROCEDURE SALR-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=7
GOTO TOP
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN DELETION '
   SET COLOR TO 7/1 
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'S.R. No     ' GET MDOC_NO PICTURE '9999999'
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
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
   SET COLOR TO /w
   @ R, 15 SAY MDOC
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does Not Exists ... Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(75)
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
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE
      SET COLOR TO 7/1 
      @ R, 2 SAY 'Debit / Party Code'
      SET COLOR TO /w
      @ R, 23 SAY '  -  -   '
      SET COLOR TO 7/1 
      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3
      SET COLOR TO 7/1 
      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ R, 33 SAY TITLE
      MTITLE= TITLE
      SET COLOR TO 7/1 
      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '
      SET COLOR TO /w 
      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ
      SET COLOR TO 7/1 
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   MREC2= RECNO()
   DO sr-dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran INDEX stidx
   IF ANS='Y'
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=7
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE SALR-PRT
CLEAR
CLOSE ALL
SELECT 3
USE item INDEX item-idx
SELECT 4
USE ldgrcode INDEX ac1
SELECT 5
USE loc INDEX loc-idx
SELECT 6
USE p-order INDEX pord-idx
SELECT 7
USE item-bal INDEX itm-bal
MUNIT= '    '
SET COLOR TO 7/1 
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Sale Return Note '
   @ 4, 33 SAY '---------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 7
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From S.R.#  : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   USE st-tran INDEX stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   S.R.#  : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=MDOC_TYPE
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
            DO top-prt
            @ 3, 30 SAY 'Sale Return Note '
            L= L+1
            @ L, 30 SAY '---------------- '
            L= L+1
         ELSE
            DO screen
            @ 3, 30 SAY 'Sale Return Note '
            L= L+1
            @ L, 30 SAY '---------------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'S.R. No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MC1= C_C1
         MC2= C_C2
         MC3= C_C3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Debit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 4
         SEEK MCODE
         IF FOUND()
            @ L, 30 SAY TITLE
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
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
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
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'
         SET COLOR TO /w
         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
         SET COLOR TO 7/1
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
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
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

PROCEDURE SR-COR
SELECT 1
GOTO TOP
MREC2= RECNO()
DO sr-dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
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
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
               SET COLOR TO /g
               @ R, 30 SAY MORD
               SET COLOR TO 
               SELECT 6
               STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO n/g
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
         SET COLOR TO 
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
         @ R, 13 SAY ITEM
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
            SET COLOR TO /g
            @ R, 30 SAY MORD
            SET COLOR TO 
            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE SR-DSP
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE
   SET COLOR TO /w
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
   SET COLOR TO 
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
   @ R, 13 SAY ITEM
   SELECT 1
   SET COLOR TO /w
   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'
   SET COLOR TO 
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN

PROCEDURE SALR-VCH
CLEAR
CLOSE ALL
SELECT 1
USE st-tran INDEX stidx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE ldgrcode INDEX ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=7
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
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
L= 3
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 14 SAY 'S.R No'
      @ L, 26 SAY 'Party'
      @ L, 57 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 14 SAY 'S.R No'
         @ L, 26 SAY 'Party'
         @ L, 57 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MDOCNO= REPLICATE('0',5-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY 'SR-'
      @ L, 16 SAY MDOCNO
      MC_C1= C_C1
      MC_C2= C_C2
      MC_C3= C_C3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MC_C1,2)+STR(MC_C2,2)+STR(MC_C3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 57 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 88 SAY ORDER_NO
      @ L, 91 SAY MQTY PICTURE '9999999.9999'
      @ L, 104 SAY MRATE PICTURE '99999999.9999'
      @ L, 117 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN

PROCEDURE STCK-MEN
DO WHILE .T.
   USE st-key INDEX st-idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 25
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY '   S t o c k   T r a n s f e r '
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 - Stock Transfer ..... Entry      '
   @ L+2, M PROMPT ' 2 - Stock Transfer ..... Correction '
   @ L+4, M PROMPT ' 3 - Stock Transfer ..... Deletion   '
   @ L+6, M PROMPT ' 4 - Stock Transfer ..... Printing   '
   @ L+8, M PROMPT ' 5 - Stock Transfer ..... Display    '
   @ L+10, M PROMPT ' 9 - Return to Previous Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         CLEAR
         DO stck-ent
      CASE MCHOICE=2
         CLEAR
         DO stck-cor
      CASE MCHOICE=3
         CLEAR
         DO stck-del
      CASE MCHOICE=4
         CLEAR
         DO stck-prt
      CASE MCHOICE=5
         CLEAR
         DO stck-dis
      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE STCK-ENT
CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE item INDEX item-idx
SELECT 4
USE loc INDEX loc-idx
SELECT 6
USE item-bal INDEX itm-bal
DO WHILE .T.
   CLEAR
   MDOC_TYPE= 8
   @ 1, 30 SAY 'STOCK TRANSFER ENTRY '
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE2, MORDER_NO, MLOC_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO
   R= 3
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 8 SAY 'Transfer Voucher No' GET MDOC_NO PICTURE '9999999'
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
      SET COLOR TO /g
      @ R, 28 SAY MVCH_NO
      SET COLOR TO 
      SELECT 2
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This T.R. No Already Exists  -- Press any key'
         WAIT ''
         SET COLOR TO 7/1 
         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 40 SAY 'Date   ' GET MDATE
      READ
      DO chk
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 8 SAY 'From Location      ' GET MLOC_CODE PICTURE '9999'
         READ
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 4
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w      
         @ R, 40 SAY LOC
         SET COLOR TO 
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 8 SAY 'To   Location      ' GET MLOC_CODE2 PICTURE '9999'
         READ
         IF MLOC_CODE2=0
            LOOP
         ENDIF
         IF MLOC_CODE2=MLOC_CODE
            LOOP
         ENDIF
         SELECT 4
         SEEK MLOC_CODE2
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         SET COLOR TO /w      
         @ R, 40 SAY LOC
         SET COLOR TO 
         MLOC2= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 46 SAY 'Unit '
      @ R, 55 SAY 'Qty '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 23, 45 CLEAR TO 23, 78
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Item Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(55)
               LOOP
            ELSE
               MUNIT= UNIT
            ENDIF
            SET COLOR TO /w      
            @ R, 13 SAY ITEM
            SET COLOR TO 
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 6
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            STORE STR(MLOC_CODE2,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 6
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         SET COLOR TO /w 
         @ R, 46 SAY MUNIT
         SET COLOR TO 
         DO WHILE .T.
            @ R, 55 GET MQTY PICTURE '9999999.9999'
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
         SELECT 5
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
         REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE, SR_NO WITH MSR_NO
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'F'
         UNLOCK
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
         REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE2, SR_NO WITH MSR_NO
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'T'
         UNLOCK
         USE
         IF ANS='Y'
            IF R<=21
               STORE 0 TO MI1, MI2, MI3, MQTY
               R= R+1
               LOOP
            ELSE
               STORE 0 TO MI1, MI2, MI3, MQTY
               @ 11, 0 CLEAR TO 24, 79
               R= 11
               LOOP
            ENDIF
         ENDIF
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         IF ANS='Y'
            DO stk-cor
         ENDIF
         SELECT 2
         USE st-tran INDEX stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key INDEX st-idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 5
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

PROCEDURE STK-DSP
R= 11
@ R, 0 CLEAR TO 24, 79
SELECT 5
GOTO MREC2
DO WHILE .not. EOF()
   MI1= I1
   MI2= I2
   MI3= I3
   SET COLOR TO /w
   @ R, 2 SAY '  -  -    '
   @ R, 2 SAY MI1 PICTURE '99'
   SET COLOR TO n/g
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
   SET COLOR TO 
   SELECT 2
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Item Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(55)
      LOOP
   ELSE
      MUNIT= UNIT
   ENDIF
   SET COLOR TO /w      
   @ R, 13 SAY ITEM
   @ R, 46 SAY MUNIT
   SET COLOR TO 
   SELECT 5
   SET COLOR TO /w
   @ R, 55 SAY T_QTY PICTURE '9999999.9999'
   SET COLOR TO 
   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
   IF I1=MI1.and.I2=MI2.and.I3=MI3
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
   ENDIF
ENDDO
RETURN

PROCEDURE STK-COR
SELECT 5
GOTO TOP
MREC2= RECNO()
DO stk-dsp
GOTO TOP
MREC2= RECNO()
R= 10
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 11, 0 CLEAR TO 24, 79
      R= 11
      IF .not. EOF()
         MREC2= RECNO()
         DO stk-dsp
         GOTO MREC2
         R= 11
      ENDIF
   ENDIF
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 5
      DELETE
      SKIP
      DELETE
      IF EOF()
         EXIT
      ENDIF
   ENDIF
   IF MQES='N'
      SELECT 5
      IF .not. EOF()
         SKIP  +2
         LOOP
      ENDIF
      IF EOF()
         SELECT 5
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.
            SET COLOR TO /w
            @ R, 2 SAY '  -  -    '
            SET COLOR TO 
            @ R, 2 GET MI1 PICTURE '99'
            READ
            SET COLOR TO n/g
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
            SET COLOR TO 
            @ 23, 50 CLEAR TO 23, 78
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
            SET COLOR TO /w
            @ R, 13 SAY ITEM
            @ R, 46 SAY UNIT
            MUNIT= UNIT
            SET COLOR TO 
            @ R, 55 GET MQTY PICTURE '9999999.9999'
            READ
            EXIT
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
         REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'F'
         APPEND BLANK
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
         REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE2
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'T'
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 5
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         SET COLOR TO /w
         @ R, 2 SAY '  -  -    '
         SET COLOR TO 
         @ R, 2 GET MI1 PICTURE '99'
         READ
         SET COLOR TO n/g
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
         SET COLOR TO 
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
         SET COLOR TO /w      
         @ R, 13 SAY ITEM
         @ R, 46 SAY UNIT
         MUNIT= UNIT
         SET COLOR TO 
         @ R, 55 GET MQTY PICTURE '9999999.9999'
         READ
         EXIT
      ENDDO
      SELECT 5
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
      REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'F'
      SKIP
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
      REPLACE T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE2
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'T'
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
      SELECT 5
      EXIT
   ENDIF
ENDDO
RETURN

PROCEDURE STCK-COR
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=8
GOTO TOP
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
SELECT 4
USE item-bal INDEX itm-bal
DO WHILE .T.
   SELECT 1
   CLEAR
   MDOC_TYPE= 8
   @ 1, 28 SAY 'STOCK TRANSFER CORRECTION'
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE2, MORDER_NO, MLOC_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   R= 3
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Transfer Voucher No' GET MDOC_NO PICTURE '9999999'
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
   SET COLOR TO /g
   @ R, 28 SAY MVCH_NO
   SET COLOR TO 
   SELECT 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 
      @ 23, 10 SAY SPACE(75)
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
   SELECT 1
   USE
   SELECT 5
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
   SELECT 5
   GOTO TOP
   MDATE= DATE
   @ R, 40 SAY 'Date   ' GET MDATE
   READ
   DO chk
   R= R+2
   ABC= 0
   DO WHILE ABC=0
      MLOC_CODE= LOC_CODE
      @ R, 8 SAY 'From Location      ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 3
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w      
      @ R, 40 SAY LOC
      SET COLOR TO 
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+2
   SELECT 5
   SKIP
   ABC= 0
   DO WHILE ABC=0
      MLOC_CODE2= LOC_CODE
      @ R, 8 SAY 'To   Location      ' GET MLOC_CODE2 PICTURE '9999'
      READ
      IF MLOC_CODE2=0
         LOOP
      ENDIF
      IF MLOC_CODE2=MLOC_CODE
         LOOP
      ENDIF
      SELECT 3
      SEEK MLOC_CODE2
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w      
      @ R, 40 SAY LOC
      SET COLOR TO 
      MLOC2= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 46 SAY 'Unit '
   @ R, 55 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO STK-COR
   SELECT 5
   MSR_NO= 0
   GOTO TOP
   DO WHILE .not. EOF()
      MSR_NO= MSR_NO+1
      REPLACE LOC_CODE WITH MLOC_CODE, DATE WITH MDATE, SR_NO WITH MSR_NO
      SKIP
      MSR_NO= MSR_NO+1
      REPLACE LOC_CODE WITH MLOC_CODE2, DATE WITH MDATE, SR_NO WITH MSR_NO
      SKIP
   ENDDO
   USE
   SELECT 1
   USE st-tran INDEX stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
   SET FILTER TO DOC_TYPE=8
   GOTO TOP
   SELECT 5
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
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

PROCEDURE STCK-DEL
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE st-tran INDEX stidx
SET FILTER TO DOC_TYPE=8
GOTO TOP
SELECT 2
USE item INDEX item-idx
SELECT 3
USE loc INDEX loc-idx
SELECT 4
USE item-bal INDEX itm-bal
DO WHILE .T.
   SELECT 1
   CLEAR
   MDOC_TYPE= 8
   @ 1, 30 SAY 'STOCK TRANSFER DELETION'
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE2, MORDER_NO, MLOC_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   R= 3
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Transfer Voucher No' GET MDOC_NO PICTURE '9999999'
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
   SET COLOR TO /g
   @ R, 28 SAY MVCH_NO
   SET COLOR TO 
   SELECT 1
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 
      @ 23, 10 SAY SPACE(75)
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
   SELECT 1
   USE
   SELECT 5
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
   SELECT 5
   GOTO TOP
   @ R, 40 SAY 'Date   ' GET DATE
   R= R+2
   ABC= 0
   DO WHILE ABC=0
      MLOC_CODE= LOC_CODE
      @ R, 8 SAY 'From Location      ' GET MLOC_CODE PICTURE '9999'
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 3
      SEEK MLOC_CODE
      IF FOUND()
         SET COLOR TO /w      
         @ R, 40 SAY LOC
         SET COLOR TO 
         MLOC= LOC
      ENDIF
      ABC= 1
   ENDDO
   R= R+2
   SELECT 5
   SKIP
   ABC= 0
   DO WHILE ABC=0
      MLOC_CODE2= LOC_CODE
      @ R, 8 SAY 'To   Location      ' GET MLOC_CODE2 PICTURE '9999'
      IF MLOC_CODE2=0
         LOOP
      ENDIF
      SELECT 3
      SEEK MLOC_CODE2
      IF FOUND()
         SET COLOR TO /w      
         @ R, 40 SAY LOC
         SET COLOR TO 
         MLOC2= LOC
      ENDIF
      ABC= 1
   ENDDO
   CLEAR GETS
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 46 SAY 'Unit '
   @ R, 55 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 5
   GOTO TOP
   DO WHILE .not. EOF()
      MI1= I1
      MI2= I2
      MI3= I3
      SET COLOR TO /w
      @ R, 2 SAY '  -  -    '
      @ R, 2 SAY MI1 PICTURE '99'
      SET COLOR TO n/g
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
      SET COLOR TO 
      SELECT 2
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Item Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(55)
         LOOP
      ELSE
         MUNIT= UNIT
      ENDIF
      SET COLOR TO /w      
      @ R, 13 SAY ITEM
      @ R, 46 SAY MUNIT
      SET COLOR TO 
      SELECT 5
      SET COLOR TO /w
      @ R, 55 SAY T_QTY PICTURE '9999999.9999'
      SET COLOR TO 
      SKIP
      R= R+1
      IF R>=23
         WAIT
         @ 11, 0 CLEAR TO 24, 79
         R= 11
      ENDIF
      IF I1=MI1.and.I2=MI2.and.I3=MI3
         IF .not. EOF()
            SKIP
            LOOP
         ENDIF
      ENDIF
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 1
   USE st-tran INDEX stidx
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
      SELECT 5
      USE st-key INDEX st-idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 1
   SET FILTER TO DOC_TYPE=8
   GOTO TOP
   SELECT 5
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=8
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE STCK-PRT
CLEAR ALL
CLEAR
SET DATE brit
SELECT 1
USE st-tran INDEX stidx
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE item INDEX item-idx
SELECT 4
USE loc INDEX loc-idx
MT= 0
DO WHILE MT=0
   SELECT 1
   @ 2, 25 SAY ' STOCK TRANSFER REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 8, 20 SAY 'From Date    (dd/mm/yy) :' GET FDATE
   @ 10, 20 SAY 'To   Date    (dd/mm/yy) :' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF TDATE<FDATE
      @ 23, 25 SAY 'To Date Should be greater than From Date'
      WAIT ''
      @ 23, 25 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=8
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   MT= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit - P/Q ' GET P FUNCTION '!'
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
SELECT 1
GOTO TOP
L= 3
HEADING= .T.
PAGE= 0
MBAL= 0
STORE 0 TO MLOC_CODE1, MLOC_CODE2
MDES= SPACE(15)
STORE SPACE(25) TO MLOC1, MLOC2
PG= 1
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      DO top-prt
      @ L, 1 SAY CHR(14)
      @ L, 12 SAY 'STOCK TRANSFER REPORT'
      L= L+1
      @ L, 1 SAY 'From Date : '+DTOC(FDATE)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(TDATE)
      @ L, 68 SAY 'Page : '+STR(PG,2)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 12 SAY 'Voucher #'
      @ L, 28 SAY 'Location'
      @ L, 65 SAY 'Item Code'
      @ L, 80 SAY 'Description'
      @ L, 115 SAY 'Quantity'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MF_T= F_T
   IF MF_T='F'
      MLOC_CODE1= LOC_CODE
      MDES= 'From :'
   ENDIF
   MDOCNO= DOC_NO
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',5-LEN(LTRIM(STR(MDOCNO))))+LTRIM(STR(MDOCNO))
   @ L, 12 SAY 'TRANSFER-'+MDOC_NO
   SELECT 4
   SEEK MLOC_CODE1
   IF FOUND()
      MLOC1= LOC
      @ L, 28 SAY MDES+MLOC1
   ENDIF
   SELECT 1
   CNT= 0
   DO WHILE DOC_NO=MDOCNO
      IF DOC_NO<>MDOCNO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      IF CNT=1
         MF_T= F_T
         IF MF_T='T'
            MLOC_CODE2= LOC_CODE
            MDES= 'To   :'
         ENDIF
         SELECT 4
         GOTO TOP
         SEEK MLOC_CODE2
         IF FOUND()
            MLOC2= LOC
         ENDIF
         @ L, 28 SAY MDES+MLOC2
      ENDIF
      SELECT 1
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 65 SAY MP1
      @ L, 67 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 68 SAY MP2
      @ L, 70 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 71 SAY MP3
      SELECT 3
      SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
      IF FOUND()
         @ L, 80 SAY ITEM
      ENDIF
      SELECT 1
      @ L, 115 SAY T_QTY
      L= L+1
      CNT= CNT+1
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   IF L>=52
      @ 1, 1 SAY CHR(18)
      L= 3
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
ENDDO
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
CLEAR
RETURN

PROCEDURE STCK-DIS
CLEAR ALL
CLEAR
SET DELETED ON
SET DATE brit
SELECT 2
USE st-tran INDEX stidx
SELECT 3
USE item INDEX item-idx
SELECT 4
USE loc INDEX loc-idx
MT= 0
DO WHILE MT=0
   SELECT 2
   MDOC_TYPE= 8
   @ 2, 25 SAY ' STOCK TRANSFER REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE, MDOC_NO, MDOC_NO1
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 20 SAY 'From TR No : '
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
   SET COLOR TO /w
   @ 10, 35 SAY MVCH_NO
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This TR No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   TR No : '
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
   SET COLOR TO /w
   @ 12, 35 SAY MVCH_NO1
   SET COLOR TO 7/1 
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This TR No Does not Exists  -- Press any key'
      WAIT ''
      SET COLOR TO 7/1 
      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=8
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
   MT= 1
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
L= 3
HEADING= .T.
CNT= 0
DO WHILE .not. EOF()
   CLEAR
   MDOC_NO= DOC_NO
   L= 3
   HEADING= .T.
   STORE 0 TO CNT, MTOT, PAGE
   SELECT 1
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 30 SAY 'STOCK TRANSFER REPORT'
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'STOCK TRANSFER REPORT'
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'Transfer No  : '
         @ L, 18 SAY MVCH_NO
         @ L, 56 SAY 'Date     : '+DTOC(DATE)
         L= L+1
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'From Location: '
         @ L, 18 SAY LOC_CODE PICTURE '9999'
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 4
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         SELECT 1
         SKIP
         L= L+1
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'To Location  : '
         @ L, 18 SAY LOC_CODE PICTURE '9999'
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 4
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         SELECT 1
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 50 SAY 'Unit'
         @ L, 60 SAY 'Qty '
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      IF F_T='F'
         SKIP
         LOOP
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
      SELECT 3
      SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
      IF FOUND()
         @ L, 12 SAY ITEM
         @ L, 50 SAY UNIT
      ENDIF
      SELECT 1
      @ L, 60 SAY T_QTY
      L= L+1
      IF .not. EOF()
         SKIP
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
      IF P='P'.and.L>56
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   IF P='S'
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
   IF P='P'
      EJECT
   ENDIF
ENDDO
IF P='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SELECT 1
USE
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN

PROCEDURE PTAG-MEN
DO WHILE .T.
   L= 8
   M= 25
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 24 SAY ' Production Targets & Budgeting '
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 - Formula File            '
   @ L+2, M PROMPT ' 2 - Production Target       '
   @ L+4, M PROMPT ' 9 - Return to Previous Menu '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=3
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=2
         CLEAR
         DO targ-men
      CASE MCHOICE=1
         CLEAR
         DO formumen
   ENDCASE
ENDDO
RETURN

PROCEDURE FORMUMEN
DO WHILE .T.
   L= 8
   M= 23
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY ' Formula File Maintenance '
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 -  Formula .... Entry      '
   @ L+1, M PROMPT ' 2 -  Formula .... Correction '
   @ L+2, M PROMPT ' 3 -  Formula .... Deletion   '
   @ L+3, M PROMPT ' 4 -  Formula .... Printing   '
   @ L+4, M PROMPT ' 5 -  Formula Printing (Item Wise)'
   @ L+6, M PROMPT ' 9 -  Return to Previous Menu '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO formuent
      CASE MCHOICE=2
         DO formucor
      CASE MCHOICE=3
         DO formudel
      CASE MCHOICE=4
         DO formuprt
      CASE MCHOICE=5
         DO form-itm
      CASE MCHOICE=6
         CLEAR ALL
         CLEAR
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE FORMUENT
CLEAR
CLEAR ALL
SET DELETED ON
SELECT 1
USE formula INDEX for-idx
SELECT 2
USE item INDEX item-idx
SELECT 3
USE item-bal INDEX itm-bal
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   C= 5
   C2= 35
   R= 7
   SET COLOR TO +7/1
   @ 3, 25 SAY ' Item Formula Entry '
   SET COLOR TO 7/1
   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 7/1
   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0
      SET COLOR TO 7/1
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3
   SET COLOR TO 7/1
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   SET COLOR TO 7/1
   R= R+2
   @ R, 5 SAY REPLICATE('-',64)
   R= R+1
   M= R
   DO WHILE .T.
      @ R, C SAY 'Raw Item No .......'
      SET COLOR TO /w
      @ R, 27 SAY '  -  -    '
      SET COLOR TO 7/1
      @ R, 27 GET MRI1 PICTURE '99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      SET COLOR TO n/g
      MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
      @ R, 27 SAY MPR1
      @ R, 29 SAY '-'
      @ R, 30 GET MRI2 PICTURE '99'
      READ
      MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
      @ R, 30 SAY MPR2
      @ R, 32 SAY '-'
      @ R, 33 GET MRI3 PICTURE '9999'
      READ
      IF MRI3=0
         SET COLOR TO 7/1
         LOOP
      ENDIF
      MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
      @ R, 33 SAY MPR3
      SET COLOR TO 7/1
      @ 23, 50 CLEAR TO 23, 78
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MRI1=0.and.MRI2=0.and.MRI3=0
         LOOP
      ENDIF
      SELECT 1
      STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
      SEEK MR
      IF FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'The Formula For This Item Already Exsist - Retry '
         WAIT ''
         @ 23, 10 SAY SPACE(60)
         LOOP
      ENDIF
      SELECT 2
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
      SEEK MRICODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ R, 38 SAY ITEM
      SET COLOR TO 7/1
      SELECT 3
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
      SEEK MRICODE
      IF FOUND()
         MRATE= AVG_PRICE
      ENDIF
      SELECT 1
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
         R= R+2
         @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
         READ
         IF MQTY=0.or.MRATE=0
            R= R-2
            LOOP
         ENDIF
         MT= 1
      ENDDO
      COR= ' '
      DO WHILE .not. COR$'NY'
         @ 23, 25 SAY ' Any Correction - Y/N ' GET COR PICTURE '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 77
      IF COR='Y'
         R= R-4
         LOOP
      ELSE
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE FI1 WITH MFI1, FI2 WITH MFI2, FI3 WITH MFI3
         REPLACE RI1 WITH MRI1, RI2 WITH MRI2, RI3 WITH MRI3
         REPLACE QTY WITH MQTY
         REPLACE RATE WITH MRATE
         UNLOCK
      ENDIF
      ANS= ' '
      DO WHILE .not. ANS$'NY'
         @ 23, 20 SAY 'More Data For This Finished Item - Y/N ' GET ANS PICTURE '!'
         READ
      ENDDO
      @ 23, 10 CLEAR TO 23, 77
      IF ANS='Y'
         R= R-4
         STORE 0 TO MRI1, MRI2, MRI3, MQTY, MRATE
         @ M, C CLEAR TO 17, 71
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'NY'
      @ 23, 20 SAY ' More Data Entry - Y/N ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR ALL
      EXIT
   ENDIF
ENDDO
CLOSE ALL
CLEAR
RETURN

PROCEDURE FORMUCOR
CLEAR
CLEAR ALL
SELECT 2
USE item INDEX item-idx
SELECT 1
USE formula INDEX for-idx
DO WHILE .T.
   CLEAR
   C= 5
   C2= 35
   R= 7
   SET COLOR TO u
   @ 3, 25 SAY ' Item Formula Correction '
   SET COLOR TO 
   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 
   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   SET COLOR TO 
   R= R+2
   @ R, C SAY 'Raw Item No .......'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 
   @ R, 27 GET MRI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   @ R, 27 SAY MPR1
   @ R, 29 SAY '-'
   @ R, 30 GET MRI2 PICTURE '99'
   READ
   MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   @ R, 30 SAY MPR2
   @ R, 32 SAY '-'
   @ R, 33 GET MRI3 PICTURE '9999'
   READ
   IF MRI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ R, 33 SAY MPR3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MRI1=0.and.MRI2=0.and.MRI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
   SEEK MR
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Formula For This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(60)
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
   SEEK MRICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   SET COLOR TO 
   R= R+2
   SELECT 1
   MQTY= QTY
   MRATE= RATE
   MT= 0
   DO WHILE MT=0
      @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
      R= R+2
      @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
      READ
      IF MQTY=0.or.MRATE=0
         R= R-2
         LOOP
      ENDIF
      MT= 1
   ENDDO
   SELECT 1
   REPLACE QTY WITH MQTY
   REPLACE RATE WITH MRATE
   ANS= ' '
   DO WHILE .not. ANS$'NY'
      @ 23, 20 SAY ' More Data Correction - Y/N ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR ALL
      EXIT
   ENDIF
ENDDO
CLOSE ALL
CLEAR
RETURN

PROCEDURE FORMUDEL
CLEAR
CLEAR ALL
SET DELETED ON
SELECT 2
USE item INDEX item-idx
SELECT 1
USE formula INDEX for-idx
DO WHILE .T.
   CLEAR
   C= 5
   C2= 35
   R= 7
   SET COLOR TO u
   @ 3, 25 SAY ' Item Formula Deletion '
   SET COLOR TO 
   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 
   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   SET COLOR TO 
   R= R+2
   @ R, C SAY 'Raw Item No .......'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 
   @ R, 27 GET MRI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   @ R, 27 SAY MPR1
   @ R, 29 SAY '-'
   @ R, 30 GET MRI2 PICTURE '99'
   READ
   MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   @ R, 30 SAY MPR2
   @ R, 32 SAY '-'
   @ R, 33 GET MRI3 PICTURE '9999'
   READ
   IF MRI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ R, 33 SAY MPR3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MRI1=0.and.MRI2=0.and.MRI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
   SEEK MR
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Formula For This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(60)
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
   SEEK MRICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   SET COLOR TO 
   R= R+2
   SELECT 1
   MQTY= QTY
   MRATE= RATE
   @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
   R= R+2
   @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
   CLEAR GETS
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE
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

PROCEDURE FORMUPRT
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE formula INDEX for-idx
CLEAR
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
STORE 0 TO PAGE, CNT, MTOT, MGT
SELECT 2
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'FORMULA FILE PRINTING '
      ELSE
         DO SCREEN
         @ 3, 30 SAY 'FORMULA FILE PRINTING '
      ENDIF
      L= L+1
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      @ L, 43 SAY 'Unit '
      @ L, 50 SAY 'Qty '
      @ L, 60 SAY 'Rate'
      @ L, 72 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MFI1= FI1
   MFI2= FI2
   MFI3= FI3
   MF1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   MF2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   MF3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFITEM
   STORE QTY TO MQTY
   SELECT 1
   SEEK MFITEM
   IF FOUND()
      MNAME= ITEM
   ELSE
      MNAME= 'Invalid'
   ENDIF
   SELECT 2
   @ L, 1 SAY MF1 PICTURE '99'
   @ L, 3 SAY '-'
   @ L, 4 SAY MF2 PICTURE '99'
   @ L, 6 SAY '-'
   @ L, 7 SAY MF3 PICTURE '9999'
   @ L, 14 SAY MNAME
   L= L+1
   MLEN= LEN(RTRIM(MNAME))
   @ L, 1 SAY REPLICATE('-',17+MLEN)
   L= L+1
   SELECT 2
   MGT= 0
   DO WHILE FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
      IF FI1<>MFI1.and.FI2<>MFI2.and.FI3<>MFI3
         EXIT
      ENDIF
      IF HEADING
         L= 3
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'FORMULA FILE PRINTING '
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'FORMULA FILE PRINTING '
         ENDIF
         L= L+1
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 2 SAY 'Item Code'
         @ L, 14 SAY 'Item'
         @ L, 43 SAY 'Unit '
         @ L, 50 SAY 'Qty '
         @ L, 60 SAY 'Rate'
         @ L, 72 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 2
      MRI1= RI1
      MRI2= RI2
      MRI3= RI3
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRITEM
      SELECT 1
      SEEK MRITEM
      IF FOUND()
         STORE ITEM TO MRNAME
         MUNIT= UNIT
      ELSE
         MRNAME= 'Invalid'
         MUNIT= '      '
      ENDIF
      SELECT 2
      MR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
      MR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
      MR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
      @ L, 1 SAY MR1 PICTURE '99'
      @ L, 3 SAY '-'
      @ L, 4 SAY MR2 PICTURE '99'
      @ L, 6 SAY '-'
      @ L, 7 SAY MR3 PICTURE '9999'
      @ L, 14 SAY MRNAME
      @ L, 43 SAY MUNIT
      @ L, 47 SAY QTY PICTURE '9999.9999'
      @ L, 60 SAY RATE PICTURE '9999.9999'
      MTOT= 0
      MTOT= QTY*RATE
      @ L, 70 SAY MTOT PICTURE '999999.99'
      MGT= MGT+MTOT
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
      IF L>53
         HEADING= .T.
      ENDIF
      IF .not. EOF()
         SKIP
         L= L+1
      ENDIF
   ENDDO
   @ L, 60 SAY REPLICATE('-',20)
   L= L+1
   @ L, 60 SAY 'Total'
   @ L, 70 SAY MGT PICTURE '999999.99'
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

PROCEDURE FORM-ITM
CLEAR
CLOSE ALL
SELECT 1
USE item INDEX item-idx
SELECT 2
USE formula INDEX for-idx1
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   R= 9
   C= 10
   SET COLOR TO +7/1
   @ 5, 25 SAY ' Formula Printing (Item Wise) '
   SET COLOR TO 7/1
   @ 23, 50 SAY ' Press Esc To Return '
   STORE 0 TO MFI1, MFI2, MFI3
   @ R, C SAY 'Finished Item No ..'
   SET COLOR TO /w
   @ R, 27 SAY '  -  -    '
   SET COLOR TO 7/1
   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO /w
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0
      SET COLOR TO 7/1
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3
   SET COLOR TO 7/1
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 5 SAY 'This Finished Item Code Does Not Exist in Formula File ... Please Re-try'
      WAIT ''
      @ 23, 1 SAY SPACE(70)
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 38 SAY ITEM
   MITEM= ITEM
   SET COLOR TO 7/1
   EXIT
ENDDO
SELECT 2
SET FILTER TO FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
STORE 0 TO PAGE, CNT, MTOT, MGT
SELECT 2
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 25 SAY 'FORMULA PRINTING (ITEM WISE)'
      ELSE
         DO SCREEN
         @ 3, 25 SAY 'FORMULA PRINTING (ITEM WISE)'
      ENDIF
      L= L+1
      @ L, 1 SAY 'Finished Item : '
      @ L, 17 SAY MP1 PICTURE '99'
      @ L, 19 SAY '-'
      @ L, 20 SAY MP2 PICTURE '99'
      @ L, 22 SAY '-'
      @ L, 23 SAY MP3 PICTURE '9999'
      @ L, 28 SAY MITEM
      @ L, 70 SAY 'Page -'
      @ L, 78 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      @ L, 43 SAY 'Unit '
      @ L, 50 SAY 'Qty '
      @ L, 60 SAY 'Rate'
      @ L, 72 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MRI1= RI1
   MRI2= RI2
   MRI3= RI3
   SELECT 1
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRCODE
   SEEK MRCODE
   IF .not. FOUND()
      MNAME= 'Invalid'
   ELSE
      MNAME= ITEM
      MUNIT= UNIT
   ENDIF
   SELECT 2
   MR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   MR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   MR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ L, 1 SAY MR1 PICTURE '99'
   @ L, 3 SAY '-'
   @ L, 4 SAY MR2 PICTURE '99'
   @ L, 6 SAY '-'
   @ L, 7 SAY MR3 PICTURE '9999'
   @ L, 14 SAY MNAME
   @ L, 43 SAY MUNIT
   @ L, 47 SAY QTY PICTURE '9999.9999'
   @ L, 60 SAY RATE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*RATE
   @ L, 70 SAY MTOT PICTURE '999999.99'
   MGT= MGT+MTOT
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
   IF L>53
      HEADING= .T.
   ENDIF
   IF .not. EOF()
      SKIP
      L= L+1
   ENDIF
ENDDO
@ L, 60 SAY REPLICATE('-',20)
L= L+1
@ L, 60 SAY 'Total'
@ L, 70 SAY MGT PICTURE '999999.99'
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

PROCEDURE TARG-MEN
DO WHILE .T.
   L= 8
   M= 23
   CLEAR
   SET COLOR TO 7/1
   DO top
   DO bot
   SET COLOR TO +7/1  
   @ 6, 25 SAY 'Target  File  Maintenance'
   SET COLOR TO 7/1
   @ L+0, M PROMPT ' 1 - Target .... Entry       '
   @ L+2, M PROMPT ' 2 - Target .... Correction  '
   @ L+4, M PROMPT ' 3 - Target .... Deletion    '
   @ L+6, M PROMPT ' 4 - Target .... Printing    '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu '
   DO bot
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO targ-ent
      CASE MCHOICE=2
         DO targ-cor
      CASE MCHOICE=3
         DO targ-del
      CASE MCHOICE=4
         DO targ-rep
      CASE MCHOICE=5
         CLEAR ALL
         CLEAR
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE TARG-ENT
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO targ-idx
DO WHILE .T.
   CLEAR
   STORE 0 TO MI1, MI2, MI3, MQTY
   SET COLOR TO u
   @ 5, 30 SAY ' Item Target Entry '
   SET COLOR TO 
   DO WHILE .T.
      R= 8
      SET COLOR TO 
      @ 23, 50 SAY ' Press Esc To Return '
      @ R, 10 SAY 'Item Code '
      SET COLOR TO /w
      @ R, 22 SAY '  -  -    '
      SET COLOR TO 
      @ R, 22 GET MI1 PICTURE '99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      SET COLOR TO n/g
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 22 SAY MP1
      @ R, 24 SAY '-'
      @ R, 25 GET MI2 PICTURE '99'
      READ
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 25 SAY MP2
      @ R, 27 SAY '-'
      @ R, 28 GET MI3 PICTURE '9999'
      READ
      IF MI3=0
         LOOP
      ENDIF
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 28 SAY MP3
      SET COLOR TO 
      @ 23, 50 CLEAR TO 23, 78
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MI1=0.and.MI2=0.and.MI3=0
         LOOP
      ENDIF
      SELECT 1
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'The Target Of This Item Already Exsist - Retry '
         WAIT ''
         @ 23, 10 SAY SPACE(55)
         LOOP
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ R, 33 SAY ITEM
      SET COLOR TO 
      R= R+2
      @ R, 10 SAY 'Target Quantity ..... ' GET MQTY PICTURE '99999999'
      READ
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 24, 20 CLEAR TO 24, 78
      IF ANS='Y'
         LOOP
      ENDIF
      SELECT 1
      APPEND BLANK
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      REPLACE QUANTITY WITH MQTY
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

PROCEDURE TARG-COR
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO targ-idx
DO WHILE .T.
   CLEAR
   R= 8
   STORE 0 TO MI1, MI2, MI3, MQTY
   SET COLOR TO u
   @ 5, 27 SAY ' Item Target Correction '
   SET COLOR TO 
   @ 23, 50 SAY ' Press Esc To Return '
   @ R, 10 SAY 'Item Code '
   SET COLOR TO /w
   @ R, 22 SAY '  -  -    '
   SET COLOR TO 
   @ R, 22 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 22 SAY MP1
   @ R, 24 SAY '-'
   @ R, 25 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 25 SAY MP2
   @ R, 27 SAY '-'
   @ R, 28 GET MI3 PICTURE '9999'
   READ
   IF MI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 28 SAY MP3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Target Of This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SELECT 2
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 33 SAY ITEM
   SET COLOR TO 
   SELECT 1
   R= R+2
   @ R, 10 SAY 'Target Quantity ..... ' GET QUANTITY PICTURE '99999999'
   READ
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

PROCEDURE TARG-DEL
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO targ-idx
DO WHILE .T.
   CLEAR
   R= 8
   STORE 0 TO MI1, MI2, MI3, MQTY
   SET COLOR TO u
   @ 5, 27 SAY ' Item Target Deletion '
   SET COLOR TO 
   @ 23, 50 SAY ' Press Esc To Return '
   @ R, 10 SAY 'Item Code '
   SET COLOR TO /w
   @ R, 22 SAY '  -  -    '
   SET COLOR TO 
   @ R, 22 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SET COLOR TO n/g
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 22 SAY MP1
   @ R, 24 SAY '-'
   @ R, 25 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 25 SAY MP2
   @ R, 27 SAY '-'
   @ R, 28 GET MI3 PICTURE '9999'
   READ
   IF MI3=0
      SET COLOR TO 
      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 28 SAY MP3
   SET COLOR TO 
   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Target Of This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SELECT 2
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R, 33 SAY ITEM
   SET COLOR TO 
   SELECT 1
   R= R+2
   @ R, 10 SAY 'Target Quantity ..... ' GET QUANTITY PICTURE '99999999'
   CLEAR GETS
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE
      PACK
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

PROCEDURE TARG-REP
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO targ-idx
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to retu to menu .... '
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
L= 3
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      IF P='P'
         DO top-prt1
         @ 3, 15 SAY CHR(18)
         @ 3, 30 SAY ' Bughet Target Report '
         @ 4, 30 SAY ' -------------------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 30 SAY ' Bughet Target Report '
         L= L+1
         @ L, 30 SAY ' -------------------- '
         @ L, 62 SAY 'Page - '
         @ L, 70 SAY PAGE PICTURE '999'
      ENDIF
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 5 SAY 'Item '
      @ L, 19 SAY 'Description'
      @ L, 65 SAY 'Target'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
      L= L+1
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 5 SAY MP1 PICTURE '99'
   @ L, 7 SAY '-'
   @ L, 8 SAY MP2 PICTURE '99'
   @ L, 10 SAY '-'
   @ L, 11 SAY MP3 PICTURE '9999'
   SELECT 2
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      STORE 'Invalid Name ' TO MNAME
   ELSE
      STORE ITEM TO MNAME
   ENDIF
   SELECT 1
   @ L, 19 SAY MNAME
   @ L, 60 SAY QUANTITY
   SKIP
   L= L+1
   IF P='S'
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
         L= 3
         HEADING= .T.
      ENDIF
   ENDIF
   IF P='P'
      IF L>56
         L= 3
         HEADING= .T.
      ENDIF
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
CLEAR
SET CONFIRM ON
CLOSE ALL
RETURN

PROCEDURE MAINTMEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1  
   @ 6, 27 SAY 'System Maintenance Menu'
   SET COLOR TO 7/1  
   @ 9, 21 PROMPT ' 1 - Backup Data Files To Diskette (Copy)  '
   @ 11, 21 PROMPT ' 2 - Backup Data Files To Diskette (Backup)'
   @ 13, 21 PROMPT ' 3 - Re-Index Data Files     '
   @ 15, 21 PROMPT ' 4 - Sort Vouchers Date Wise '
   @ 17, 21 PROMPT ' 9 - Return to Previous Menu '
   CANS= 0
   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO back-up1
      CASE CANS=2
         DO back-up2
      CASE CANS=3
         DO reindex
      CASE CANS=4
         DO dsort
      CASE CANS=5
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN

PROCEDURE BACK-UP1
CLEAR
CLEAR ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 8, 15 SAY 'Please Insert Diskette in Drive A: To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Make Backup - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      RUN copy *.dbf a:
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN

PROCEDURE BACK-UP2
CLEAR
CLEAR ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 8, 15 SAY 'Please Insert Diskette in Drive A: To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Make Backup - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      RUN backup  *.dbf a:
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN

PROCEDURE SECURMEN
DO WHILE .T.
   SET COLOR TO 7/1  
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1  
   @ 6, 27 SAY ' System Security Menu'
   SET COLOR TO 7/1  
   @ 8, 25 PROMPT ' 1 - Change Passwords        '
   @ 10, 25 PROMPT ' 2 - Initialize Passwords    '
   @ 12, 25 PROMPT ' 3 - Unpost Vouchers From Ledger'
   @ 14, 25 PROMPT ' 9 - Return to Previous Menu '
   CANS= 0
   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO password
      CASE CANS=2
         DO init-pw
      CASE CANS=3
         DO un-post
      CASE CANS=4
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN

PROCEDURE PASSWORD
CLEAR
CLOSE ALL
USE invt-pw
GOTO TOP
STORE SPACE(6) TO MPW1, MPW2, MPW3, MPW4, MPW5, MPW6, MPW7, MPW8
R= 5
DO WHILE .T.
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 1'
   @ R, 46 GET MM FUNCTION '!'
   READ
   MCODE= MM
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm1     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 2'
   @ R, 46 GET MM1 FUNCTION '!'
   READ
   MCODE= MM1
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm2     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 3'
   @ R, 46 GET MM2 FUNCTION '!'
   READ
   MCODE= MM2
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm3     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 4'
   @ R, 46 GET MM3 FUNCTION '!'
   READ
   MCODE= MM3
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm4     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 5'
   @ R, 46 GET MM4 FUNCTION '!'
   READ
   MCODE= MM4
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm5     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 6'
   @ R, 46 GET MM5 FUNCTION '!'
   READ
   MCODE= MM5
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   R= R+2
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm6     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 7'
   @ R, 46 GET MM6 FUNCTION '!'
   READ
   MCODE= MM6
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   R= R+2
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm7     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 8'
   @ R, 46 GET MM7 FUNCTION '!'
   READ
   MCODE= MM7
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 20 SAY 'Any Change In These Passwords - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 23, 19 CLEAR TO 23, 66
   IF ANS='Y'
      R= 5
      GOTO TOP
      LOOP
   ENDIF
   EXIT
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE INIT-PW
CLEAR
CLOSE ALL
@ 10, 15 SAY 'Running This Program Will Initialize All Passwords '
P= ' '
DO WHILE .not. P$'YN'
   @ 12, 15 SAY 'Want to proceed ......Y/N' GET P FUNCTION '!'
   READ
ENDDO
IF P='N'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
USE invt-pw
GOTO TOP
STORE SPACE(6) TO MPW1, MPW2, MPW3, MPW4, MPW5, MPW6, MPW7, MPW8
R= 5
DO WHILE .T.
   MCODE= MPW1
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW2
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW3
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW4
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW5
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW6
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW7
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW8
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   EXIT
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE UN-POST
CLEAR
CLEAR ALL
SET EXCLUSIVE ON
CLOSE ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 10, 15 SAY ' This Option will UN-POST all Posted Vouchers'
   @ 11, 15 SAY ' from the Ledger.'
   @ 13, 15 SAY ' Do you want to continue - Y/N ' GET ANS PICTURE '!'
   READ
ENDDO
IF ANS='N'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
USE item-bal
DO WHILE .not. EOF()
   IF I3=0
      SKIP
      LOOP
   ENDIF
   REPLACE QTY WITH OPEN_BAL, AVG_PRICE WITH OPEN_AVG, SR_NO WITH 1
   SKIP
   IF EOF()
      EXIT
   ENDIF
ENDDO
USE stock
DELETE ALL FOR SR_NO<>1
PACK
USE st-tran
REPLACE FOR FLAG='Y' FLAG WITH ' '
USE p-order
REPLACE ALL QTY_DEL WITH 0, QTY_BAL WITH 0
USE s-order
REPLACE ALL QTY_DEL WITH 0, QTY_BAL WITH 0
CLOSE ALL
SET EXCLUSIVE OFF
CLEAR
CLOSE ALL
RETURN

PROCEDURE REINDEX
CLEAR
CLOSE ALL
SET SAFETY OFF
SET DELETED OFF
SET EXCLUSIVE OFF
SET COLOR TO 7/1
CLEAR
@ 2, 2 SAY 'Reindex/Re-organize Data Files'
P= ' '
DO WHILE .not. P$'CQ'
   @ 3, 2 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
@ 2, 20 SAY 'Re-indexing Data Files  --- Please wait'
CLOSE ALL
SET EXCLUSIVE ON
CLOSE ALL
USE item
PACK
USE ldgrcode
PACK
USE item-bal
PACK
USE stock
PACK
USE loc
PACK
USE dept
PACK
USE mach
PACK
USE zone
PACK
USE city
PACK
USE st-tran
PACK
USE st-work
ZAP
USE st-cor
ZAP
USE st-key
PACK
USE p-order
PACK
USE s-order
PACK
USE formula
PACK
COPY TO for-work STRUCTURE
USE for-work
ZAP
CLOSE ALL
SET EXCLUSIVE OFF
CLOSE ALL
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO item-idx
INDEX ON ITEM TO itm1-idx
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO itm-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itmd-idx
USE stock
INDEX ON STR(D_D1,2)+STR(D_D2,2)+STR(D_D3,3) TO std-idx
INDEX ON STR(CITY_CODE,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO cstck
INDEX ON STR(ZONE_CODE,2)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO zstck
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO stck-idx
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TO itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TO itemidx1
INDEX ON DOC_NO TO itemidx2
INDEX ON DATE TO stk-idx
USE loc
INDEX ON LOC_CODE TO loc-idx
USE dept
INDEX ON DEPT_CODE TO dept-idx
USE mach
INDEX ON STR(DEPT_CODE,3)+STR(MACH_CODE,4) TO mach-idx
USE zone
INDEX ON ZONE_CODE TO zone-idx
USE city
INDEX ON CITY_CODE TO city-idx
USE st-tran
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TO stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO stidx1
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TO st-idx
USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO pord-idx
USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TO sord-idx
USE formula
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4)+STR(RI1,2)+STR(RI2,2)+STR(RI3,4) TO for-idx
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4) TO for-idx1
CLEAR
CLOSE ALL
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN

PROCEDURE DSORT
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
CLOSE ALL
SELECT 1
USE st-tran
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TO stidx
SELECT 2
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TO st-idx
ZAP
SELECT 1
DO WHILE .not. EOF()
   MDOC_TYPE= DOC_TYPE
   MDOC_NO= DOC_NO
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      APPEND BLANK
      REPLACE DOC_TYPE WITH MDOC_TYPE
      REPLACE DOC_NO WITH MDOC_NO
   ENDIF
   SELECT 1
   SKIP
ENDDO
SELECT 2
GOTO TOP
REPLACE ALL FLAG WITH 'Y'
CLOSE ALL
CLEAR
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TO st-idx
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN

PROCEDURE DENIED
CLEAR
ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)
   SET COLOR TO *w
   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO
SET COLOR TO 
CLEAR
RETURN

PROCEDURE CHK
SET DATE brit
IF MDATE>CTOD('31/12/99')
   SET ESCAPE OFF
   CLOSE ALL
   CLEAR
   QUIT
ENDIF
RETURN
 
