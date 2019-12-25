* Inventory
* This Revision
* 24-Nov-2005


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
USE city ORDER city_idx
SELECT 2
USE zone ORDER zone_idx
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
USE city ORDER city_idx
SELECT 2
USE zone ORDER zone_idx
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
USE city ORDER city_idx
SELECT 2
USE zone ORDER zone_idx
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
USE city ORDER city_idx
SELECT 2
USE zone ORDER zone_idx
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
         DO SCREEN IN TBL_PROC.PRG
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
