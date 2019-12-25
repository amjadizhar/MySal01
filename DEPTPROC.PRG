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
USE dept ORDER dept_idx
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
USE dept ORDER dept_idx
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
USE dept ORDER dept_idx
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
USE dept ORDER dept_idx
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
         DO SCREEN IN TBL_PROC.PRG
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
