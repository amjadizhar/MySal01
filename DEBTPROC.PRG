* 24-Nov-2005
* PROCEDURE DEBTPROC.PRG
* WRITTEN BY AMJAD IZHAR
* WRITTEN ON 07TH JUNE,2005
* THIS REVISION: 7TH JUNE, 2005 9:49 AM
***************************************************



*******************
PROCEDURE DEBT_MEN
*******************

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



   @ 6, 20 SAY 'Inventory : Debtors Code File Maintenance'



IF _DOS
   SET COLOR TO 7/1 
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

USE ldgrcode ORDER ac1

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
********************

*****************************
PROCEDURE DEBT_COR
*****************************
CLEAR
CLOSE ALL

USE ldgrcode ORDER ac1
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

   @ 5, 20 SAY 'Inventory : Debtors Codes Correction'

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
****************


***************************
PROCEDURE DEBT_DEL
****************************

CLEAR
CLOSE ALL

SET DELETED ON

USE ldgrcode ORDER ac1
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
********************


************************
PROCEDURE DEBT_PRT
************************

IF _DOS
SET COLOR TO 7/1 
ENDIF

CLEAR
CLOSE ALL

USE ldgrcode ORDER ac1

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
         DO SCREEN IN TBL_PROC.PRG
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
**************


