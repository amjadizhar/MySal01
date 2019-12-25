* PROGRAM: INVT1MNU.PRG
* WRITTEN BY : AMJAD IZHAR
* THIS REVISION; 19-12-2019
****************************************

***********************
PROCEDURE CODE_MEN
***********************

DO WHILE .T.

   INT= 1

   			SET COLOR TO 7/1
   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

   			SET COLOR TO +7/1

   		@ 6, 27 SAY 'Code File Maintenance'

		R=8
		C=24

   		@ R, C PROMPT ' 1  -  Item       .. Codes '
   		@ R+1, C PROMPT ' 2  -  Location   .. Codes '
   		@ R+2, C PROMPT ' 3  -  Department .. Codes '
   		@ R+3, C PROMPT ' 4  -  Machine    .. Codes '
   		@ R+4, C PROMPT ' 5  -  Zone       .. Codes '
   		@ R+5, C PROMPT ' 6  -  City       .. Codes '

   IF INT<>1

      @ R+6, C PROMPT ' 7  -  Debtors    .. Codes '
      @ R+7, C PROMPT ' 8  -  Creditors  .. Codes '
      @ R+9, C PROMPT ' 9  -  Return to Main Menu '

   ELSE
      @ R+9, C PROMPT ' 9  -  Return to Main Menu '

   ENDIF

   CHOICE= 0

			@ 22,65 SAY "Inventory"

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
******

***********************
PROCEDURE ITEM_MEN
***********************

DO WHILE .T.

   INT= 1
   L= 8
   M= 24




   SET COLOR TO 7/1


   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG


   SET COLOR TO +7/1


   @ 6, 20 SAY 'Inventory :  Item Code File Maintenance'


   


   @ L+0, M PROMPT ' 1 - Item Code ...... Entry          '
   @ L+1, M PROMPT ' 2 - Item Code ...... Correction     '
   @ L+2, M PROMPT ' 3 - Item Code ...... Deletion       '
   @ L+3, M PROMPT ' 4 - Item Code ...... Printing       '
   @ L+4, M PROMPT ' 5 - Complete Item Codes Printing    '
   @ L+5, M PROMPT ' 6 - Item Codes Printing with Dr/Cr  '
   @ L+6, M PROMPT ' 7 - Item Codes Printing (Code Wise) '
   @ L+8, M PROMPT ' 8 - Item View ...... Browse         '
   @ L+10, M PROMPT ' 9 - Return to Previous Menu         '

   MCHOICE= 0


		
			@ 22,65 SAY "Inventory"
		



   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=9
         CLEAR
         CLOSE DATABASE

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN


      CASE MCHOICE=8
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         *DO ITEMVIEW.SPR



      CASE MCHOICE=7
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO itemprt3 IN itemproc.PRG

      CASE MCHOICE=6
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO itemprt2 IN itemproc.PRG

      CASE MCHOICE=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO itemprt1 IN itemproc.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO item_prt IN itemproc.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO item_del IN itemproc.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO item_cor IN itemproc.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO item_ent IN itemproc.PRG

   ENDCASE

ENDDO


RETURN
************************

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

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY 'Department Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' A - Department Code .... Entry      '
   @ L+2, M PROMPT ' B - Department Code .... Correction '
   @ L+4, M PROMPT ' C - Department Code .... Deletion   '
   @ L+6, M PROMPT ' D - Department Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu         '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dept_prt IN DEPTPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dept_del IN DEPTPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dept_cor IN DEPTPROC.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dept_ent IN DEPTPROC.PRG

   ENDCASE


ENDDO


RETURN
*********************************


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


   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG


IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY 'Location Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' A - Location Code .... Entry      '
   @ L+2, M PROMPT ' B - Location Code .... Correction '
   @ L+4, M PROMPT ' C - Location Code .... Deletion   '
   @ L+6, M PROMPT ' D - Location Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu       '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF

		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF


   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO loc_prt IN LOCPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO loc_del IN LOCPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO loc_cor IN LOCPROC.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO loc_ent IN LOCPROC.PRG

   ENDCASE


ENDDO

RETURN
******


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

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY ' Machine Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' A - Machine Code .... Entry      '
   @ L+2, M PROMPT ' B - Machine Code .... Correction '
   @ L+4, M PROMPT ' C - Machine Code .... Deletion   '
   @ L+6, M PROMPT ' D - Machine Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu      '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO mach_prt IN MACHPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO mach_del IN MACHPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO mach_cor IN MACHPROC.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO mach_ent IN MACHPROC.PRG

   ENDCASE


ENDDO

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

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY '   Zone Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' A - Zone Code .... Entry      '
   @ L+2, M PROMPT ' B - Zone Code .... Correction '
   @ L+4, M PROMPT ' C - Zone Code .... Deletion   '
   @ L+6, M PROMPT ' D - Zone Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu   '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO zone_ent IN ZONEPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO zone_cor IN ZONEPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO zone_del IN ZONEPROC.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO zone_prt IN ZONEPROC.PRG

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO


RETURN
**********

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

   @ L+0, M PROMPT ' A - City Code .... Entry      '
   @ L+2, M PROMPT ' B - City Code .... Correction '
   @ L+4, M PROMPT ' C - City Code .... Deletion   '
   @ L+6, M PROMPT ' D - City Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu   '
   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO city_ent IN CITYPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO city_cor IN CITYPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO city_del IN CITYPROC.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO city_prt IN CITYPROC.PRG

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE


ENDDO


RETURN
******


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

   @ 6, 26 SAY '  Debtors Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ L+0, M PROMPT ' A - Debtors Code .... Entry      '
   @ L+2, M PROMPT ' B - Debtors Code .... Correction '
   @ L+4, M PROMPT ' C - Debtors Code .... Deletion   '
   @ L+6, M PROMPT ' D - Debtors Code .... Printing   '
   @ L+8, M PROMPT ' Q - Return to Previous Menu      '

   MCHOICE= 0

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF



		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO debt_prt IN debtproc.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO debt_del IN debtproc.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO debt_cor IN debtproc.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO debt_ent IN debtproc.PRG

   ENDCASE

ENDDO

RETURN
*********



*******************
PROCEDURE CRED_MEN
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

   @ 6, 26 SAY '  Creditors Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ L+0, M PROMPT ' A - Creditors Code .... Entry'
   @ L+2, M PROMPT ' B - Creditors Code .... Correction'
   @ L+4, M PROMPT ' C - Creditors Code .... Deletion'
   @ L+6, M PROMPT ' D - Creditors Code .... Printing'
   @ L+8, M PROMPT ' Q - Return to Previous Menu'

   MCHOICE= 0



		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=5
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cred_prt IN CREDPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cred_del IN CREDPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cred_cor IN CREDPROC.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cred_ent IN CREDPROC.PRG

   ENDCASE

ENDDO

RETURN
**********

******************
PROCEDURE ORD_MEN
******************

DO WHILE .T.

IF _DOS
   L= 8
   M= 19
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG
   
IF _DOS
   SET COLOR TO +7/1 
ENDIF

IF _DOS
   @ 6, 25 SAY '  Orders  File  Maintenance'
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ L+0, M PROMPT ' A - Orders To Supplier/Creditor . Entry      '
   @ L+1, M PROMPT ' B - Orders To Supplier/Creditor . Correction '
   @ L+2, M PROMPT ' C - Orders To Supplier/Creditor . Deletion   '
   @ L+3, M PROMPT ' D - Orders To Supplier/Creditor . Printing   '
   @ L+4, 16 TO L+4, 64
   @ L+5, M PROMPT ' E - Orders From Dealers/Debtors . Entry      '
   @ L+6, M PROMPT ' F - Orders From Dealers/Debtors . Correction '
   @ L+7, M PROMPT ' G - Orders From Dealers/Debtors . Deletion   '
   @ L+8, M PROMPT ' H - Orders From Dealers/Debtors . Printing   '
   @ L+10, M PROMPT ' Q - Return to Previous Menu                  '
ENDIF

R = 12
C = 60

IF _WINDOWS
   @ R-2, 65 SAY 'Orders  File  Maintenance' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Orders To Supplier/Creditor . Entry      ' + SPACE(21)
   @ R+1.5, C PROMPT ' B - Orders To Supplier/Creditor . Correction ' + SPACE(17)
   @ R+3, C PROMPT ' C - Orders To Supplier/Creditor . Deletion   ' + SPACE(19)
   @ R+4.5, C PROMPT ' D - Orders To Supplier/Creditor . Printing   ' + SPACE(20)
   @ R+6.5, C TO R+6.5, C+49
   @ R+7.5, C PROMPT ' E - Orders From Dealers/Debtors . Entry      ' + SPACE(20)
   @ R+9, C PROMPT ' F - Orders From Dealers/Debtors . Correction ' + SPACE(15)
   @ R+10.5, C PROMPT ' G - Orders From Dealers/Debtors . Deletion   ' + SPACE(16)
   @ R+12, C PROMPT ' H - Orders From Dealers/Debtors . Printing   ' + SPACE(17)
   @ R+13.5, C PROMPT ' Q - Return to Previous Menu                  ' + SPACE(26)
ENDIF


   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "Inventory" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=9
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

      CASE MCHOICE=8
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dord_prt IN ORDRPROC.PRG

      CASE MCHOICE=7
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dord_del IN ORDRPROC.PRG

      CASE MCHOICE=6
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dord_cor IN ORDRPROC.PRG

      CASE MCHOICE=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dord_ent IN ORDRPROC.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cord_prt IN ORDRPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cord_del IN ORDRPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cord_cor IN ORDRPROC.PRG

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO cord_ent IN ORDRPROC.PRG

   ENDCASE

ENDDO
RETURN
*******

* Opening Balances
******************
PROCEDURE OPEN_MEN
******************

DO WHILE .T.

   L= 8
   M= 24

   SET COLOR TO 7/1

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

   SET COLOR TO +7/1 

   @ 6, 25 SAY '      Opening Balance Menu   '

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
		
			@ 22,65 SAY "Inventory"
	
	SET COLOR TO 7/1

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR

         DO open_ent IN OPENPROC.PRG

      CASE MCHOICE=2
         CLEAR

         DO open_cor IN OPENPROC.PRG

      CASE MCHOICE=3
         CLEAR

         DO open_del IN OPENPROC.PRG

      CASE MCHOICE=4
         CLEAR

         DO open_prt IN OPENPROC.PRG

      CASE MCHOICE=5
         CLEAR

         DO openprt IN OPENPROC.PRG

      CASE MCHOICE=6
         CLEAR

         DO openprt1 IN OPENPROC.PRG

      CASE MCHOICE=7
         CLEAR

         DO openprt2 IN OPENPROC.PRG

      CASE MCHOICE=8
         CLEAR

         DO openprt3 IN OPENPROC.PRG

      CASE MCHOICE=9
         CLEAR
         CLOSE ALL

         RETURN

   ENDCASE

ENDDO

RETURN
***********************

**************************************************************
* INVENTORY : DAILY TRANSACTIONS MENU
**************************************************************
PROCEDURE TRAN_MEN
********************

DO WHILE .T.

   INT= 1
IF _DOS
   L= 8
   M= 23
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
 
   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF

IF _DOS
   @ 6, 25 SAY ' Daily Transaction Maintenance'
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ L+0, M PROMPT ' A - Purchase / Purchase Returns    '
   @ L+1, M PROMPT ' B - Issue To Production / Returns  '
   @ L+2, M PROMPT ' C - Production Receipts            '
   @ L+3, M PROMPT ' D - Sales / Sales Returns          '
   @ L+4, M PROMPT ' E - Stock Transfer                 '
   @ L+5, M PROMPT ' F - Production Targets & Budgeting '
   @ L+7, M PROMPT ' Q - Return to Previous Menu        '
ENDIF

R = 12
C = 62

IF _WINDOWS
   @ R-2, 68 SAY 'Daily Transactions' FONT 'FoxFont',11
ENDIF

IF _WINDOWS
   @ R, C PROMPT ' A - Purchase / Purchase Returns    ' + SPACE(10)
   @ R+1.5, C PROMPT ' B - Issue To Production / Returns  ' + SPACE(10)
   @ R+3, C PROMPT ' C - Production Receipts            ' + SPACE(16)
   @ R+4.5, C PROMPT ' D - Sales / Sales Returns          ' + SPACE(18)
   @ R+6, C PROMPT ' E - Stock Transfer                 ' + SPACE(22)
   @ R+7.5, C PROMPT ' F - Production Targets & Budgeting ' + SPACE(6)
   @ R+9, C PROMPT ' Q - Return to Previous Menu        ' + SPACE(12)
ENDIF


   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "Inventory" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purr_men

*******************************************************************
* CASE MCHOICE = 2
*    @ L+1, M PROMPT ' 2 - Issue To Production / Returns  '
*******************************************************************

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issu_men 

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO prod_men

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO sale_men

      CASE MCHOICE=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO stck_men

      CASE MCHOICE=6
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO ptag_men

      CASE MCHOICE=7
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO


RETURN
***********


********************
PROCEDURE PURR_MEN
********************

DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF


   SET DELETED ON

   IF INT=1

      USE tran-key ALIAS tran_key EXCLUSIVE
      INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG key
      INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG vchno
	  SET ORDER TO key
      
      *INDEX key-idx

      DELETE ALL FOR FLAG=' '

      CLEAR
      CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF



   ENDIF


   USE st-key ORDER st_idx
   DELETE ALL FOR FLAG=' '

   CLEAR
   CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF


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

   @ L-1, M PROMPT ' A - Purchase ............ Entry      '
   @ L+0, M PROMPT ' B - Purchase ............ Correction '
   @ L+1, M PROMPT ' C - Purchase ............ Deletion   '
   @ L+2, M PROMPT ' D - GRN      ............ Printing   '
   @ L+3, M PROMPT ' E - Verification Voucher  Printing   '
   @ L+4, M TO L+4, 61
   @ L+5, M PROMPT ' F - Purchase Return ..... Entry      '
   @ L+6, M PROMPT ' G - Purchase Return ..... Correction '
   @ L+7, M PROMPT ' H - Purchase Return ..... Deletion   '
   @ L+8, M PROMPT ' I - PR.NO    ............ Printing   '
   @ L+9, M PROMPT 'J - Verification Voucher  Printing   '
   @ L+11, M PROMPT 'Q - Return to Previous Menu          '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO pur_ent IN PURRPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO pur_cor IN PURRPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO pur_del IN PURRPROC.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO pur_prt IN PURRPROC.PRG

      CASE MCHOICE=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO pur_vch IN PURRPROC.PRG

      CASE MCHOICE=6
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purr_ent IN PURRPROC.PRG

      CASE MCHOICE=7
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purr_cor IN PURRPROC.PRG

      CASE MCHOICE=8
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purr_del IN PURRPROC.PRG

      CASE MCHOICE=9
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purrprt IN PURRPROC.PRG

      CASE MCHOICE=10
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO purrvch IN PURRPROC.PRG

      CASE MCHOICE=11
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO


RETURN
*****************


**********************
PROCEDURE POST
**********************

CLEAR
CLOSE ALL

SET DATE brit
SET CENT ON

IF _DOS
 SET COLOR TO 7/1
ENDIF

         IF _WINDOWS
         SET COLOR TO
         ENDIF


SELECT 1

CLEAR

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

USE st-tran
SORT ON DOC_TYPE, DOC_NO, SR_NO TO abc
USE

SELECT 1
USE abc
COPY TO st-tran

*RUN del abc.dbf

SELECT 1
USE st-tran
INDEX ON DATE TAG stdate

SELECT 2
USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG ordidx

SELECT 3
USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal

SELECT 4
USE stock

SELECT 5
USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sordidx

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


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

P= ' '

DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO

IF P='Q'
   CLOSE ALL
   CLEAR

		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

         IF _WINDOWS
         SET COLOR TO
         ENDIF

   RETURN
ENDIF

CLEAR

@ 12, 23 SAY 'Press any key to start posting'
WAIT ''

CLEAR

		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

@ 13, 23 SAY 'Please wait data is being posted....'

SELECT 1
CNT= 0

GOTO TOP
SET FILTER TO FLAG=' '.and.DATE<=MPDATE

GOTO TOP

MSR_NO= 0


DO WHILE .not. EOF()


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

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
         MAVG= AVG_PRICE
         MTOT_QTY= QTY-MQTY
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

         IF _WINDOWS
         SET COLOR TO
         ENDIF



		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TAG itemidx1
INDEX ON DOC_NO TAG itemidx2
INDEX ON DATE TAG stk_idx
CLOSE ALL


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

RETURN
**********

**********************
PROCEDURE REP_MEN
**********************


DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC
   DO bot IN TBL_PROC

IF _DOS
   SET COLOR TO 7/1 
ENDIF

IF _DOS
   @ 6, 25 SAY '  R E P O R T S   M E N U '
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF


IF _DOS
   @ 8, 25 PROMPT ' A - Purchase Report       '
   @ 9, 25 PROMPT ' B - Pending Orders Report '
   @ 10, 25 PROMPT ' C - Sale Report           '
   @ 11, 25 PROMPT ' D - Current Status Report '
   @ 12, 25 PROMPT ' E - Consumption Reports   '
   @ 13, 25 PROMPT ' F - Production Reports    '
   @ 14, 25 PROMPT ' G - Sale Return Report (Party Wise) '
   @ 15, 25 PROMPT ' H - Purchase Return Report (Party Wise) '
   @ 16, 25 PROMPT ' I - Stock Transfer Report (Item Wise)   '
   @ 18, 25 PROMPT 'Q - Return to Main Menu   '
ENDIF

R = 12
C = 61

IF _WINDOWS
   @ R-2, 62 SAY '  R E P O R T S   M E N U ' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Purchase Report       ' + SPACE(31)
   @ R+1.5, C PROMPT ' B - Pending Orders Report ' + SPACE(26)
   @ R+3, C PROMPT ' C - Sale Report           ' + SPACE(36)
   @ R+4.5, C PROMPT ' D - Current Status Report ' + SPACE(28)
   @ R+6, C PROMPT ' E - Consumption Reports   ' + SPACE(26)
   @ R+7.5, C PROMPT ' F - Production Reports    ' + SPACE(30)
   @ R+9, C PROMPT ' G - Sale Return Report (Party Wise)' + SPACE(13)
   @ R+10.5, C PROMPT ' H - Purchase Return Report (Party Wise) ' + SPACE(3)
   @ R+12, C PROMPT ' I - Stock Transfer Report (Item Wise)' + SPACE(11)
   @ R+13.5, C PROMPT 'Q - Return to Main Menu   ' + SPACE(28)
ENDIF

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "Inventory" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men1

      CASE MCHOICE=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men2

      CASE MCHOICE=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men3

      CASE MCHOICE=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men4

      CASE MCHOICE=5

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men5

      CASE MCHOICE=6

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_men6

      CASE MCHOICE=7

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_salr

      CASE MCHOICE=8

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_purr

      CASE MCHOICE=9

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_tran

      CASE MCHOICE=10
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         CLOSE ALL

         RETURN

   ENDCASE


ENDDO

RETURN
**************

********************
PROCEDURE MAINTMEN
********************

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

         IF _WINDOWS
         SET COLOR TO
         ENDIF


   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1  
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

IF _DOS
   @ 6, 27 SAY 'System Maintenance Menu'
ENDIF



IF _DOS
   SET COLOR TO 7/1  
ENDIF 


         IF _WINDOWS
         SET COLOR TO
         ENDIF

IF _DOS
   @ 9, 21 PROMPT ' A - Backup Data Files To Diskette (Copy)  '
   @ 11, 21 PROMPT ' B - Backup Data Files To Diskette (Backup)'
   @ 13, 21 PROMPT ' C - Re-Index Data Files     '
   @ 15, 21 PROMPT ' D - Sort Vouchers Date Wise '
   @ 17, 21 PROMPT ' Q - Return to Previous Menu '
ENDIF

R = 16
C = 59

IF _WINDOWS
   @ R-2, 65 SAY 'System Maintenance Menu' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Backup Data Files To BKP Folder (Copy)  ' + SPACE(5)
   @ R+1.5, C PROMPT ' B - Backup Data Files To BKP Folder (Zip)' + SPACE(11)
   @ R+3, C PROMPT ' C - Re-Index Data Files     ' + SPACE(38)
   @ R+4.5, C PROMPT ' D - Sort Vouchers Date Wise ' + SPACE(32)
   @ R+6, C PROMPT ' Q - Return to Previous Menu ' + SPACE(31)
ENDIF

   CANS= 0

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF



		IF _WINDOWS
			@ 37,141 SAY "Inventory" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CANS


   DO CASE

      CASE CANS=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO back_up1

      CASE CANS=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO back_up2

      CASE CANS=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO reindex IN INVMAINT.PRG

      CASE CANS=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO dsort IN INVT2LB1.PRG

      CASE CANS=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         EXIT

   ENDCASE


ENDDO

CLEAR


RETURN
*************

*******************
PROCEDURE SECURMEN
*******************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1  
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1  
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

IF _DOS
   @ 6, 27 SAY ' System Security Menu'
ENDIF



IF _DOS
   SET COLOR TO 7/1  
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

IF _DOS
   @ 8, 25 PROMPT ' A - Change Passwords        '
   @ 10, 25 PROMPT ' B - Initialize Passwords    '
   @ 12, 25 PROMPT ' C - Unpost Vouchers From Ledger'
   @ 14, 25 PROMPT ' Q - Return to Previous Menu '
ENDIF

R = 16
C = 65

IF _WINDOWS
   @ R-2, 66 SAY ' System Security Menu' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Change Passwords        ' + SPACE(14)
   @ R+1.5, C PROMPT ' B - Initialize Passwords    ' + SPACE(18)
   @ R+3, C PROMPT ' C - Unpost Vouchers From Ledger' + SPACE(2)
   @ R+4.5, C PROMPT ' Q - Return to Previous Menu ' + SPACE(10)
ENDIF

   CANS= 0

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF



		IF _WINDOWS
			@ 37,141 SAY "Inventory" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF


   MENU TO CANS
 
 
   DO CASE

      CASE CANS=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO password IN INVTLIB1.PRG

      CASE CANS=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO init_pw IN INVTLIB1.PRG

      CASE CANS=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO un_post IN INVTLIB1.PRG

      CASE CANS=4
         CLEAR
         EXIT

   ENDCASE


ENDDO

CLEAR
RETURN
***********

*******************************************************************
* INVENTORY : Issue to Production / Returns Menu
*******************************************************************
PROCEDURE ISSU_MEN
********************
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
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 6, 20 SAY 'Inventory : Issue To Production / Returns '

IF _DOS
   SET COLOR TO 7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF


   @ L+0, M PROMPT ' A - Issue To Production ........ Entry      '
   @ L+1, M PROMPT ' B - Issue To Production ........ Correction '
   @ L+2, M PROMPT ' C - Issue To Production ........ Deletion   '
   @ L+3, M PROMPT ' D - Issue To Production Note ... Printing   '
   @ L+4, M PROMPT ' E - Issue To Production with Formula        '
   @ L+5, M TO L+5, 61
   @ L+6, M PROMPT ' F - Issue To Production Return.. Entry      '
   @ L+7, M PROMPT ' G - Issue To Production Return.. Correction '
   @ L+8, M PROMPT ' H - Issue To Production Return.. Deletion   '
   @ L+9, M PROMPT ' I - Issue To Production Return.. Printing   '
   @ L+11, M PROMPT 'Q - Return to Previous Menu                 '

   MCHOICE= 0

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

**********************************************************************
* CASE MCHOICE = 1
*    @ L+0, M PROMPT ' 1 - Issue To Production ........ Entry      '
**********************************************************************
      CASE MCHOICE=1
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issu_ent IN ISSUPROC.PRG

      CASE MCHOICE=2
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issu_cor IN ISSUPROC.PRG

      CASE MCHOICE=3
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issu_del IN ISSUPROC.PRG

      CASE MCHOICE=4
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issu_prt IN ISSUPROC.PRG

      CASE MCHOICE=5
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO issuent IN ISSUPROC.PRG

      CASE MCHOICE=6
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO ispr_ent IN ISSUPROC.PRG

      CASE MCHOICE=7
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO ispr_cor IN ISSUPROC.PRG

      CASE MCHOICE=8
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO ispr_del IN ISSUPROC.PRG

      CASE MCHOICE=9
         CLEAR

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO ispr_prt IN ISSUPROC.PRG

      CASE MCHOICE=10
         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO


RETURN
**********

**********************
PROCEDURE REP_MEN1
************************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 6, 25 SAY 'P U R C H A S E    R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 8, 22 PROMPT ' A - Purchase Report (Date Wise)          '
   @ 9, 22 PROMPT ' B - Purchase Report (Party Wise)         '
   @ 10, 22 PROMPT ' C - Purchase Report (Item Wise)          '
   @ 11, 22 PROMPT ' D - Purchase Report Display              '
   @ 12, 22 PROMPT ' E - Purchase Report (Party Wise Summary) '
   @ 13, 22 PROMPT ' F - Purchase Report (Item Wise Summary)  '
   @ 15, 22 PROMPT ' Q - Return to Main Menu                  '


   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF



		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur1

      CASE MCHOICE=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur2

      CASE MCHOICE=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur3


      CASE MCHOICE=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur4

      CASE MCHOICE=5

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur5

      CASE MCHOICE=6

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pur6


      CASE MCHOICE=7

         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO

RETURN
******

**********************
PROCEDURE REP_MEN2
**********************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 6, 25 SAY '   O R D E R S     R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 8, 25 SAY '        P U R C H A S E S '
   @ 9, 25 PROMPT ' A - Pending Orders (Date Wise) '
   @ 10, 25 PROMPT ' B - Pending Orders (Party Wise) '
   @ 11, 25 PROMPT ' C - Pending Orders (Item Wise)  '
   @ 12, 25 SAY '            S A L E S '
   @ 13, 25 PROMPT ' D - Pending Orders (Date Wise)  '
   @ 14, 25 PROMPT ' E - Pending Orders (Party Wise) '
   @ 15, 25 PROMPT ' F - Pending Orders (Item Wise)  '
   @ 17, 25 PROMPT ' Q - Return to Main Menu         '

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord1

      CASE MCHOICE=2


         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord2

      CASE MCHOICE=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord3

      CASE MCHOICE=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord4

      CASE MCHOICE=5

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord5

      CASE MCHOICE=6

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_ord6

      CASE MCHOICE=7

         CLEAR

         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN
 
   ENDCASE
ENDDO
RETURN
******


************************
PROCEDURE REP_MEN5
************************

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 6, 23 SAY 'C O N S U M P T I O N   R E P O R T S '

IF _DOS
   SET COLOR TO 7/1
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 8, 20 PROMPT ' A - Consumption Report (Item Wise) '
   @ 9, 20 PROMPT ' B - Consumption Report (Department Wise) '
   @ 10, 20 PROMPT ' C - Consumption Report (Date Wise) '
   @ 11, 20 PROMPT ' D - Consumption Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' E - Consumption Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' Q - Return to Main Menu'

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_con IN REPTPROC.PRG

      CASE MCHOICE=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_con1 IN REPTPROC.PRG

      CASE MCHOICE=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_con2 IN REPTPROC.PRG

      CASE MCHOICE=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_con3 IN REPTPROC.PRG

      CASE MCHOICE=5

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_con4 IN REPTPROC.PRG

      CASE MCHOICE=6

         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         RETURN

   ENDCASE

ENDDO


RETURN


***********************
PROCEDURE REP_MEN6
***********************

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1   
ENDIF


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 6, 23 SAY '      PRODUCTION REPORTS  '


   @ 8, 20 PROMPT ' A - Production Report (Item Wise) '
   @ 9, 20 PROMPT ' B - Production Report (Department Wise) '
   @ 10, 20 PROMPT ' C - Production Report (Date Wise) '
   @ 11, 20 PROMPT ' D - Production Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' E - Production Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' Q - Return to Main Menu'

   MCHOICE= 0


		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF


		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pro

      CASE MCHOICE=2

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pro1

      CASE MCHOICE=3

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pro2

      CASE MCHOICE=4

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pro3

      CASE MCHOICE=5

         IF _WINDOWS
         SET COLOR TO
         ENDIF

         DO rep_pro4

      CASE MCHOICE=6

         CLEAR
         CLOSE ALL

         IF _WINDOWS
         SET COLOR TO
         ENDIF

		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
		ENDIF

         RETURN

   ENDCASE

ENDDO

RETURN
*****************


**************************
PROCEDURE REP_MEN4
**************************

DO WHILE .T.
   CLEAR

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 26 SAY ' Current Status Reports  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 22 PROMPT ' A - Present Stock Status Report          '
   @ 9, 22 PROMPT ' B - Present Stock Status (Location Wise) '
   @ 10, 22 PROMPT ' C - Present Stock Status (Item Wise)     '
   @ 11, 22 PROMPT ' D - Present Stock Status (Location & Item Wise) '
   @ 12, 22 PROMPT ' E - Stock Ledger Display                 '
   @ 13, 22 PROMPT ' F - Stock Ledger Printing                '
   @ 14, 22 PROMPT ' G - Stock Below Minimum Level            '
   @ 15, 22 PROMPT ' H - Stock Below Reording Level           '
   @ 16, 22 PROMPT ' I - Stock Above Maximum Level            '
   @ 18, 22 PROMPT 'Q - Return to Main Menu                  '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE
      CASE MCHOICE=1
         DO rep_stat IN REPTPROC.PRG
      CASE MCHOICE=2
         DO stat_loc IN REPTPROC.PRG
      CASE MCHOICE=3
         DO stat_itm IN REPTPROC.PRG
      CASE MCHOICE=4
         DO statitem IN REPTPROC.PRG
      CASE MCHOICE=5
         DO rep_ledg IN REPTPROC.PRG
      CASE MCHOICE=6
         DO repledg IN REPTPROC.PRG
      CASE MCHOICE=7
         DO rep_min IN REPTPROC.PRG
      CASE MCHOICE=8
         DO re_ord IN REPTPROC.PRG
      CASE MCHOICE=9
         DO rep_max IN REPTPROC.PRG
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
**********


**********************
PROCEDURE PROD_MEN
**********************

DO WHILE .T.

   SET DELETED ON

   USE st-key ORDER st_idx

   DELETE ALL FOR FLAG=' '

   CLEAR
   CLOSE ALL

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

IF _WINDOWS
SET COLOR TO
ENDIF


   @ 6, 28 SAY '  Production  Receipts  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF

   @ L+0, M PROMPT ' A - Production Receipts .. Entry      '
   @ L+1, M PROMPT ' B - Production Receipts .. Correction '
   @ L+2, M PROMPT ' C - Production Receipts .. Deletion   '
   @ L+3, M PROMPT ' D - Production Receipts .. Printing   '
   @ L+4, M PROMPT ' E - Production Receipts &             '
   @ L+5, M SAY '      Issue to Production with Formula  '
   @ L+7, M PROMPT ' Q - Return to Previous Menu           '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=6
         CLEAR
         CLOSE DATABASE
         RETURN

      CASE MCHOICE=5
         CLEAR
         DO prodent IN PRODPROC.PRG

      CASE MCHOICE=4
         CLEAR
         DO prod_prt IN PRODPROC.PRG

      CASE MCHOICE=3
         CLEAR
         DO prod_del IN PRODPROC.PRG

      CASE MCHOICE=2
         CLEAR
         DO prod_cor IN PRODPROC.PRG

      CASE MCHOICE=1
         CLEAR
         DO prod_ent IN PRODPROC.PRG

   ENDCASE

ENDDO

RETURN
**********************


************************
PROCEDURE STCK_MEN
************************

DO WHILE .T.
   USE st-key ORDER st_idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   L= 8
   M= 25


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


   @ 6, 20 SAY 'I n v e n t o r y   S t o c k   T r a n s f e r '

IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF	

   @ L+0, M PROMPT ' A - Stock Transfer ..... Entry      '
   @ L+2, M PROMPT ' B - Stock Transfer ..... Correction '
   @ L+4, M PROMPT ' C - Stock Transfer ..... Deletion   '
   @ L+6, M PROMPT ' D - Stock Transfer ..... Printing   '
   @ L+8, M PROMPT ' E - Stock Transfer ..... Display    '
   @ L+10, M PROMPT ' Q - Return to Previous Menu         '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE
      CASE MCHOICE=1
         CLEAR
         DO stck_ent IN STCKPROC.PRG

      CASE MCHOICE=2
         CLEAR
         DO stck_cor IN STCKPROC.PRG

      CASE MCHOICE=3
         CLEAR
         DO stck_del IN STCKPROC.PRG

      CASE MCHOICE=4
         CLEAR
         DO stck_prt IN STCKPROC.PRG

      CASE MCHOICE=5
         CLEAR
         DO stck_dis IN STCKPROC.PRG

      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN

   ENDCASE


ENDDO

RETURN
****************



***********************
PROCEDURE SALE_MEN
***********************

DO WHILE .T.

   SET DELETED ON

   IF INT=1
      USE tran-key ALIAS tran_key EXCLUSIVE
      INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG key
      INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG vchno
      SET ORDER TO vchno
      *ORDER vchno
      
      DELETE ALL FOR FLAG=' '
      CLEAR
      CLOSE ALL
   ENDIF

   USE st-key ORDER st_idx

   DELETE ALL FOR FLAG=' '

   CLEAR
   CLOSE ALL

   L= 8
   M= 23


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

   @ 6, 20 SAY 'Inventory - Sales / Sale Returns  '

IF _DOS
   SET COLOR TO 7/1
ELSE
SET COLOR TO
ENDIF

   @ L+0, M PROMPT ' A - Sales ............ Entry      '
   @ L+1, M PROMPT ' B - Sales ............ Correction '
   @ L+2, M PROMPT ' C - Sales ............ Deletion   '
   @ L+3, M PROMPT ' D - Invoice .......... Printing   '
   @ L+4, M PROMPT ' E - Verification Voucher Printing '
   @ L+5, M TO L+5, 61
   @ L+6, M PROMPT ' F - Sales Return ..... Entry      '
   @ L+7, M PROMPT ' G - Sales Return ..... Correction '
   @ L+8, M PROMPT ' H - Sales Return ..... Deletion   '
   @ L+9, M PROMPT ' I - SR. NO  .......... Printing   '
   @ L+10, M PROMPT 'J - Verification Voucher Printing '
   @ L+12, M PROMPT 'Q - Return to Previous Menu       '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=11
         CLEAR
         CLOSE DATABASE
         RETURN

      CASE MCHOICE=10
         CLEAR
         DO salr_vch IN SALEPROC.PRG

      CASE MCHOICE=9
         CLEAR
         DO salr_prt IN SALEPROC.PRG

      CASE MCHOICE=8
         CLEAR
         DO salr_del IN SALEPROC.PRG

      CASE MCHOICE=7
         CLEAR
         DO salr_cor IN SALEPROC.PRG

      CASE MCHOICE=6
         CLEAR
         DO salr_ent IN SALEPROC.PRG

      CASE MCHOICE=5
         CLEAR
         DO sale_vch IN SALEPROC.PRG

      CASE MCHOICE=4
         CLEAR
         DO sale_prt IN SALEPROC.PRG

      CASE MCHOICE=3
         CLEAR
         DO sale_del IN SALEPROC.PRG

      CASE MCHOICE=2
         CLEAR
         DO sale_cor IN SALEPROC.PRG

      CASE MCHOICE=1
         CLEAR
         DO sale_ent IN SALEPROC.PRG

   ENDCASE

ENDDO

RETURN
****************


************************
PROCEDURE TARG_MEN
**************************


DO WHILE .T.
   L= 8
   M= 23
   CLEAR

		IF _DOS
   			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

		IF _DOS
   			SET COLOR TO +7/1  
		ELSE
			SET COLOR TO
		ENDIF

   @ 6, 20 SAY 'Inventory - Target  File  Maintenance'

		IF _DOS
   			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

   @ L+0, M PROMPT ' A - Target .... Entry       '
   @ L+2, M PROMPT ' B - Target .... Correction  '
   @ L+4, M PROMPT ' C - Target .... Deletion    '
   @ L+6, M PROMPT ' D - Target .... Printing    '
   @ L+8, M PROMPT ' Q - Return to Previous Menu '

   DO bot IN TBL_PROC.PRG


   MCHOICE= 0

   MENU TO MCHOICE


   DO CASE

      CASE MCHOICE=1
         DO targ_ent

      CASE MCHOICE=2
         DO targ_cor

      CASE MCHOICE=3
         DO targ_del

      CASE MCHOICE=4
         DO targ_rep

      CASE MCHOICE=5
         CLEAR ALL
         CLEAR
         RETURN

   ENDCASE


ENDDO


RETURN
*************
