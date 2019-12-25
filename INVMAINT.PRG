* PROGRAM: INVMAINT.PRG
* TO HAVE PROCS FOR MAINTENANCE FOR INVENTORY FILES
* WRITTEN BY AMJAD IZHAR
* WRITTEN ON 01-06-2005
****************************************************

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

   @ 6, 27 SAY 'Inventory System Maintenance Menu'

IF _DOS
   SET COLOR TO 7/1  
ENDIF 


         IF _WINDOWS
         SET COLOR TO
         ENDIF

   @ 9, 21 PROMPT ' 1 - Backup Data Files To Diskette (Copy)  '
   @ 11, 21 PROMPT ' 2 - Backup Data Files To Diskette (Backup)'
   @ 13, 21 PROMPT ' 3 - Re-Index Data Files     '
   @ 15, 21 PROMPT ' 4 - Sort Vouchers Date Wise '
   @ 17, 21 PROMPT ' 9 - Return to Previous Menu '

   CANS= 0

		IF _DOS
			@ 22,65 SAY "Inventory"
		ENDIF



		IF _WINDOWS
			@ 36.077,75.200 SAY "Inventory" ;
			FONT "Times New Roman", 20 ;
			STYLE "BIT"
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

         DO reindex IN INVT2LB1.PRG

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


******************
PROCEDURE REINDEX
******************

CLEAR
CLOSE ALL

SET SAFETY OFF
SET DELETED OFF
SET EXCLUSIVE OFF

IF _DOS
 SET COLOR TO 7/1
ENDIF

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
SET EXCLUSIVE ON
CLOSE ALL

USE item EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
INDEX ON ITEM TAG itm1_idx

USE ldgrcode EXCLUSIVE
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1

USE item-bal EXCLUSIVE
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itmd_idx

USE stock EXCLUSIVE
INDEX ON STR(D_D1,2)+STR(D_D2,2)+STR(D_D3,3) TAG std_idx
INDEX ON STR(CITY_CODE,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG cstck
INDEX ON STR(ZONE_CODE,2)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG zstck
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG stck_idx
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TAG itemidx1
INDEX ON DOC_NO TAG itemidx2
INDEX ON DATE TAG stk_idx

USE loc EXCLUSIVE
INDEX ON LOC_CODE TAG loc_idx

USE dept EXCLUSIVE
INDEX ON DEPT_CODE TO dept_idx

USE mach EXCLUSIVE
INDEX ON STR(DEPT_CODE,3)+STR(MACH_CODE,4) TAG mach_idx

USE zone EXCLUSIVE
INDEX ON ZONE_CODE TAG zone_idx

USE city EXCLUSIVE
INDEX ON CITY_CODE TAG city_idx

USE st-tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1

USE st-key EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx

USE p-order EXCLUSIVE
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG pord_idx

USE s-order EXCLUSIVE
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sord_idx

USE formula EXCLUSIVE
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4)+STR(RI1,2)+STR(RI2,2)+STR(RI3,4) TAG for_idx
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4) TAG for_idx1

CLEAR
CLOSE ALL
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN
*************
