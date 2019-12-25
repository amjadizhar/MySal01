* PROGRAM : INDXPROC.PRG
* TO INDEX ALL FILES IN 
* ACCOUNTING,INVENTORY,PAYROLL,INVOICE,EXPORT
* SOFTWARES
* WRITTEN BY : AMJAD IZHAR
* WRITTEN ON: 8TH JUNE,2005
* THIS REVISION: 8TH JUNE,2005
******************************************************

SET EXCLUSIVE ON

DO DbfsPack
DO IndexInvt

************************
PROCEDURE DbfsPack
************************

CLEAR

*CLEA ALL
*CLOSE ALL

SELECT 1
USE item-bal.dbf ALIAS item_bal EXCLUSIVE
PACK




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





RETURN
******************



****************************
PROCEDURE IndexAcc
****************************



RETURN
*****************************

*****************************
PROCEDURE IndexInvt
*****************************

CLEAR
*CLEAR ALL
*CLOSE ALL

SET EXCLUSIVE ON
SET TALK OFF
SET SAFE OFF
SET DATE TO BRIT
SET CENT ON

USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item_bal
FLUSH
USE

USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal



CLEAR
CLOSE ALL

SET SAFETY OFF
SET DELETED OFF
SET EXCLUSIVE OFF

IF _DOS
 SET COLOR TO 7/1
ENDIF

CLEAR
*@ 2, 2 SAY 'Reindex/Re-organize Data Files'

P= ' '


CLEAR

*@ 2, 20 SAY 'Re-indexing Data Files  --- Please wait'
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

USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
INDEX ON ITEM TAG itm1_idx

USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1

USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itmd_idx

USE stock
INDEX ON STR(D_D1,2)+STR(D_D2,2)+STR(D_D3,3) TAG std_idx
INDEX ON STR(CITY_CODE,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG cstck
INDEX ON STR(ZONE_CODE,2)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG zstck
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG stck_idx
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TAG itemidx1
INDEX ON DOC_NO TAG itemidx2
INDEX ON DATE TAG stk_idx

USE loc
INDEX ON LOC_CODE TAG loc_idx

USE dept
INDEX ON DEPT_CODE TAG dept_idx

USE mach
INDEX ON STR(DEPT_CODE,3)+STR(MACH_CODE,4) TAG mach_idx

USE zone
INDEX ON ZONE_CODE TAG zone_idx

USE city
INDEX ON CITY_CODE TAG city_idx

USE st-tran
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1

USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx

USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG pord_idx

USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sord_idx

USE formula
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4)+STR(RI1,2)+STR(RI2,2)+STR(RI3,4) TAG for_idx
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4) TAG for_idx1

CLEAR
CLOSE ALL
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN
*******************************
