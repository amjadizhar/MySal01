* PROGRAM: CLOSSTK.PRG
* WRITTEN ON : 02-07-99.
* PROGRAMMER: AMJAD IZHAR
* This Revision: 04-11-2015
* CLOSE STOCK FILES AND OPEN NEW YEAR BALANCES

USE stock
DELETE ALL FOR NOT EMPTY(doc_no)
PACK

SET DATE TO BRITISH
* Entering the date of the new begining year
mDate = date()
CLEAR
@ 10,8 SAY "Please Enter the New Year Begining Date: " FONT 'FoxFont',11
@ 12,8 GET mDate PICTURE "@E" FONT 'FoxFont',11
READ

@ 10,0 TO 18,79 CLEAR
@ 10,8 SAY "Replacing Master with Begining Year date....." FONT 'FoxFont',11

REPLACE ALL date WITH mdate


*mDate = {01/01/00}
*REPLACE ALL DATE WITH mDate

REPLACE ALL sr_no WITH 1

USE item-bal
INDEX ON str(i1)+str(i2)+str(i3) TAG item 

CLEA ALL
CLOSE ALL

SELECT 1
USE stock

SELECT 2
USE item-bal ORDER item

SELECT 1
SET RELATION TO str(i1)+str(i2)+str(i3) INTO b ADDITIVE

REPLACE ALL qty_bal WITH b.qty
REPLACE ALL open_bal WITH b.qty

SELECT b

REPLACE ALL Open_bal WITH qty
REPLACE ALL sr_no WITH 1

SELECT 1

BROW FIELDS;
	date,i1,i2,i3,sr_no,qty_bal,open_bal,;
	b.qty,b.open_bal
	
CLEA ALL
CLOSE ALL

USE st-tran
ZAP

*USE abc
*ZAP

USE st-key
ZAP

CLEA ALL
CLOSE ALL
