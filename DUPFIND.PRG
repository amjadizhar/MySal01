* PROGRAM: DUPFIND.PRG
* FOR USMAN'S SALE PROGRAM
* TO FIND DUPLICATE RECORDS IN ITRAN.DBF
* PROGRAMMER : AMJAD IZHAR
* WRITTEN ON : 19-01-2001
* LAST REVISION: 19-01-2001
**********************************************************
* FILE USED ITRAN.DBF

* STRUCTURE OF ITRAN.DBF
* I1		N	2.0
* I2		N	2.0
* I3		N	3.0
* QTY		N	4.0
* SR_NO		N	2.0
* C1		N	2.0
* C2		N	2.0
* C3		N	3.0
* DATE		D	8
* DOC_TYPE	N	1.0
* DOC_NO	C	6
* AMOUNT	N	12.2
* NAMOUNT	N	12.2

CLEAR
CLEA ALL
CLOSE ALL

SELECT 0 AS "Doc_type",SPACE(6) AS "Doc_No";
FROM itran;
INTO DBF dupfind.dbf
ZAP


SELECT DISTINCT qty,sr_no,doc_type,doc_no;
FROM itran;
INTO DBF image1.dbf

SELECT qty,sr_no,doc_type,doc_no;
FROM itran;
INTO DBF image2.dbf

INDEX ON STR(qty,4)+STR(sr_no,2)+STR(doc_type,1)+doc_no TAG RowCont ADDITIVE

CLOSE ALL

SELECT 1
USE image1
SELECT 2
USE image2
SET ORDER TO RowCont
SELECT 3
USE dupfind

SELECT 1
SET RELATION TO STR(qty,4)+STR(sr_no,2)+STR(doc_type,1)+doc_no INTO image2 ADDITIVE
SET SKIP TO image2

PUBLIC mRecNO

mRecNo = RECNO()
mDocType = doc_type
mDocNo = doc_no


DO WHILE NOT EOF()
SKIP
IF RECNO() = mRecNo
wait windows "Press any key to Continue . . ."
SELECT dupfind
append blank
repl doc_type WITH mDocType,doc_no WITH mDocNo
SELECT 1
ENDIF

mRecNo = RECNO()
mDocType = doc_type
mDocNo = doc_no

ENDDO

