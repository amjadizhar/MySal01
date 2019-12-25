************************
* PROGRAM : INIFILES.PRG
* Programs to Initialize files keeping previous
* chart of account
* AUTHOR : Amjad Izhar
* written : 9 oct. 1996
* Revised Again On : 19-01-2001 7:15 PM
*******************************
* FIRST unpost ledgers

CLOSE DATA

USE master.dbf
REPLACE ALL bl_amount WITH 0
REPLACE ALL status WITH '  '

USE tran.dbf
ZAP

USE tran-key.dbf
ZAP

USE ldgrcode.dbf
REPLACE ALL ldgr_bal WITH 0
REPLACE ALL status WITH '  '
REPLACE ALL open_bal WITH 0
REPLACE ALL open_stat WITH '  '



IF FILE("tran-rep.dbf")
	USE tran-rep.dbf
	ZAP
ENDIF

IF FILE("trl-work.dbf")
	USE trl-work.dbf
	ZAP
ENDIF

IF FILE("mast-ref.dbf")
	USE mast-ref.dbf
	ZAP
ENDIF

IF FILE("pl-dtl.dbf")
	USE pl-dtl.dbf
	ZAP
ENDIF

IF FILE("mldgr.dbf")
	USE mldgr.dbf
	ZAP
ENDIF

IF FILE("tab2.dbf")
	USE tab2.dbf
	ZAP
ENDIF

IF FILE("rep1.dbf")
	USE rep1.dbf
	ZAP
ENDIF

IF FILE("mtran.dbf")
	USE mtran.dbf
	ZAP
ENDIF

IF FILE("mmast.dbf")
	USE mmast.dbf
	ZAP
ENDIF
