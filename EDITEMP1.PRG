*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º 08/25/96            EDITEMP1.SPR               11:26:28 º
*       º This Revision: 26-08-2017                               º
*       ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*       º                                                         º
*       º Author's Name  Amjad Izhar                              º
*       º                                                         º
*       º Copyright (c) 1996 CosmoSoft Systems                    º
*       º Address   54-Zeenat Block Allama Iqbal Town             º
*       º City,Lahore   Zip 54570                                 º
*       º                                                         º
*       º Description:                                            º
*       º Continuation from EditEmp.PRG                           º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½


#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS
SET DATE TO BRITISH
SET CENT ON

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º                MS-DOS Window definitions                º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

IF NOT WEXIST("newemp1") ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.PJX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.SCX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.MNX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.PRG" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.FRX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.QPR"
	DEFINE WINDOW newemp1 ;
		FROM INT((SROW()-24)/2),INT((SCOL()-79)/2) ;
		TO INT((SROW()-24)/2)+23,INT((SCOL()-79)/2)+78 ;
		TITLE "Edit Employee" ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		COLOR SCHEME 1
ENDIF


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º              EDITEMP1/MS-DOS Screen Layout              º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

#REGION 1
IF WVISIBLE("newemp1")
	ACTIVATE WINDOW newemp1 SAME
ELSE
	ACTIVATE WINDOW newemp1 NOSHOW
ENDIF
@ 1,2 SAY "Employee Code:" ;
	SIZE 1,14, 0

@ 1,17 GET employee.deptcode ;
	SIZE 1,2 ;
	DEFAULT 0 ;
	PICTURE "@L" ;
	DISABLE

@ 1,19 SAY "-" ;
	SIZE 1,1, 0

@ 1,20 GET employee.empno ;
	SIZE 1,3 ;
	DEFAULT 0 ;
	PICTURE "@L" ;
	DISABLE

@ 1,25 SAY "Department:" ;
	SIZE 1,11, 0

@ 1,37 GET deptt.deptname ;
	SIZE 1,20 ;
	DEFAULT " " ;
	DISABLE

@ 3,2 SAY "Employee Name:" ;
	SIZE 1,14, 0

@ 3,17 GET employee.salutation ;
	SIZE 1,2 ;
	DEFAULT " " ;
	PICTURE "!x" ;
	VALID _rda0oiu2p() ;
	MESSAGE "Enter Mr or Ms"

@ 3,42 SAY "Father Name:" ;
	SIZE 1,12, 0

@ 7,2 SAY "Address:" ;
	SIZE 1,8, 0

@ 11,5 SAY "City:" ;
	SIZE 1,5, 0

@ 7,41 SAY "Phone Numbers:" ;
	SIZE 1,14, 0

@ 9,41 SAY "Phone No.1" ;
	SIZE 1,10, 0

@ 10,41 SAY "Phone No.2" ;
	SIZE 1,10, 0

@ 11,41 SAY "Phone PP." ;
	SIZE 1,9, 0

@ 5,2 SAY "N.I.C. Number:" ;
	SIZE 1,14, 0

@ 14,2 SAY "Date of Appointment:" ;
	SIZE 1,20, 0

@ 16,2 SAY "Designation:" ;
	SIZE 1,12, 0

@ 18,2 SAY "Reference 1:" ;
	SIZE 1,12, 0

@ 20,2 SAY "Reference 2:" ;
	SIZE 1,12, 0

@ 20,48 SAY "Notes:" ;
	SIZE 1,6, 0

@ 5,33 SAY "Working:" ;
	SIZE 1,8, 0

@ 5,46 SAY "On Probation:" ;
	SIZE 1,13, 0

@ 3,20 GET salary.name ;
	SIZE 1,20 ;
	DEFAULT " "

@ 3,55 GET employee.father ;
	SIZE 1,20 ;
	DEFAULT " " ;
	VALID _rda0oiuew() ;
	MESSAGE "Enter Employee's Father Full Name"

@ 5,17 GET employee.nicno ;
	SIZE 1,15 ;
	DEFAULT " " ;
	PICTURE "99999-9999999-9" ;
	MESSAGE "Enter Employee's National Identity Card Number"

@ 5,42 GET salary.working ;
	SIZE 1,3 ;
	DEFAULT .F.

@ 5,60 GET salary.probation ;
	SIZE 1,3 ;
	DEFAULT .F.

@ 8,2 GET employee.addr1 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rda0oiunp() ;
	MESSAGE "Enter employee's Permanent Address Line Number 1"

@ 9,2 GET employee.addr2 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rda0oiuu9() ;
	MESSAGE "Enter Employee's Permanent Address Line Number 2"

@ 10,2 GET employee.addr3 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rda0oiv16() ;
	MESSAGE "Enter employee's Permanent Address Line Number 3"

@ 11,12 GET employee.city ;
	SIZE 1,15 ;
	DEFAULT " " ;
	VALID _rda0oiv7b() ;
	MESSAGE "Enter Employee's City of above address"

@ 9,53 GET employee.phone1 ;
	SIZE 1,16 ;
	DEFAULT " " ;
	PICTURE "################" ;
	MESSAGE "Enter First Contact Phone No. Like 0333-6938898"

@ 10,53 GET employee.phone2 ;
	SIZE 1,16 ;
	DEFAULT " " ;
	PICTURE "################" ;
	MESSAGE "Enter Second Contact Phone No. Like 0333-6938898"

@ 11,53 GET employee.phonepp ;
	SIZE 1,16 ;
	DEFAULT " " ;
	PICTURE "################" ;
	MESSAGE "Enter Employee's nearby phone number. In case of failure on above phones"

@ 14,24 GET employee.doa ;
	SIZE 1,10 ;
	DEFAULT {  /  /    } ;
	PICTURE "@E" ;
	MESSAGE "Date of appointment from where you want to start this employee attendence"

@ 16,15 GET employee.designatn ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rda0oivha() ;
	MESSAGE "Enter Employee Designation Like Manager Production etc."

@ 18,15 GET employee.ref1 ;
	SIZE 1,30 ;
	DEFAULT " " ;
	VALID _rda0oivoj() ;
	MESSAGE "Employee's acquaintances or anyone who has refered him/her"

@ 20,15 GET employee.ref2 ;
	SIZE 1,30 ;
	DEFAULT " " ;
	VALID _rda0oivvj() ;
	MESSAGE "Employee's acquaintances or anyone who has refered him/her"

@ 20,55 GET notes ;
	SIZE 1,5 ;
	DEFAULT " " ;
	PICTURE "@! xxxxx" ;
	WHEN _rda0oiw2p() ;
	MESSAGE "Any Other Notes Like Employee's Hobbies, Interests, Researches Etc."

IF NOT WVISIBLE("newemp1")
	ACTIVATE WINDOW newemp1
ENDIF

READ MODAL

RELEASE WINDOW newemp1

#REGION 0
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIU2P           employee.salutation VALID          º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:    9  º
*       º Variable:            employee.salutation                º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      1                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiu2p     &&  employee.salutation VALID
#REGION 1
IF INLIST(salutation,'Mr','Ms') = .F.
WAIT WINDOW "Mr or Ms" TIMEOUT 1
RETURN .F.
ENDIF
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIUEW           employee.father VALID              º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   26  º
*       º Variable:            employee.father                    º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      2                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiuew     &&  employee.father VALID
#REGION 1
REPLACE father WITH PROPER(father)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIUNP           employee.addr1 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   30  º
*       º Variable:            employee.addr1                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      3                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiunp     &&  employee.addr1 VALID
#REGION 1
REPLACE addr1 WITH PROPER(addr1)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIUU9           employee.addr2 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   31  º
*       º Variable:            employee.addr2                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      4                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiuu9     &&  employee.addr2 VALID
#REGION 1
REPLACE addr2 WITH PROPER(addr2)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIV16           employee.addr3 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   32  º
*       º Variable:            employee.addr3                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      5                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiv16     &&  employee.addr3 VALID
#REGION 1
REPLACE addr3 WITH PROPER(addr3)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIV7B           employee.city VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   33  º
*       º Variable:            employee.city                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      6                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiv7b     &&  employee.city VALID
#REGION 1
REPLACE city WITH PROPER(city)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIVHA           employee.designatn VALID           º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   38  º
*       º Variable:            employee.designatn                 º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      7                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oivha     &&  employee.designatn VALID
#REGION 1
REPLACE designatn WITH PROPER(designatn)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIVOJ           employee.ref1 VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   39  º
*       º Variable:            employee.ref1                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      8                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oivoj     &&  employee.ref1 VALID
#REGION 1
REPLACE ref1 WITH PROPER(ref1)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIVVJ           employee.ref2 VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   40  º
*       º Variable:            employee.ref2                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      9                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oivvj     &&  employee.ref2 VALID
#REGION 1
REPLACE ref2 WITH PROPER(ref2)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RDA0OIW2P           notes WHEN                         º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         EDITEMP1,     Record Number:   41  º
*       º Variable:            notes                              º
*       º Called By:           WHEN Clause                        º
*       º Object Type:         Field                              º
*       º Snippet Number:      10                                 º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rda0oiw2p     &&  notes WHEN
#REGION 1
DEFINE WINDOW NtsWin1 FROM 5,31 TO 19,74;
 TITLE "Other Notes for "+ALLTRIM(salutation)+"."+ALLTRIM(name);
 FOOTER "Press Esc to Exit";
 CLOSE FLOAT GROW SHADOW ZOOM

PUSH KEY
ON KEY LABEL ESC CLOSE MEMO notes
MODI MEMO notes WINDOW NtsWin1
ON KEY LABEL ESC
POP KEY

RETURN .T.