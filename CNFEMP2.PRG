*       靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       � 08/19/96             CNFEMP2.SPR               11:31:23 �
*       �                                                         �
*       픔컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       � Author's Name                                           �
*       �                                                         �
*       � Copyright (c) 1996 Company Name                         �
*       � Address                                                 �
*       � City,     Zip                                           �
*       �                                                         �
*       � Description:                                            �
*       � This program was automatically generated by GENSCRN.    �
*       �                                                         �
*       聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�


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

*       靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       �                MS-DOS Window definitions                �
*       �                                                         �
*       聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*


*       靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       �              CNFEMP2/MS-DOS Screen Layout               �
*       �                                                         �
*       聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*

#REGION 1
@ 4,10 SAY "Employee Name:" ;
	SIZE 1,14, 0
@ 4,25 GET employee.salutation ;
	SIZE 1,2 ;
	DEFAULT " " ;
	DISABLE
@ 4,28 GET employee.name ;
	SIZE 1,20 ;
	DEFAULT " " ;
	DISABLE
@ 6,10 SAY "Date of Confirmation:" ;
	SIZE 1,21, 0
@ 6,32 GET employee.doc ;
	SIZE 1,8 ;
	DEFAULT {  /  /  } ;
	PICTURE "@E" ;
	WHEN _rd40op5bn() ;
	VALID _rd40op5i0()



READ MODAL


#REGION 0
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF


*       靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       � _RD40OP5BN           employee.doc WHEN                  �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         CNFEMP2,     Record Number:    6   �
*       � Variable:            employee.doc                       �
*       � Called By:           WHEN Clause                        �
*       � Object Type:         Field                              �
*       � Snippet Number:      1                                  �
*       �                                                         �
*       聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*
FUNCTION _rd40op5bn     &&  employee.doc WHEN
#REGION 1
REPLACE employee.doc WITH date()
RETURN .T.

*       靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*       �                                                         �
*       � _RD40OP5I0           employee.doc VALID                 �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         CNFEMP2,     Record Number:    6   �
*       � Variable:            employee.doc                       �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      2                                  �
*       �                                                         �
*       聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*

FUNCTION _rd40op5i0     &&  employee.doc VALID
#REGION 1
IF employee.doc - employee.doa < 180
	YesNo = space(1)
	DEFINE WINDOW WYesNo FROM 8,10 TO 16,55 TITLE "Caution"
	ACTIVATE WINDOW WYesNo
	@ 0,1 SAY "Date of Appointment:"
	@ ROW(),COL()+1 SAY employee.doa PICT "@E"
	@ ROW()+1,1 SAY "The Confirmation Date Should Be: "
	@ ROW(),COL() SAY doa+180 PICT "@E"
	@ ROW()+2,1 SAY "The Period is"
	@ ROW()+1,1 SAY "Less Than 180 Days. Is This OK ?"
	@ ROW(),COL()+2 GET YesNo PICT "@!" VALID INLIST(YesNo,'Y','N');
				ERROR "(Y)es,(N)o";
				MESSAGE "(Y)es, (N)o"
	READ
	
		IF YesNo = "Y"

			RELEASE WINDOW WYesNo
			RETURN .T.
		ELSE

			RELEASE WINDOW WYesNo
			RETURN .F.
		ENDIF
	
ENDIF

RETURN .T.