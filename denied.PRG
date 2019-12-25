******************
*PROCEDURE DENIED
******************

IF _WINDOWS
SET COLOR TO
ENDIF


CLEAR

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 28,65.200 SAY "I n v e n t o r y" ;
			FONT "Times New Roman", 14 ;
			STYLE "BIT"
		ENDIF


ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)

IF _DOS
   SET COLOR TO *+7/1
ENDIF

   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR

RETURN
************
