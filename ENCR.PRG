* PROGRAM: ENCR.PRG
* PROGRAM TO ENCRYPT A PW FIELD


SET TALK OFF
SET SYSMENU OFF

ON KEY LABEL F1 ?? CHR(7)
ON KEY LABEL F10 ?? CHR(7)
CLEAR MACROS


PUBLIC mm1,mm2,Title1
CLEAR

=BrowWin1() WITH "Users List"


USE usr ORDER code


BROW FIELDS;
usrcode :1 :H=" ",;
name :20 :H="Menu Name",;
cv :H = "Password" :P = "@!";
FREEZE cv;
WINDOW BrUsr;
WHEN back();
VALID UpdPw();
TITLE space(35)




REPLACE ALL cv WITH " "
CLOSE DATA
CLEAR
RELEASE WINDOWS
RETURN


PROCEDURE back


STORE SUBSTR(pw,1,1) TO ncode1
STORE SUBSTR(pw,2,1) TO ncode2
STORE SUBSTR(pw,3,1) TO ncode3
STORE SUBSTR(pw,4,1) TO ncode4
STORE SUBSTR(pw,5,1) TO ncode5
STORE SUBSTR(pw,6,1) TO ncode6
STORE SUBSTR(pw,7,1) TO ncode7
STORE SUBSTR(pw,8,1) TO ncode8
STORE SUBSTR(pw,9,1) TO ncode9


mm1 = chr(asc("&ncode1")+30)+;
	  chr(asc("&ncode2")+28)+;
	  chr(asc("&ncode3")+29)+;
	  chr(asc("&ncode4")+21)+;
	  chr(asc("&ncode5")+19)+;
	  chr(asc("&ncode6")+13)+;
	  chr(asc("&ncode7")+12)+;
	  chr(asc("&ncode8")+18)+;
	  chr(asc("&ncode9")+19)


REPLACE cv WITH mm1


RETURN .T.


PROCEDURE UpdPw

STORE SUBSTR(cv,1,1) TO ncode1
STORE SUBSTR(cv,2,1) TO ncode2
STORE SUBSTR(cv,3,1) TO ncode3
STORE SUBSTR(cv,4,1) TO ncode4
STORE SUBSTR(cv,5,1) TO ncode5
STORE SUBSTR(cv,6,1) TO ncode6
STORE SUBSTR(cv,7,1) TO ncode7
STORE SUBSTR(cv,8,1) TO ncode8
STORE SUBSTR(cv,9,1) TO ncode9

CLEAR
mm2 = chr(asc("&ncode1")-30)+;
	  chr(asc("&ncode2")-28)+;
	  chr(asc("&ncode3")-29)+;
	  chr(asc("&ncode4")-21)+;
	  chr(asc("&ncode5")-19)+;
	  chr(asc("&ncode6")-13)+;
	  chr(asc("&ncode7")-12)+;
	  chr(asc("&ncode8")-18)+;
	  chr(asc("&ncode9")-19)

REPLACE pw WITH mm2
RETURN




*****************
FUNCTION BrowWin1
*****************
Title1 = "Menu Password List"

DEFINE WINDOW full1 FROM 0,0 TO 24,79 COLOR W/B
ACTIVATE WINDOW full1

DEFINE WINDOW BrUsr FROM 5,25 TO 18,65 COLOR N/G
@ 3,24 FILL TO 3,64 COLOR N/R
@ 3,36 SAY Title1 COLOR W/R

RETURN
******
