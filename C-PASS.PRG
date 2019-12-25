* 
* 
* C-PASS.FXP 
* ---------- 
*
CLOSE DATABASES
SELECT 1
USE SHARED Pass ALIAS PASS
STORE SPACE(6) TO MPASS1, MPASS2, MPASS3, MPASS4, MPASS5
STORE 0 TO MSR_NO1, MSR_NO2, MSR_NO3, MSR_NO4, MSR_NO5
DEFINE WINDOW CPASS FROM INT((SROWS()-15)/2), INT((SCOLS()-40)/2) TO INT((SROWS()-15)/2)+15, INT((SCOLS()-40)/2)+40 NOFLOAT NOCLOSE SHADOW TITLE 'CHANGE PASSWORD PROGRAMME' NOMINIMIZE PANEL COLOR SCHEME 5
ACTIVATE WINDOW SAME CPASS
@ 2, 6 SAY 'Password'
SELECT PASS
SET ORDER TO Sr_No
GOTO TOP
MSR_NO1 = SR_NO
MPASS1 = PASS
SKIP
MSR_NO2 = SR_NO
MPASS2 = PASS
SKIP
MSR_NO3 = SR_NO
MPASS3 = PASS
SKIP
MSR_NO4 = SR_NO
MPASS4 = PASS
SKIP
MSR_NO5 = SR_NO
MPASS5 = PASS
NCODE1 = SUBSTR(MPASS1,1,1)
NCODE2 = SUBSTR(MPASS1,2,1)
NCODE3 = SUBSTR(MPASS1,3,1)
NCODE4 = SUBSTR(MPASS1,4,1)
NCODE5 = SUBSTR(MPASS1,5,1)
NCODE6 = SUBSTR(MPASS1,6,1)
Mpass1 = chr(asc("&ncode1")-26)+chr(asc("&ncode2")-30)+chr(asc("&ncode3")-29)+chr(asc("&ncode4")-21)+chr(asc("&ncode5")-19)+chr(asc("&ncode6")-13)
NCODE1 = SUBSTR(MPASS2,1,1)
NCODE2 = SUBSTR(MPASS2,2,1)
NCODE3 = SUBSTR(MPASS2,3,1)
NCODE4 = SUBSTR(MPASS2,4,1)
NCODE5 = SUBSTR(MPASS2,5,1)
NCODE6 = SUBSTR(MPASS2,6,1)
Mpass2 = chr(asc("&ncode1")-26)+chr(asc("&ncode2")-30)+chr(asc("&ncode3")-29)+chr(asc("&ncode4")-21)+chr(asc("&ncode5")-19)+chr(asc("&ncode6")-13)
NCODE1 = SUBSTR(MPASS3,1,1)
NCODE2 = SUBSTR(MPASS3,2,1)
NCODE3 = SUBSTR(MPASS3,3,1)
NCODE4 = SUBSTR(MPASS3,4,1)
NCODE5 = SUBSTR(MPASS3,5,1)
NCODE6 = SUBSTR(MPASS3,6,1)
Mpass3 = chr(asc("&ncode1")-26)+chr(asc("&ncode2")-30)+chr(asc("&ncode3")-29)+chr(asc("&ncode4")-21)+chr(asc("&ncode5")-19)+chr(asc("&ncode6")-13)
NCODE1 = SUBSTR(MPASS4,1,1)
NCODE2 = SUBSTR(MPASS4,2,1)
NCODE3 = SUBSTR(MPASS4,3,1)
NCODE4 = SUBSTR(MPASS4,4,1)
NCODE5 = SUBSTR(MPASS4,5,1)
NCODE6 = SUBSTR(MPASS4,6,1)
Mpass4 = chr(asc("&ncode1")-26)+chr(asc("&ncode2")-30)+chr(asc("&ncode3")-29)+chr(asc("&ncode4")-21)+chr(asc("&ncode5")-19)+chr(asc("&ncode6")-13)
NCODE1 = SUBSTR(MPASS5,1,1)
NCODE2 = SUBSTR(MPASS5,2,1)
NCODE3 = SUBSTR(MPASS5,3,1)
NCODE4 = SUBSTR(MPASS5,4,1)
NCODE5 = SUBSTR(MPASS5,5,1)
NCODE6 = SUBSTR(MPASS5,6,1)
Mpass5 = chr(asc("&ncode1")-26)+chr(asc("&ncode2")-30)+chr(asc("&ncode3")-29)+chr(asc("&ncode4")-21)+chr(asc("&ncode5")-19)+chr(asc("&ncode6")-13)
@ 2, 20 SAY MSR_NO1 PICTURE '9'
@ 2, 30 GET MPASS1 PICTURE '@!X6'
@ 4, 20 SAY MSR_NO2 PICTURE '9'
@ 4, 30 GET MPASS2 PICTURE '@!X6'
@ 6, 20 SAY MSR_NO3 PICTURE '9'
@ 6, 30 GET MPASS3 PICTURE '@!X6'
@ 8, 20 SAY MSR_NO4 PICTURE '9'
@ 8, 30 GET MPASS4 PICTURE '@!X6'
@ 10, 20 SAY MSR_NO5 PICTURE '9'
@ 10, 30 GET MPASS5 PICTURE '@!X6'
READ
NCODE1 = SUBSTR(MPASS1,1,1)
NCODE2 = SUBSTR(MPASS1,2,1)
NCODE3 = SUBSTR(MPASS1,3,1)
NCODE4 = SUBSTR(MPASS1,4,1)
NCODE5 = SUBSTR(MPASS1,5,1)
NCODE6 = SUBSTR(MPASS1,6,1)
Mpass1 = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
NCODE1 = SUBSTR(MPASS2,1,1)
NCODE2 = SUBSTR(MPASS2,2,1)
NCODE3 = SUBSTR(MPASS2,3,1)
NCODE4 = SUBSTR(MPASS2,4,1)
NCODE5 = SUBSTR(MPASS2,5,1)
NCODE6 = SUBSTR(MPASS2,6,1)
Mpass2 = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
NCODE1 = SUBSTR(MPASS3,1,1)
NCODE2 = SUBSTR(MPASS3,2,1)
NCODE3 = SUBSTR(MPASS3,3,1)
NCODE4 = SUBSTR(MPASS3,4,1)
NCODE5 = SUBSTR(MPASS3,5,1)
NCODE6 = SUBSTR(MPASS3,6,1)
Mpass3 = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
NCODE1 = SUBSTR(MPASS4,1,1)
NCODE2 = SUBSTR(MPASS4,2,1)
NCODE3 = SUBSTR(MPASS4,3,1)
NCODE4 = SUBSTR(MPASS4,4,1)
NCODE5 = SUBSTR(MPASS4,5,1)
NCODE6 = SUBSTR(MPASS4,6,1)
Mpass4 = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
NCODE1 = SUBSTR(MPASS5,1,1)
NCODE2 = SUBSTR(MPASS5,2,1)
NCODE3 = SUBSTR(MPASS5,3,1)
NCODE4 = SUBSTR(MPASS5,4,1)
NCODE5 = SUBSTR(MPASS5,5,1)
NCODE6 = SUBSTR(MPASS5,6,1)
Mpass5 = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
GOTO TOP
REPLACE PASS WITH MPASS1
SKIP
REPLACE PASS WITH MPASS2
SKIP
REPLACE PASS WITH MPASS3
SKIP
REPLACE PASS WITH MPASS4
SKIP
REPLACE PASS WITH MPASS5
FLUSH
RELEASE WINDOW CPASS
RETURN
