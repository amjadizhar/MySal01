********************************************
* PROGRAM: EMPBIOS1.PRG
* Program for Menu Choice 
* Employee Bio Data & Salary
********************************************

DO SetPbVars
DO IniSet1
DO OpenDbfs

**********************
PROCEDURE SetPbVars
**********************
PUBLIC mmcode,myear
RETURN
**********************


******************
PROCEDURE IniSet1
******************

CLEAR
HIDE POPUP mainmenu

SET TALK OFF
SET SYSMENU OFF
SET SAFETY OFF

SET FUNCTION F1 TO
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
SET FUNCTION F10 TO
SET FUNCTION F11 TO
SET FUNCTION F12 TO

RETURN
******************


********************
PROCEDURE OpenDbfs
********************

USE attend
STORE Mcode TO mmcode
STORE year TO myear

USE

SELECT 1
USE employee ORDER empno

SELECT 2
USE deptt ORDER deptcode

SELECT 3
USE salary ORDER empsrl

SELECT 4
USE salcode ORDER salcode

SELECT 5
USE attend ORDER empsrl

SELECT 6
USE ovtime ORDER empsrl

SELECT 7
USE artime ORDER empsrl

SELECT 8
USE dptime ORDER empsrl

SELECT 9
USE deduct ORDER empsrl
SET FILTER TO mcode = mmcode AND year = myear

SELECT 10
USE saldtail ORDER empsrl
SET FILTER TO mcode = mmcode AND year = myear

SELECT salary
SET RELATION TO salcode INTO salcode ADDITIVE


SELECT employee
SET RELATION TO empsrl INTO salary ADDITIVE
SET RELATION TO empsrl INTO attend ADDITIVE
SET RELATION TO empsrl INTO ovtime ADDITIVE
SET RELATION TO empsrl INTO artime ADDITIVE
SET RELATION TO empsrl INTO dptime ADDITIVE
SET RELATION TO empsrl INTO deduct ADDITIVE
SET RELATION TO empsrl INTO saldtail ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE


? mmcode
? myear

RETURN
********************************