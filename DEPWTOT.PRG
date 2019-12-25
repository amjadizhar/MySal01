* Department wise total of Over Time Query

CLEAR
CLOSE DATA

SELECT 15
USE attend ALIAS atnd

SELECT 16
USE month ORDER mcode ALIAS mnth

SELECT 15
SET RELATION TO mcode INTO mnth

SELECT 17
USE ovtime ALIAS ovtm


SELECT deptcode,sum(d1ot),sum(d2ot),sum(d3ot),;
	   sum(d4ot),sum(d5ot),sum(d6ot),sum(d7ot),;
	   sum(d8ot),sum(d9ot),sum(d10ot),sum(d11ot),;
	   sum(d12ot),sum(d13ot),sum(d14ot),sum(d15ot),;
	   sum(d16ot),sum(d17ot),sum(d18ot),sum(d19ot),;
	   sum(d20ot),sum(d21ot),sum(d22ot),sum(d23ot),;
	   sum(d24ot),sum(d25ot),sum(d26ot),sum(d27ot),;
	   sum(d28ot),sum(d29ot),sum(d30ot),sum(d31ot);
FROM ovtm;
WHERE deptcode BETWEEN 1 AND 9;
GROUP BY deptcode;
INTO CURSOR dptwtot 

