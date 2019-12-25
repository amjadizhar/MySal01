clea
clea all
close all


SELECT STR(deptcode,2)+"-"+STR(empno,3)+"-"+name AS "empname",;
	   month,;
	   tot_pay + ot_amt AS "Paid";
FROM	slrydat;
WHERE year = 2000 OR year = 2001;
INTO DBF tmp1;
ORDER BY empname,month

close all

use tmp1
dele all for paid = 0
pack

do genxtab with 'tabemp'