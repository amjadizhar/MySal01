*******************
PROCEDURE RepEmpSr
*******************
USE employee ORDER empno
REPLACE ALL EMPSRL WITH RECNO()
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE attend ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE artime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE ovtime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE dptime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE salary ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL

SELECT 1
USE deduct ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL

SELECT 1
USE saldtail ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL


SELECT 1
USE leaves ORDER EMPNO
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
REINDEX
CLEAR
CLEAR ALL



RETURN
*************************
