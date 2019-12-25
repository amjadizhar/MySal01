* Add G.P. Fund in Duduction File
Set Talk Off
Set Stat On
Sele 1
Use Deduct Alia Deduct
Set Filter To MCode=01 .And. Year=2000


Sele 2
Use Salary Alia Salary
Set Order To Empno2

Sele Deduct
Replace All Pfund With 0
Goto Top
MDeptcode=0
MEmpno=0
MPFund=0
Do While .Not. Eof()
	MDeptcode=Deptcode
	MEmpno=Empno
	Sele Salary
	Set Order To Empno2
	Seek Str(MDeptcode)+Str(MEmpno)
	brow
	MPFund=Basic*6/100
	Sele Deduct
	Replace PFund With round(MPFund,0)
	Skip
	MDeptcode=0
	MEmpno=0
	MPFund=0
Enddo
Close All
Clear All
Clear