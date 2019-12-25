*-----------------------------------------------*
*Program to Enter/Modifi/Delete The City Code	*
*Author		:	Muhammad Usman					*
*Date		:	12-04-2000						*
*Prog		:	City							*
*Last Modify:	12-04-2000						*
*-----------------------------------------------*
* Use Files
Close Databases

Sele 1
Use City Alia City Shared

Sele 2
Use Ldgrcod Alia Ldgrcod Shared

*-----------------------------------------------
*Veriable Initialization
MC_Code=Spac(4)					&& City Code
MName=Spac(30)					&& City Name
MOpt=0							&& Option
*-----------------------------------------------
* Window Defination
	DEFINE WINDOW city ;
		FROM INT((SROW()-16)/2),INT((SCOL()-69)/2) ;
		TO INT((SROW()-16)/2)+15,INT((SCOL()-69)/2)+68 ;
		TITLE " CITY HEAD MAINTENANCE " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW city SAME
*-----------------------------------
Do While .T.
	Clear
	Do DspHedPro
	@ 13,18 SAY "Press F1 to see City Code List" Color GR+
	ON KEY LABEL F1 DO ShwCtyPro In Proc1
	@ 3,30 Get MC_Code Pict"@!" Valid MC_Code#Spac(4) Error 'City Code Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind City
		Return
		Exit
	Endif
	Sele City
	Set Order To C_Code
	Seek MC_Code
	If Opt="New"
		If Found()	
			Wait Wind 'City Code Already Exist'
			MC_Code=Spac(4)					&& City Code
			Loop
		Endif
	Endif
	If Opt="Modi"
		If .Not. Found()	
			Wait Wind 'City Code Does Not Exist'
			MC_Code=Spac(4)					&& City Code
			Loop
		Endif
		@ 13,01 To 13,62 Clear
		Do While .T.
			@ 13,18 Get MOpt Func '*H \<Cancel;\<Modify;\<Delete' Size 1,10 Color Scheme 19
			Read
		    If Readkey() = 15 .or. Readkey() = 271			&& Enter Key
				Exit
		    Endif
		Enddo
		Do Case
			Case MOpt = 1
			Optflag = 'C'
			Case MOpt = 2
			Optflag = 'M'
			Case MOpt = 3
			Optflag = 'D'
		Endcase
		@ 13,01 To 13,62 Clear
		If Optflag='C'
			MC_Code=Spac(4)					&& City Code
			Loop
		Endif
		If Optflag='M'
			Sele City
			MName=Name
		Endif
		If Optflag='D'
			Sele Ldgrcod 
			Set Order To C_Code
			Seek MC_Code
			If Found()
				Wait Wind 'This Code Already Exist In Ledger Head Maintenance'
			Else
				Sele City
				Dele
			Endif
			MC_Code=Spac(4)					&& City Code
			Loop
		Endif
	Endif
	@ 8,30 Get MName Pict"@!" Valid MName#Spac(30) Error 'City Name Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop		
	Endif
	Do SavDatPro
	MC_Code=Spac(4)					&& City Code
	MName=Spac(30)					&& City Name
EndDo
Rele Wind City
Return
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 03,05 Say "City Code"
@ 08,05 Say "City Name"
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele City
	Set Order To C_Code
	Seek MC_Code
	If .Not. Found()
		Append Blank
	Endif
	Replace C_Code With MC_Code
	Replace Name With MName
	Flush
