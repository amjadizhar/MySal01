*-----------------------------------------------*
*Program to Enter/Modifi/Delete The Ledger Code	*
*Author		:	Muhammad Usman					*
*Date		:	19-01-2000						*
*Prog		:	Ldgrcod							*
*Last Modify:	19-01-2000						*
*-----------------------------------------------*
* Use Files
Close Databases

Sele 1
Use Ldgrcod Alia Ldgrcod Shared

Sele 2
Use Tran Alia Tran Shared

Sele 3
Use City Alia City Shared

*-----------------------------------------------
*Veriable Initialization
MC1=0								&& First Ledger Code
MC2=0								&& Second Ledger Code
MC3=0								&& Thired Ledger Code
MTitle=Spac(30)						&& Title Of Ledger Code
MOpt=0								&& Option
MC_Code=Spac(4)						&& City Code
*-----------------------------------------------
* Window Defination
	DEFINE WINDOW Ldgrcod ;
		FROM INT((SROW()-16)/2),INT((SCOL()-69)/2) ;
		TO INT((SROW()-16)/2)+15,INT((SCOL()-69)/2)+68 ;
		TITLE " LEDGER HEAD MAINTENANCE " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW Ldgrcod SAME
*-----------------------------------
Do While .T.
	Clear
	Do DspHedPro
	@ 12,18 SAY "Press F1 to see Leadger Heads List" Color GR+
	ON KEY LABEL F1 DO ShwLdgPro In Proc1
	@ 2,30 Say "  -  -   "
	@ 2,30 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 2,33 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 2,36 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind Ldgrcod
		Return
		Exit
	Endif
	Sele Ldgrcod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If Opt="New"
		If Found()	
			Wait Wind 'Ledger Head Code Already Exist'
			MC1=0								&& First Ledger Code
			MC2=0								&& Second Ledger Code
			MC3=0								&& Thired Ledger Code
			Loop
		Endif
	Endif
	If Opt="Modi"
		If .Not. Found()	
			Wait Wind 'Ledger Head Code Not Exist'
			MC1=0								&& First Ledger Code
			MC2=0								&& Second Ledger Code
			MC3=0								&& Thired Ledger Code
			Loop
		Endif
		@ 12,01 To 12,62 Clear
		Do While .T.
			@ 12,18 Get MOpt Func '*H \<Cancel;\<Modify;\<Delete' Size 1,10 Color Scheme 19
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
		@ 12,01 To 12,62 Clear
		If Optflag='C'
			MC1=0								&& First Ledger Code
			MC2=0								&& Second Ledger Code
			MC3=0								&& Thired Ledger Code
			Loop
		Endif
		If Optflag='M'
			Sele Ldgrcod
			MTitle=Title
			MC_Code=C_Code
		Endif
		If Optflag='D'
			Sele Tran
			Set Order To C123
			Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
			If Found()
				Wait Wind 'This Code Already Exist In Daily Transaction'
			Else
				Sele Ldgrcod
				Dele
			Endif
			MC1=0								&& First Ledger Code
			MC2=0								&& Second Ledger Code
			MC3=0								&& Thired Ledger Code
			Loop
		Endif
	Endif
	@ 5,30 Get MTitle Pict"@!" Valid MTitle#Spac(30) Error 'Ledger Head Title Can Not Be Blank'
	Read
	@ 12,01 To 12,62 Clear
	@ 12,18 SAY "Press F1 to see City Code List" Color GR+
	ON KEY LABEL F1 DO ShwCtyPro In Proc1
	@ 8,30 Get MC_Code Pict"@!" Valid MC_Code#Spac(4) Error 'City Code Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop
	Endif
	Do SavDatPro
	MC1=0								&& First Ledger Code
	MC2=0								&& Second Ledger Code
	MC3=0								&& Thired Ledger Code
	MTitle=Spac(30)						&& Title Of Ledger Code
	MC_Code=Spac(4)						&& City Code
EndDo
Rele Wind Ldgrcod
Return
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 02,05 Say "Ledger Head Code"
@ 05,05 Say "Title of Ledger Head"
@ 08,05 Say "City Code"
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele Ldgrcod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If .Not. Found()
		Append Blank
	Endif
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Title With MTitle
	Replace C_Code With MC_Code
	Flush

