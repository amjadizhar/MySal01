*-----------------------------------------------*
*Program to Enter/Modifi/Delete The Data Type	*
*Author		:	Muhammad Usman					*
*Date		:	27-01-2000						*
*Prog		:	DType							*
*Last Modify:	27-01-2000						*
*-----------------------------------------------*
* Use Files
Close Databases

Sele 1
Use DType Alia DType Shared

Sele 2
Use Tran Alia Tran Shared

*-----------------------------------------------
*Veriable Initialization
MDoc_Type=0							&& Data Type Code
MType1=Spac(2)						&& Data Type Abbreviation
MType2=Spac(15)						&& Data Type Description
MOpt=0								&& Option
*-----------------------------------------------
* Window Defination
	DEFINE WINDOW DType ;
		FROM INT((SROW()-16)/2),INT((SCOL()-69)/2) ;
		TO INT((SROW()-16)/2)+15,INT((SCOL()-69)/2)+68 ;
		TITLE " DATA TYPE MAINTENANCE " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW DType SAME
*-----------------------------------
Do While .T.
	Clear
	Do DspHedPro
	@ 12,18 SAY "Press F1 to see Data Type List" Color GR+
	ON KEY LABEL F1 DO ShwDtyPro In Proc1
	@ 2,30 Get MDoc_Type Pict"@1K 9"
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind DType
		Return
		Exit
	Endif
	Sele DType
	Set Order To Doc_Type
	Seek MDoc_Type
	If Opt="New"
		If Found()	
			Wait Wind 'Data Type Code Already Exist'
			MDoc_Type=0							&& Data Type Code
			Loop
		Endif
	Endif
	If Opt="Modi"
		If .Not. Found()	
			Wait Wind 'Data Type Code Not Exist'
			MDoc_Type=0							&& Data Type Code
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
			MDoc_Type=0							&& Data Type Code
			Loop
		Endif
		If Optflag='M'
			Sele DType
			MType1=Type1
			MType2=Type2
		Endif
		If Optflag='D'
			Sele Tran
			Set Order To Doc_Type
			Seek MDoc_Type
			If Found()
				Wait Wind 'This Code Already Exist In Daily Transaction'
			Else
				Sele DType
				Dele
			Endif
			MDoc_Type=0							&& Data Type Code
			Loop
		Endif
	Endif
	@ 5,30 Get MType1 Pict"@!" Valid MType1#Spac(2) Error 'Abbreviation Type Can Not Be Blank'
	@ 7,30 Get MType2 Pict"@!" Valid MType2#Spac(15) Error 'Discription Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop
	Endif
	Do SavDatPro
	MDoc_Type=0							&& Data Type Code
	MType1=Spac(2)						&& Data Type Abbreviation
	MType2=Spac(15)						&& Data Type Description
EndDo
Rele Wind DType
Return
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 02,05 Say "Data Type Code"
@ 05,05 Say "Abbreviation Type"
@ 07,05 Say "Discription"
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele DType
	Set Order To Doc_Type
	Seek MDoc_Type
	If .Not. Found()
		Append Blank
	Endif
	Replace Doc_Type With MDoc_Type
	Replace Type1 With MType1
	Replace Type2 With MType2
	Flush
