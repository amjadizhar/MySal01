*-----------------------------------------------*
*Program to Enter/Modifi/Delete The Item Code	*
*Author		:	Muhammad Usman					*
*Date		:	21-01-2000						*
*Prog		:	Item							*
*Last Modify:	21-01-2000						*
*-----------------------------------------------*
* Use Files
Close Databases

Sele 1
Use Item Alia Item Shared

Sele 2
Use ITran Alia ITran Shared

*-----------------------------------------------
*Veriable Initialization
MI1=0							&& First Item Code
MI2=0							&& Second Item Code
MI3=0							&& Third Item Code
MItem=Spac(30)					&& Item Description
MRate=0							&& Item Rate
MSTax=0							&& Sales Tax %
MOpt=0							&& Option
*-----------------------------------------------
* Window Defination
	DEFINE WINDOW item ;
		FROM INT((SROW()-16)/2),INT((SCOL()-69)/2) ;
		TO INT((SROW()-16)/2)+15,INT((SCOL()-69)/2)+68 ;
		TITLE " ITEM HEAD MAINTENANCE " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW item SAME
*-----------------------------------
Do While .T.
	Clear
	Do DspHedPro
	@ 13,18 SAY "Press F1 to see Items Code List" Color GR+
	ON KEY LABEL F1 DO ShwItmPro In Proc1
	@ 2,30 Say "  -  -    "
	@ 2,30 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	@ 2,33 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	@ 2,36 Get MI3 Pict"@4K 9999" Valid MI3#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind Item
		Return
		Exit
	Endif
	Sele Item
	Set Order To I123
	Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
	If Opt="New"
		If Found()	
			Wait Wind 'Item Code Already Exist'
			MI1=0								&& First Item Code
			MI2=0								&& Second Item Code
			MI3=0								&& Third Item Code
			Loop
		Endif
	Endif
	If Opt="Modi"
		If .Not. Found()	
			Wait Wind 'Item Code Not Exist'
			MI1=0								&& First Item Code
			MI2=0								&& Second Item Code
			MI3=0								&& Third Item Code
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
			MI1=0								&& First Item Code
			MI2=0								&& Second Item Code
			MI3=0								&& Third Item Code
			Loop
		Endif
		If Optflag='M'
			Sele Item
			MItem=Item
			MRate=Rate
		Endif
		If Optflag='D'
			Sele ITran
			Set Order To I123
			Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
			If Found()
				Wait Wind 'This Code Already Exist In Daily Transaction'
			Else
				Sele Item
				Dele
			Endif
			MI1=0								&& First Item Code
			MI2=0								&& Second Item Code
			MI3=0								&& Third Item Code
			Loop
		Endif
	Endif
	@ 4,30 Get MItem Pict"@!" Valid MItem#Spac(30) Error 'Item Name Can Not Be Blank'
	@ 6,30 Get MRate Pict"9999.99"
	@ 8,30 Get MSTax Pict"99.99"
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop		
	Endif
	Do SavDatPro
	MI1=0							&& First Item Code
	MI2=0							&& Second Item Code
	MI3=0							&& Third Item Code
	MItem=Spac(30)					&& Item Description
	MRate=0							&& Item Rate
EndDo
Rele Wind Item
Return
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 02,05 Say "Item Code"
@ 04,05 Say "Item Description"
@ 06,05 Say "Rate"
@ 08,05 Say "Sales Tax %"
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele Item
	Set Order To I123
	Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
	If .Not. Found()
		Append Blank
	Endif
	Replace I1 With MI1
	Replace I2 With MI2
	Replace I3 With MI3
	Replace Item With MItem
	Replace Rate With MRate
	Replace STax With MSTax
	Flush
