*---------------------------------------*
*Program to Enter The Item Transaction	*
*Author		:	Muhammad Usman			*
*Date		:	19-04-2000				*
*Prog		:	ITran					*
*Last Modify:	19-04-2000				*
*---------------------------------------*
* Use Files

Sele 1
Use ITran Alia ITran Shared

Sele 2
Use Item Alia Item Shared

Sele 3
Use Tran Alia Tran Shared

Sele 4
Use LdgrCod Alia LdgrCod Shared

Sele 5
Use DType Alia DType Shared

*-----------------------------------
* Window Defination
	DEFINE WINDOW ITran ;
		FROM INT((SROW()-25)/1),INT((SCOL()-80)/1) ;
		TO INT((SROW()-25)/1)+24, INT((SCOL()-80)/1)+79 ;
		TITLE " DAILY TRANSACTION " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW ITran SAME
*-----------------------------------
*Veriable Initialization
MDate=Ctod("  /  /    ")			&& Invoice Date
MC1=0								&& Party Code 1
MC2=0								&& Party Code 2
MC3=0								&& Party Code 3
MSAmount=0							&& Sales Tax Amount
MNAmount=0							&& Net Total Amount
MAmount=0							&& Total Amount
MDoc_No=Spac(6)						&& Document Number
MDoc_Type=0							&& Document Type
MCR_DR=Spac(1)						&& Credit / Debit
MLdgr_Bal=0							&& Leadger Balance
MStatus=Spac(2)						&& Status of Leadger Balance
MI1=0								&& Item Code 1
MI2=0								&& Item Code 2
MI3=0								&& Item Code 3
MQty=0								&& Item Quantity
MTQty=0								&& Total Quantity
MTAmount=0							&& Total Amount
MTNAmount=0							&& Total Net Amount
MSr_No=1							&& Serial Number
MLine=1								&& Line Number
MAns=Spac(1)						&& Answer
*-----------------------------------
Do While .T.
	Do DspHedPro
	@ MLine,10 GET MDate Pict("99/99/9999")
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind ITran
		Return
		Exit
	Endif
	@ 22,01 To 22,78 Clear
	@ 22,22 SAY "Press F1 to see Document Type Code List" Color GR+
	ON KEY LABEL F1 DO ShwDtyPro In Proc1
	@ MLine,57 Get MDoc_Type Pict"9"
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop
	Endif
	Sele DType
	Set Order To Doc_Type
	Seek MDoc_Type
	If .Not. Found()
		Wait Wind 'Data Type Not Exist'
		MDoc_Type=0							&& Document Type
		Loop
	Endif				
	@ MLine,60 Get MDoc_No Pict"@!" Valid MDoc_No#Spac(6) Error 'Document Number Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Loop
	Endif
	Sele Tran
	Set Order To Type_No
	Seek Str(MDoc_Type,1)+MDoc_No
	If Found()
		Wait Wind 'Document Number Already Exist'
		MDoc_Type=0							&& Document Type
		MDoc_No=Spac(6)						&& Document Number
		Loop
	Endif				
	MLine=MLine+1
	Do While .T.
		Sele LdgrCod
		Set Order To Title
		@ 22,01 To 22,78 Clear
		@ 22,27 SAY "Press F1 to see Party Code List" Color GR+
		ON KEY LABEL F1 DO ShwLdgPro In Proc1
		@ MLine,20 Say "  -  -   "
		@ MLine,20 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
		@ MLine,23 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank'
		@ MLine,26 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank'
		Read
		If Read()=12 .Or. Read()=268				&& Escape Key
			Exit
		Endif
		Set Order To C123
		Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
		If .Not. Found()
			Wait Wind 'Party Code Not Exist'
			MC1=0								&& Party Code 1
			MC2=0								&& Party Code 2
			MC3=0								&& Party Code 3
			Loop
		Else
			@ MLine,35 Say Title
			Exit
		Endif
	Enddo
	MLine=MLine+4
	Do While .T.
		Sele Item
		Set Order To Item
		@ 22,01 To 22,62 Clear
		@ 22,26 SAY "Press F1 to see Item Code List" Color GR+
		ON KEY LABEL F1 DO ShwItmPro In Proc1
		@ MLine,02 Say "  -  -    "
		@ MLine,02 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank'
		@ MLine,05 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank'
		@ MLine,08 Get MI3 Pict"@4K 9999" Valid MI3#0 Error 'Item Code Can Not Be Blank'
		Read
		If Read()=12 .Or. Read()=268				&& Escape Key
			Exit
		Endif
		Set Order To I123
		Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
		If .Not. Found()
			Wait Wind 'Item Code Not Exist'
			MI1=0								&& Item Code 1
			MI2=0								&& Item Code 2
			MI3=0								&& Item Code 3
			Loop
		Else
			@ MLine,15 Say Item
		Endif
		@ MLine,48 Get MQty Pict"@4K 9999"
		Read
		MSAmount=MQty*Item.Rate
		MNAmount=MQty*Item.Rate
		MAmount=MSAmount+Round((MSAmount*Item.STax/100),2)
		@ MLine,53 Say MNAmount Pict"999999999.99"
		@ MLine,53 Get MNAmount Pict"999999999.99"
		@ MLine,66 Say MAmount Pict"999999999.99"
		@ MLine,66 Get MAmount Pict"999999999.99"
		Read
		MAns="N"
		@ MLine,00 Get MAns Pict"@!" Valid MAns="N" .Or. MAns="C" Error 'N=Next  C=Change'
		Read
		If MAns="C"
			Loop
		Else
			Do SavDatPro
			MTQty=MTQty+MQty
			MTNAmount=MTNAmount+Round(MNAmount,2)
			MTAmount=MTAmount+Round(MAmount,2)
			MI1=0								&& Item Code 1
			MI2=0								&& Item Code 2
			MI3=0								&& Item Code 3
			MQty=0								&& Item Quantity
			MAmount=0							&& Total Amount
			MNAmount=0							&& Net Amount
			MSr_No=MSr_No+1						&& Serial Number
			Sele ITran
		Endif
		MLine=MLine+1
		If MLine>19
			@ 6,0 TO 21,78 Clear
			MLine=6
		Endif
	Enddo
	@ 22,01 To 22,62 Clear
	@ 22,30 Say "Total"
	@ 22,48 Say MTQty Pict"9999"
	@ 22,53 Say MTNAmount Pict"999999999.99"
	@ 22,66 Say MTAmount Pict"999999999.99"
	Wait Wind 'Press Enter To Continue'
	Clear 
	Do Sav1DatPro
	MDate=Ctod("  /  /    ")			&& Invoice Date
	MC1=0								&& Party Code 1
	MC2=0								&& Party Code 2
	MC3=0								&& Party Code 3
	MSAmount=0							&& Sales Tax Amount
	MAmount=0							&& Total Amount
	MNAmount=0							&& Net Amount
	MDoc_No=Spac(6)						&& Document Number
	MDoc_Type=0							&& Document Type
	MCR_DR=Spac(1)						&& Credit / Debit
	MLdgr_Bal=0							&& Leadger Balance
	MStatus=Spac(2)						&& Status of Leadger Balance
	MI1=0								&& Item Code 1
	MI2=0								&& Item Code 2
	MI3=0								&& Item Code 3
	MQty=0								&& Item Quantity
	MTQty=0								&& Total Quantity
	MTAmount=0							&& Total Amount
	MTNAmount=0							&& Total Net Amount
	MSr_No=1							&& Serial Number
	MLine=1								&& Line Number
	MAns=Spac(1)						&& Answer
	Loop
Enddo
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 1,02 SAY "DATE"
@ 1,50 SAY "DOC #"
@ 2,02 SAY "PARTY CODE"
@ 3,00 SAY Repl("-",78)
@ 4,02 SAY "ITEM CODE"
@ 4,48 SAY "QTY"
@ 4,53 SAY "NET"
@ 4,66 SAY "AMOUNT"
@ 5,00 SAY Repl("-",78)
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele ITran
	Append Blank
	Replace Date With MDate
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Amount With MAmount
	Replace Doc_Type With MDoc_Type
	Replace Doc_No With MDoc_No
	Replace I1 With MI1
	Replace I2 With MI2
	Replace I3 With MI3
	Replace Qty With MQty
	Replace Sr_No With MSr_No	
	Replace NAmount With MNAmount
	Flush
*-----------------------------------
Procedure Sav1DatPro						&& Data 1 Save Procedure
*-----------------------------------
Sele Tran
	Append Blank
	Replace Date With MDate
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Amount With MTAmount
	Replace Doc_Type With MDoc_Type
	Replace Doc_No With MDoc_No
	Replace Cr_Dr With "DR"
	Replace Disc With "SALE INVOICE"
Sele LdgrCod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If Status="  "
		MLdgr_Bal=MTAmount
		MStatus="DR"
	Endif
	If Status="DR"
		MLdgr_Bal=Ldgr_Bal+MTAmount
		MStatus="DR"
	Endif
	If Status="CR"
		MLdgr_Bal=Ldgr_Bal-MTAmount
		If MLdgr_Bal>0
			MStatus="CR"
		Else
			MStatus="DR"
		Endif				
	Endif
Replace Ldgr_Bal With ABS(MLdgr_Bal)
Replace Status With MStatus