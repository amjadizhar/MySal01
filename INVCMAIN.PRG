*-----------------------------------*
*Program Main Menu Popup			*
*Author		:	Amjad Izhar 		*
*Date		:	20-01-2000			*
*Prog		:	InvcMain     		*
*Last Modify:	15-05-2005			*
*-----------------------------------*
Public Backgrnd
CLEAR

CLOSE ALL

SET COLOR TO

SET DATE BRIT
SET CENTURY ON
SET SCORE OFF
SET BELL OFF
SET SAFETY OFF
SET TALK OFF
SET CONFIRM OFF
SET STATUS OFF
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
SET EXCLUSIVE OFF
SET SYSMENU OFF
SET DELE ON
CLEAR MACROS

MComp="ARSLAN (PVT) LTD"

Do Backgrnd
Do C-File

On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
On Key Label Escape ?? CHR(7)

IF _DOS
SAVE SCREEN TO Backgrnd
ENDIF

DEFINE POPUP mainmenu FROM 1,0 TO 15,29 COLOR SCHEME 1				&& Main Menu
DEFINE BAR 1 OF mainmenu PROMPT "께께께�  Main Menu   께께께�" SKIP
DEFINE BAR 2 OF mainmenu PROMPT "\-"
DEFINE BAR 3 OF mainmenu PROMPT " \<Daily Transactions";
	Message "Entry All Types Of Invoices" 
DEFINE BAR 4 OF mainmenu PROMPT "\-"
DEFINE BAR 5 OF mainmenu PROMPT " \<Code Files";
	Message "Maintain The Party & Item Codes "
DEFINE BAR 6 OF mainmenu PROMPT "\-"
DEFINE BAR 7 OF mainmenu PROMPT " \<Reports";
	Message "Display Or Print All Types Of Report"
DEFINE BAR 8 OF mainmenu PROMPT "\-"
DEFINE BAR 9 OF mainmenu PROMPT " System \<Maintenance";
	Message "Progrmme Files Arrangement"
DEFINE BAR 10 OF mainmenu PROMPT "\-"
DEFINE BAR 11 OF mainmenu PROMPT " \<System Security";
	Message "Arranging Security Passwards"
DEFINE BAR 12 OF mainmenu PROMPT "\-"
DEFINE BAR 13 OF mainmenu PROMPT " \<Quit";
	Message "Exit From This Programe"
ON SELECTION POPUP mainmenu DO mmchoice

DEFINE POPUP coMenu FROM 3,25 TO 15,51 COLOR SCHEME 10			&& Code Files Menu
DEFINE BAR 1 OF coMenu PROMPT "같같같 Code  Files 같같같" SKIP
DEFINE BAR 2 OF coMenu PROMPT "\-"
DEFINE BAR 3 OF coMenu PROMPT " \<Party Code File";
	Message "Party Code (Entry, Modification & Printing/Display)"
DEFINE BAR 4 OF coMenu PROMPT "\-"
DEFINE BAR 5 OF coMenu PROMPT " \<Item Code File";
	Message "Item Code (Entry, Modification & Printing/Display)"
DEFINE BAR 6 OF coMenu PROMPT "\-"
DEFINE BAR 7 OF coMenu PROMPT " \<Document Type File";
	Message "Document Type (Entry, Modification & Printing/Display)"
DEFINE BAR 8 OF coMenu PROMPT "\-"
DEFINE BAR 9 OF coMenu PROMPT " \<City Code File";
	Message "City Code (Entry, Modification & Printing/Display)"
DEFINE BAR 10 OF coMenu PROMPT "\-"
DEFINE BAR 11 OF coMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP coMenu DO coMenu

DEFINE POPUP cpMenu FROM 6,47 TO 16,73 COLOR SCHEME 10			&& Party Code Files Menu
DEFINE BAR 1 OF cpMenu PROMPT "같같같 Party Files 같같같" SKIP
DEFINE BAR 2 OF cpMenu PROMPT "\-"
DEFINE BAR 3 OF cpMenu PROMPT " \<New Entry";
	Message "Only New Party Code Entry"
DEFINE BAR 4 OF cpMenu PROMPT "\-"
DEFINE BAR 5 OF cpMenu PROMPT " \<Modification";
	Message "Party Code Correction, Delection"
DEFINE BAR 6 OF cpMenu PROMPT "\-"
DEFINE BAR 7 OF cpMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF cpMenu PROMPT "\-"
DEFINE BAR 9 OF cpMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP cpMenu DO cpMenu

DEFINE POPUP ciMenu FROM 8,47 TO 18,73 COLOR SCHEME 10			&& Item Code Files Menu
DEFINE BAR 1 OF ciMenu PROMPT "같같같 Item  Files 같같같" SKIP
DEFINE BAR 2 OF ciMenu PROMPT "\-"
DEFINE BAR 3 OF ciMenu PROMPT " \<New Entry";
	Message "Only New Item Code Entry"
DEFINE BAR 4 OF ciMenu PROMPT "\-"
DEFINE BAR 5 OF ciMenu PROMPT " \<Modification";
	Message "Item Code Correction, Delection"
DEFINE BAR 6 OF ciMenu PROMPT "\-"
DEFINE BAR 7 OF ciMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF ciMenu PROMPT "\-"
DEFINE BAR 9 OF ciMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP ciMenu DO ciMenu

DEFINE POPUP cdMenu FROM 10,47 TO 20,73 COLOR SCHEME 10			&& Document Type Files Menu
DEFINE BAR 1 OF cdMenu PROMPT "같 Document Type Files 같" SKIP
DEFINE BAR 2 OF cdMenu PROMPT "\-"
DEFINE BAR 3 OF cdMenu PROMPT " \<New Entry";
	Message "Only New Document Type Entry"
DEFINE BAR 4 OF cdMenu PROMPT "\-"
DEFINE BAR 5 OF cdMenu PROMPT " \<Modification";
	Message "Document Type Correction, Delection"
DEFINE BAR 6 OF cdMenu PROMPT "\-"
DEFINE BAR 7 OF cdMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF cdMenu PROMPT "\-"
DEFINE BAR 9 OF cdMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP cdMenu DO cdMenu

DEFINE POPUP ccMenu FROM 12,47 TO 22,73 COLOR SCHEME 10			&& City Code Files Menu
DEFINE BAR 1 OF ccMenu PROMPT "같 City Code Files 같" SKIP
DEFINE BAR 2 OF ccMenu PROMPT "\-"
DEFINE BAR 3 OF ccMenu PROMPT " \<New Entry";
	Message "Only New City Code Entry"
DEFINE BAR 4 OF ccMenu PROMPT "\-"
DEFINE BAR 5 OF ccMenu PROMPT " \<Modification";
	Message "City Code Correction, Delection"
DEFINE BAR 6 OF ccMenu PROMPT "\-"
DEFINE BAR 7 OF ccMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF ccMenu PROMPT "\-"
DEFINE BAR 9 OF ccMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP ccMenu DO ccMenu

DEFINE POPUP reMenu FROM 3,25 TO 23,51 COLOR SCHEME 10			&& Reports Menu
DEFINE BAR 1 OF reMenu PROMPT "같같     Reports     같같" SKIP
DEFINE BAR 2 OF reMenu PROMPT "\-"
DEFINE BAR 3 OF reMenu PROMPT " \<Leadger Status";
	Message "All Types Of Leadger Status Reports"
DEFINE BAR 4 OF reMenu PROMPT "\-"
DEFINE BAR 5 OF reMenu PROMPT " \<Account Leadger";
	Message "All Types Of Account Leadger Reports"
DEFINE BAR 6 OF reMenu PROMPT "\-"
DEFINE BAR 7 OF reMenu PROMPT " \<Zone Wise Sale";
	Message "Zone Wise Cartons & Amount Reports"
DEFINE BAR 8 OF reMenu PROMPT "\-"
DEFINE BAR 9 OF reMenu PROMPT " ";
	Message "Reports"
DEFINE BAR 10 OF reMenu PROMPT "\-"
DEFINE BAR 11 OF reMenu PROMPT " ";
	Message "Report"
DEFINE BAR 12 OF reMenu PROMPT "\-"
DEFINE BAR 13 OF reMenu PROMPT " ";
	Message "Report"
DEFINE BAR 14 OF reMenu PROMPT "\-"
DEFINE BAR 15 OF reMenu PROMPT " ";
	Message ""
DEFINE BAR 16 OF reMenu PROMPT "\-"
DEFINE BAR 17 OF reMenu PROMPT " ";
	Message ""
DEFINE BAR 18 OF reMenu PROMPT "\-"
DEFINE BAR 19 OF reMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP reMenu DO reMenu

DEFINE POPUP dtMenu FROM 7,25 TO 15,51 COLOR SCHEME 10			&& Daily Transaction Menu
DEFINE BAR 1 OF dtMenu PROMPT "같� Daily Transaction 같�" SKIP
DEFINE BAR 2 OF dtMenu PROMPT "\-"
DEFINE BAR 3 OF dtMenu PROMPT " \<All Vouchers";
	Message "All Vouchers Types (Entry, Modification & Printing/Display)"
DEFINE BAR 4 OF dtMenu PROMPT "\-"
DEFINE BAR 5 OF dtMenu PROMPT " \<Sales Invoices";
	Message "Sales Invoices (Entry, Modification & Printing/Display)"
DEFINE BAR 6 OF dtMenu PROMPT "\-"
DEFINE BAR 7 OF dtMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP dtMenu DO dtMenu

DEFINE POPUP vMenu FROM 09,47 TO 19,73 COLOR SCHEME 10			&& Vouchers Menu
DEFINE BAR 1 OF vMenu PROMPT "같같같   Vouchers  같같같" SKIP
DEFINE BAR 2 OF vMenu PROMPT "\-"
DEFINE BAR 3 OF vMenu PROMPT " \<New Entry";
	Message "Only New Voucher Entry"
DEFINE BAR 4 OF vMenu PROMPT "\-"
DEFINE BAR 5 OF vMenu PROMPT " \<Modification";
	Message "Voucher Correction, Delection"
DEFINE BAR 6 OF vMenu PROMPT "\-"
DEFINE BAR 7 OF vMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF vMenu PROMPT "\-"
DEFINE BAR 9 OF vMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP vMenu DO vMenu

DEFINE POPUP vrMenu FROM 15,53 TO 23,80 COLOR SCHEME 10			&& Vouchers Printing/Display Menu
DEFINE BAR 1 OF vrMenu PROMPT "같같같 Report Menu 같같같" SKIP
DEFINE BAR 2 OF vrMenu PROMPT "\-"
DEFINE BAR 3 OF vrMenu PROMPT " \<Document Type Wise";
	Message "Report Document Type Wise"
DEFINE BAR 4 OF vrMenu PROMPT "\-"
DEFINE BAR 5 OF vrMenu PROMPT " \<Date Wise";
	Message "Report Voucher Date Wise"
DEFINE BAR 6 OF vrMenu PROMPT "\-"
DEFINE BAR 7 OF vrMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP vrMenu DO vrMenu

DEFINE POPUP sMenu FROM 11,47 TO 21,73 COLOR SCHEME 10			&& Sale Invoice Menu
DEFINE BAR 1 OF sMenu PROMPT "같같� Sale  Invoice 같같�" SKIP
DEFINE BAR 2 OF sMenu PROMPT "\-"
DEFINE BAR 3 OF sMenu PROMPT " \<New Entry";
	Message "Only New Sale Invoice Entry"
DEFINE BAR 4 OF sMenu PROMPT "\-"
DEFINE BAR 5 OF sMenu PROMPT " \<Modification";
	Message "Sale Invoice Correction, Delection"
DEFINE BAR 6 OF sMenu PROMPT "\-"
DEFINE BAR 7 OF sMenu PROMPT " \<Printing/Display";
	Message "Report Print & Screen"
DEFINE BAR 8 OF sMenu PROMPT "\-"
DEFINE BAR 9 OF sMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP sMenu DO sMenu

DEFINE POPUP srMenu FROM 15,53 TO 23,80 COLOR SCHEME 10			&& Sale Invoice Printing/Display Menu
DEFINE BAR 1 OF srMenu PROMPT "같같같 Report Menu 같같같" SKIP
DEFINE BAR 2 OF srMenu PROMPT "\-"
DEFINE BAR 3 OF srMenu PROMPT " \<Number Wise";
	Message "Report Sale Invoice Number Wise"
DEFINE BAR 4 OF srMenu PROMPT "\-"
DEFINE BAR 5 OF srMenu PROMPT " \<Date Wise";
	Message "Report Sale Invoice Date Wise"
DEFINE BAR 6 OF srMenu PROMPT "\-"
DEFINE BAR 7 OF srMenu PROMPT " \<Exit";
	Message "Return To Previous Menu"
ON SELECTION POPUP srMenu DO srMenu

DEFINE POPUP manMenu FROM 11,25 TO 17,51 COLOR SCHEME 10
DEFINE BAR 1 OF manMenu PROMPT "같같같 Maintenance 같같같" SKIP
DEFINE BAR 2 OF manMenu PROMPT "\-"
DEFINE BAR 3 OF manMenu PROMPT " \<Re-Index Data Files";
	Message "Arranging All The Records"
DEFINE BAR 4 OF manMenu PROMPT "\-"
DEFINE BAR 5 OF manMenu PROMPT " \<Exit";
	Message "Exit From This Menu"
ON SELECTION POPUP manMenu DO manMenu

DEFINE POPUP secMenu FROM 13,25 TO 21,51 COLOR SCHEME 10
DEFINE BAR 1 OF secMenu PROMPT "같같 System Security 같같" SKIP
DEFINE BAR 2 OF secMenu PROMPT "\-"
DEFINE BAR 3 OF secMenu PROMPT " \<Change Passwords";
	Message "Changeing All The Menu Passwords"
DEFINE BAR 4 OF secMenu PROMPT "\-"
DEFINE BAR 5 OF secMenu PROMPT " \<Update Balances";
	Message "Update All Leadger Balances"
DEFINE BAR 6 OF secMenu PROMPT "\-"
DEFINE BAR 7 OF secMenu PROMPT " \<Exit";
	Message "Exit From This Menu"
ON SELECTION POPUP secMenu DO secMenu

ACTIVATE POPUP mainmenu
*------------------------------------
PROCEDURE mmchoice
*------------------------------------
DO CASE

	CASE BAR() = 3
		MSrNo=3
		Do Pass
		If MPermit = "OK"
			ACTIVATE POPUP dtMenu
		Endif

	CASE BAR() = 5
		MSrNo=1
		Do Pass
		If MPermit = "OK"
			ACTIVATE POPUP coMenu
		Endif

	CASE BAR() = 7
		MSrNo=2
		Do Pass
		If MPermit = "OK"
			ACTIVATE POPUP reMenu
		Endif

	CASE BAR() = 9
		MSrNo=4
		Do Pass
		If MPermit = "OK"
			ACTIVATE POPUP manMenu
		Endif

	CASE BAR() = 11
		MSrNo=5
		Do Pass
		If MPermit = "OK"
			ACTIVATE POPUP secMenu
		Endif

	CASE BAR() = 13
		On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		CLOSE ALL
		CLEAR
		*CLEAR ALL
		RETURN TO MASTER
ENDCASE

RETURN
*------------------------------------
PROCEDURE coMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
		ACTIVATE POPUP cpMenu
	
	CASE BAR() = 5
		ACTIVATE POPUP ciMenu

	CASE BAR() = 7
		ACTIVATE POPUP cdMenu

	CASE BAR() = 9
		ACTIVATE POPUP ccMenu

	CASE BAR() = 11
		DEACTIVATE POPUP coMenu

	OTHERWISE 

ENDCASE

RETURN
*------------------------------------
PROCEDURE cpMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="New"
		Do Ldgrcod

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="Modi"
		Do Ldgrcod

	CASE BAR() = 7
		Do R-Ldcod
		
	CASE BAR() = 9
		DEACTIVATE POPUP cpMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE ciMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="New"
		Do Item

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="Modi"
		Do Item

	CASE BAR() = 7
		Do R-Itmcod
		
	CASE BAR() = 9
		DEACTIVATE POPUP ciMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE cdMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="New"
		Do Dtype

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="Modi"
		Do Dtype

	CASE BAR() = 7
		
	CASE BAR() = 9
		DEACTIVATE POPUP cdMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE ccMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="New"
		Do City

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Opt="Modi"
		Do City

	CASE BAR() = 7
		Do R-City
		
	CASE BAR() = 9
		DEACTIVATE POPUP ccMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE reMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do R-Lstat


	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do R-Led

	CASE BAR() = 7
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do R-Zone


	CASE BAR() = 9
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 

	CASE BAR() = 11
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 

	CASE BAR() = 13
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 


	CASE BAR() = 15
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 


	CASE BAR() = 17
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 

	CASE BAR() = 19
		DEACTIVATE POPUP reMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE dtMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
		ACTIVATE POPUP vMenu
	
	CASE BAR() = 5
		ACTIVATE POPUP sMenu
		
	CASE BAR() = 7
		DEACTIVATE POPUP dtMenu

	OTHERWISE 

ENDCASE

RETURN
*------------------------------------
PROCEDURE vMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do Tran

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do M-Tran

	CASE BAR() = 7
		ACTIVATE POPUP vrMenu

	CASE BAR() = 9
		DEACTIVATE POPUP vMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE vrMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do RN-Tran

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do RD-Tran

	CASE BAR() = 7
		DEACTIVATE POPUP vrMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE sMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do ITran

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do M-ITran

	CASE BAR() = 7
		ACTIVATE POPUP srMenu

	CASE BAR() = 9
		DEACTIVATE POPUP sMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE srMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do RN-Invo

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do RD-Invo

	CASE BAR() = 7
		DEACTIVATE POPUP srMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE manMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do Reindex

	CASE BAR() = 5
		DEACTIVATE POPUP manMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE secMenu
*------------------------------------
DO CASE

	CASE BAR() = 3
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do C-Pass

	CASE BAR() = 5
        On Key Label Escape
        On Key Label Rightarrow 
        On Key Label Leftarrow 
		Do Reindex
		Do Updbal

	CASE BAR() = 7
		DEACTIVATE POPUP secMenu

	OTHERWISE 

ENDCASE

On Key Label Escape ?? CHR(7)
On Key Label Rightarrow ?? CHR(7)
On Key Label Leftarrow ?? CHR(7)
RETURN
*------------------------------------
PROCEDURE BackGrnd
*------------------------------------
@ 0,0 SAY MComp COLOR SCHEME 1
@ 0,61 SAY "SALE PROGRAMME" COLOR SCHEME 1
@ 22,0 SAY "Designed & Developed by :" COLOR SCHEME 1
@ 23,0 SAY "MUHAMMAD USMAN" COLOR SCHEME 1
RETURN
