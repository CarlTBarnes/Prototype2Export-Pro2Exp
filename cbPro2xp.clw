! cbPro2Exp Copyright (c) 2021 Carl T. Barnes released under the MIT License in download file named LICENSE
! Paste in Prototypes and Pro2Exp will make your EXP lines. Very handy for a CLASS. 

  PROGRAM
!Region History Comments  
!June 2022
!   Filter out ! Comments. Some were ending up in EXP as !@ Exports. 06/08/22
!   OmitComments - Default to True because they are confusing and then you have to find the CHECK
!   OrdinalColumn - Default change 60 to 0 as align pos of @? because 60 makes EXP larger
!   Expand 30k to 60k of TEXT and warn on Paste button if Clipboard has > 60k 
!Feb 2021  
!   [28] TYPE QUEUE, GROUP are Not Exported?? '  ; QUEUE data  not exported' '  ; GROUP data not exported'  
!   [28] Change' ; comments ' to ';;' which are omitted if OmitComments checked
!           so can have '; comments' that ARE left
!   [ ] If ';; ' appear Message('FYI "; comments" can be removed checking OmitComments
!   [x] Change to CONSOLAS font?

!   10/21/17 Move to C10 
!   [x]    Update with new Data Types  BSTRING ASTRING ANY * *?  ?
!   [ ] Header should have different? "NAME dllname GUI"     LIBRARY 'ExportTest' GUI
!   [22] CONST is seen as Named type, need to remove (CONST ,CONST    PassAstring2(CONST *Astring B) ,EXPORT 
!   [x] Revise Window and have TEXT with Mangle Encoding
!   [no ] Could make a simple single Type encoder   Pick type from Drop list, CHECK for <Omittable>, CHECK for *ADRESS 
!   [x] INDEX is Bk  EntityPass  PROCEDURE(KEY K, INDEX I, VIEW V, WINDOW W, <*REPORT R>)     
!   [x] Remove spaces after < and before > so works < STRING S >
!   [x] Remove spaces after * so "* STRING" works
!   [x] "ReRun" Spin up another COPY button of EXE
!   [x] add MANGLE Button, don't do it on Accept TEXT
!   [x] CHECK to Insert Procedure Prototypes as Comments in EXP file  ;StringPass  PROCEDURE(STRING S1, *STRING S2, <STRING S3>, <*STRING S4>) 
!   [ ]
!   [x] FILE ? 
!   [ ] can be smarter --. CLASS line contains IMPLEMENTS then a message tells you to paste in the Interface
!   [x] DATA is NOT UPPER it is Original Case in C5, but C10 appears upper so go that way

!------------ old
!This is the CW\EXAMPLES\SRC\PRO2EXP program with some user interface enhancements.
!Inspired by an earlier version of Mangle.exe written by Lee G. White.
!Written by Jeff Slarve Feb-17-2005
!Enhancements include: 1. Mangling of multiple prototypes at once.
!                      2. Saving/Restoring of user preferences
!                      3. Support of multi-line prototypes (by use of the "FlattenPrototypes" function.
!                      4. Level of indent setting
!                      5. Column of ordinal setting, where the @? ends up
!                      6. Use of TEXT controls instead of STRING controls so clipboard maybe used.
! Feb-19-2005 version .091
!                      7. Use of courier instead of FixedSys (Lee White)
!                      8. RemoveProcFunc Routine (Lee White)
!                      9. Increased string size (Lee White)
!
! Please report any bugs or enhancements to carl@carlbarnes.com 
!
!Modified by Carl Barnes 12/21/2006
!               deal with old style vs new style procedure declarations better so classes could be all new style
!               got rid of Lee's RemoveProcFunc and did it my way, it did not handle proc without ()
!               changed font to FixedSys ... what was Lee thinking? Give me bold fonts!
!               deal with procedures without (paren) by adding the () e.g. Kill  PROCEDURE,BYTE
!               make CLASS EXP work...just paste in the entire declaration
!               take NAME() attribute and do not mangle or add @F and Preserve Case e.g. Kill  PROCEDURE(long),BYTE,NAME('externname')
!               added conversion to LONG for SIGNED UNSIGNED HANDLE HRESULT (ABC uses SIGNED and UNSIGNED a lot) these were coming out as named types
!               added BSTRING as "sw", ASTRING as "sa" based on what LINKNAME() returns, ErrorClass uses ASTRINGs, did not handle CProto of these
!               output data declarations including a little support for GROUP and QUEUE, even less for FILE.
!                   limitation: do not try to skip data inside GROUP, so just don't add it
!               insure check for C calling convention does not snag any ",C" e.g. PROCEDURE(LONG Idx),CBOOL,DERIVED
!               EXP entries are CASE Sensitive so allow keeping case. Clarion default is UPPER exports, but cwHH DLL did not UPPER (most times).. probably a waste of time
!               Ok I do INTERFACEs, damn it. They are a lot like CLASS just add after name _@_Interface_@_Class. Self is an issue for inherited interfaces
!                   You must paste the INTERFACE declaration after the Class and include the INTERFACE line
!                   if the Interface Inherits then you must PASTE the Parent and make it  Parent  INTERFACE!^,CHILD=inheritorsName
!                   if the CLASS line contains IMPLEMENTS then a message tells you to paste in the Interface
!                   if an INTERFACE lines (Inherits) then a message tells to add Base Interface with !^,CHILD=inheritorsName
!               Resize Window, moved stuff to allow for simple resize where FULL on TEXT expands some space.
!               added button to Copy original Prototypes to make testing a little easier
!               added button to run nput prototypes thru my Clean process, mainly helps test that
!               added buttons to Upper or lower everything .... no real need
!   Note: re case issues, if trying to match a DLLs exports (like cwHH) you can export an EXP file from LibMaker with To Text
!   Note: did I need to do all this damn parsing? Could I have just added the Parens to Procs without and been fine?
!   07/11/07 an END in Column 1 confused me
!   07/15/07 a Private CONSTRUCT or DESTRUCT method is exported, kind of like if it's Virtual+Private
!   07/15/07 confirmed that the order of the EXP for Classed does not matter
!   07/15/07 source can now contain type EQUATE()'s e.g. REFII EQUATE(LONG) so mangle gets right base types
!            added many Windows standard equates e.g. HANDLE=>LONG, not sure if that's a good idea
!EndRegion History Comments

DbIt    SHORT(1)       !Cut off all the Debug     IF DbIt THEN db(

  MAP
    Clw2Exp(*cstring ProtoLine),string
    FlattenProtoTypes(*CSTRING pPrototypes)
    Tabs2Spaces(*CSTRING pInOutString)
CleanCWCode     procedure(string CWCode),string          !compresses and removes comment
IsKeyword       procedure(*string CodeLine, string Keyword2Find),long !Check if there is keyword followed by delim, return Pos
DB              procedure(string debugMsg)               !Output Debug
AddCWExpLine    procedure(string line2add,<STRING Protoline>)               !add line to CWExpLine EXP block
Load_EquateTypeQ    procedure()                          !Load EquateTypeQ 
                                                                                                        
LoadWindowTxt   procedure(*CSTRING Protoz, *STRING Rulez, *STRING Aboutz, *STRING Cw71z)  !Move big strings to bottom
    MODULE('Win32')
        outputdebugstring(*cstring),raw,pascal,name('outputdebugstringA')
    END
  END

BtnNo         BYTE
LenIn         LONG
MaxProto       EQUATE(60000)
CWproto       CString(60001)
ExpProto      CString(60001)
CProto        CString(60001)
CWProtoLine   CString(60001)
CWExpLine     CString(60001) 
RulesTxt      STRING(7100) 
AboutTxt      STRING(1200) 
Cw71Txt       STRING(2400) 

Ndx           Short
OrdinalColumn Short         !06/22/22 was BYTE, need negatives
IndentLevel   Byte
AllowFlatten  Byte
ProtoComments Byte          !Carl I allow  ; Prototype in EXP
OmitComments  Byte          !Carl I put junk in the file as ;comments
AddEXPHeader  Byte          !Carl add the EXP header entries
CaseProcName  Byte          !Carl preserve case of Procedure name
CaseClassName Byte          !Carl preserve case of Class name
CaseSelfName  Byte          !Carl preserve case of Class name used as SELF in procedure Protos, also other Named Parm Types
IniFile       String(File:MaxFilePath)
InClass         short,static            !Am I in a Class that needs to have the ClassName added first in each prtotype?
pClassname      pstring(255),static     !name of class to add
InInterface     short,static            !Am I in an Interface that needs to have InterfaceName SELF and _@_Interface_@_Class
pInterfaceName  pstring(255),static     !name of Interface to add to the function name as Method_@_Interface_@_Class then normal function
pInterfaceSelf  pstring(255),static     !name of Interface to add to the prototype as SELF

EquateTypeQ     QUEUE,PRE(EquQ)         !07/15/2007  Allow equates for other types to be in the pasted code
LabelType          string(64)           !The type that the developer called it
ClaType            string(64)           !aka Clarion type, the longest is probably APPLICATION
                END
NamedSymbolList  STRING(1000)           !a list of non-Clarion types found 
ShowInterfaceMsg BOOL                   !do not show it repeatedly           
ExpTextEditOk    BYTE

pFilePRE        PSTRING(20)          !10/23/17 try to do FILE quick-and-fast
pFileLabel      PSTRING(64)          !10/23/17 try to do FILE quick-and-fast

W   WINDOW('Prototype to EXP Export Mangled Name'),AT(,,490,220),CENTER,GRAY,SYSTEM,MAX,ICON('cbPro2Exp.ICO'), |
            FONT('Segoe UI',9,,FONT:regular),RESIZE
        SHEET,AT(2,2),FULL,USE(?SheetMain)
            TAB(' Clarion &Prototypes '),USE(?TabProto)
                STRING('List Clarion Prototypes and Exported Data below. Can have EQUATE for Types. Supports CLASS and I' & |
                        'NTERFACE (see CW EXP help)'),AT(27,19),USE(?ClarionPrototypesLabel),TRN
                BUTTON('&Mangle'),AT(25,30,46,12),USE(?MangleBtn),SKIP,ICON(ICON:VCRplay),TIP('Mangle Prototypes'),LEFT
                BUTTON,AT(6,47,17,16),USE(?PasteButton),SKIP,ICON(ICON:Paste),TIP('Paste Clarion prototype(s) from clipboard'), |
                        FLAT
                BUTTON,AT(6,63,17,14),USE(?RefreshButton),SKIP,ICON(ICON:VCRplay),TIP('Mangle Prototypes'),FLAT
                BUTTON,AT(6,86,17,16),USE(?CopyProtoButton),SKIP,ICON(ICON:Copy),TIP('Copy Clarion Prototypes'),FLAT
                BUTTON,AT(6,111,17,16),USE(?CleanProtoBtn),SKIP,ICON(ICON:JumpPage),TIP('Clean and compress the protypes' & |
                        '<13,10>Mainly a test of the CWClean Procedure Code'),FLAT
                BUTTON('UPR'),AT(6,134,17,16),USE(?UpperProtoBtn),SKIP,TIP('UPPER CASE Prototypes'),FLAT
                BUTTON('low'),AT(6,156,17,16),USE(?LowerProtoBtn),SKIP,FONT(,,,FONT:regular),TIP('Lower Case Prototypes'),FLAT
                BUTTON,AT(6,177,17,16),USE(?LeftJustifyBtn),SKIP,ICON(ICON:VCRtop),TIP('Left Jusify Text'),FLAT
                TEXT,AT(25,46),FULL,USE(Cwproto),HVSCROLL,FONT('Consolas',11)
                PROMPT('Indent:'),AT(82,32),USE(?IndentPrompt_1)
                SPIN(@n3b),AT(107,32,30,10),USE(IndentLevel,, ?IndentLevel_1),HVSCROLL,RANGE(1,20),STEP(1)
                PROMPT('Ordinal:'),AT(144,32),USE(?OrdinalPrompt_1)
                SPIN(@n2),AT(171,32,30,10),USE(OrdinalColumn,, ?OrdinalColumn_1),HVSCROLL,RANGE(0,99)
                CHECK('Flatten'),AT(212,32,33),USE(AllowFlatten,, ?AllowFlatten_1),TRN
                CHECK('; Proto'),AT(252,32,32),USE(ProtoComments,, ?ProtoComments_1),TRN
                CHECK('O&mit ;'),AT(290,32,30),USE(OmitComments,, ?OmitComments_1),TRN
                CHECK('&Header'),AT(327,32,35),USE(AddEXPHeader,, ?AddEXPHeader_1),TRN
                PROMPT('Case:'),AT(370,32),USE(?CasePrompt_1),TRN
                CHECK('Proc'),AT(390,32,27),USE(CaseProcName,, ?CaseProcName_1),TRN
                CHECK('Class'),AT(418,32,29),USE(CaseClassName,, ?CaseClassName_1),TRN
                CHECK('Self'),AT(451,32,24),USE(CaseSelfName,, ?CaseSelfName_1),TRN
            END
            TAB(' E&XP Exports '),USE(?TabEXP)
                STRING('Clarion EXP File Mangled Exports'),AT(27,19),USE(?ClarionExpLabel),TRN
                BUTTON('&Mangle'),AT(25,30,46,12),USE(?MangleBtn2),SKIP,ICON(ICON:VCRplay),LEFT
                BUTTON,AT(6,46,17,16),USE(?CopyExportButton),SKIP,ICON(ICON:Copy),TIP('Copy Clarion .EXP text.'),FLAT
                BUTTON,AT(6,72,17,14),USE(?RefreshButton2),SKIP,ICON(ICON:VCRplay),TIP('Refresh Prototypes Mangle using ' & |
                        'current settings'),FLAT
                CHECK,AT(9,96,11,14),USE(ExpTextEditOk),SKIP,TIP('Edit EXP Text, unchecked is ReadOnly')
                PROMPT('E<13,10>D<13,10>I<13,10>T'),AT(10,110,6,40),USE(?PromptExpTextEditOk),CENTER
                TEXT,AT(25,46),FULL,USE(ExpProto),HVSCROLL,FONT('Consolas',11),READONLY
                PROMPT('Indent:'),AT(82,32),USE(?IndentPrompt)
                SPIN(@n3b),AT(107,32,30,10),USE(IndentLevel),HVSCROLL,TIP('Number of columns to indent name'), |
                        RANGE(1,20),STEP(1)
                PROMPT('Ordinal:'),AT(144,32),USE(?OrdinalPrompt)
                SPIN(@n2),AT(171,32,30,10),USE(OrdinalColumn),HVSCROLL,TIP('The number of columns over that you want th' & |
                        'e "@?" aligned'),RANGE(0,99)
                CHECK('Flatten'),AT(212,32,33),USE(AllowFlatten),TRN,TIP('Allows the conversion of multiline prototypes ' & |
                        'into single line prototypes before processing.<13,10>E.G. This:<13,10,13,10>FormatAddress(<<Str' & |
                        'ing pStreetAddress>,|<13,10> {11}   <<String pCity>,|<13,10> {14}<<String pState>,|<13,10> {14}' & |
                        '<<String pZIP>,|<13><10> {14}<<String pCountry>),String<13,10,13,10>Becomes This:<13,10,13><10>' & |
                        'FormatAddress(<<String pStreetAddress>,<<String pCity>,<<String pState>,<<String pZIP>,<<String' & |
                        ' pCountry>),String')
                CHECK('; Proto'),AT(252,32,32),USE(ProtoComments),TRN,TIP('Add ; Procedure Prototype Comment lines to the EXP')
                CHECK('O&mit ;'),AT(290,32,30),USE(OmitComments),TIP('Omit ; Problem Comment lines<13,10>Parsing problem' & |
                        's generate lines into EXP as ; Comments<13,10,9>; Data not exported ==><13,10,9>; Private not e' & |
                        'xported =>')
                CHECK('&Header'),AT(327,32,35),USE(AddEXPHeader),TRN,TIP('Add EXP Header at Top <13,10><13><10>NAME dlln' & |
                        'ame GUI<13,10>; IMAGE_BASE 05000000h  <13,10>EXPORTS')
                PROMPT('Case:'),AT(370,32),USE(?CasePrompt),TRN
                CHECK('Proc'),AT(391,32,27),USE(CaseProcName),TRN,TIP('EXP Entries ARE Case Sensitive<13,10>Clarion norm' & |
                        'ally UPPERs EXP Procedure Names<13,10>Check to preserve the case of the Procedure Name<13,10>Un' & |
                        'check to UPPER (the norm)')
                CHECK('Class'),AT(418,32,29),USE(CaseClassName),TRN,TIP('EXP Entries ARE Case Sensitive<13><10>Clarion n' & |
                        'ormally UPPERs EXP Class Names<13,10>Check this to preserve the case of the Class Name<13,10>Un' & |
                        'check to UPPER (the norm)')
                CHECK('Self'),AT(451,32,24),USE(CaseSelfName),TRN,TIP('EXP Entries ARE Case Sensitive<13,10>Clarion norm' & |
                        'ally UPPERs EXP Class Names when used as SELF<13,10>Check this to preserve the case of the Clas' & |
                        's Name as SELF<13,10>Uncheck to UPPER<13,10>This affects ALL Named Parameter Types')
            END
            TAB(' C Prototypes '),USE(?TabCPrototypes)
                STRING('Clarion C Prototypes - From original Pro2Exp I have not verified these are correct'),AT(27,20), |
                        USE(?ClarionCProtoLabel),TRN
                TEXT,AT(25,33),FULL,USE(CProto),HVSCROLL,FONT('Consolas',11,COLOR:Black),COLOR(0F7F7F7H),READONLY
                BUTTON,AT(6,30,17,16),USE(?CopyCPrototypeButton),ICON(ICON:Copy),TIP('Copy C Prototype text.'),FLAT
            END
            TAB(' Mangle &Rules '),USE(?TabRules)
                TEXT,AT(8,22),FULL,USE(RulesTxt),VSCROLL,FONT('Consolas',11,COLOR:Black),COLOR(0E1FFFFH),READONLY
            END
            TAB(' &7.1+ EXPORT '),USE(?Tab71Export)
                TEXT,AT(8,22),FULL,USE(Cw71Txt),VSCROLL,FONT('Consolas',11,COLOR:Black),COLOR(0E1FFFFH),READONLY
            END
            TAB('  About  '),USE(?TabAbout)
                TEXT,AT(8,22),FULL,USE(AboutTxt),VSCROLL,FONT('Calibri',11,COLOR:Black),COLOR(0E1FFFFH),READONLY
            END
        END
        BUTTON('ReRun'),AT(370,2,,9),USE(?ReRunBtn),TRN,SKIP,TIP('Run another instance')
    END

Converter CLASS,TYPE
Hold        CSTRING(1000)
IsRaw       BYTE
NoMangle    BYTE
IsOmitable  BYTE
IsAddress   BYTE
Adims       BYTE
Convert     FUNCTION(*cstring),STRING
StoreName   PROCEDURE(string,byte PreserveCase),VIRTUAL         !Carl  added PreserveCase
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StoreResult PROCEDURE(Byte),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
EndProc     PROCEDURE,VIRTUAL
          END

ExpConverter CLASS(Converter)
!StoreName   PROCEDURE(string,byte PreserveCase=0),VIRTUAL      !Carl PreserveCase to Base Class
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
          END

CConverter CLASS(Converter)
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
EndProc     PROCEDURE,VIRTUAL
          END
  CODE
  DO GetIniRtn 
  Load_EquateTypeQ()  
  LoadWindowTxt(CWProto, RulesTxt, AboutTxt, Cw71Txt)  !Move big strings to bottom
     
  OPEN(W)
  0{PROP:MinWidth} =0{PROP:Width}  * .75
  0{PROP:MinHeight}=0{PROP:Height} * .75
  ?SheetMain{PROP:TabSheetStyle}=1
  ?ExpProto{PROP:Background}=COLOR:BTNFACE
    ?MangleBtn2{PROP:Tip} = ?MangleBtn{PROP:Tip}
    ?IndentLevel_1  {PROP:Tip}=?IndentLevel  {PROP:Tip}
    ?OrdinalColumn_1{PROP:Tip}=?OrdinalColumn{PROP:Tip}
    ?AllowFlatten_1 {PROP:Tip}=?AllowFlatten {PROP:Tip}
    ?ProtoComments_1{PROP:Tip}=?ProtoComments{PROP:Tip}
    ?OmitComments_1 {PROP:Tip}=?OmitComments {PROP:Tip}
    ?AddEXPHeader_1 {PROP:Tip}=?AddEXPHeader {PROP:Tip}
    ?CaseProcName_1 {PROP:Tip}=?CaseProcName {PROP:Tip}
    ?CaseClassName_1{PROP:Tip}=?CaseClassName{PROP:Tip} 
    ?CaseSelfName_1 {PROP:Tip}=?CaseSelfName {PROP:Tip}
  
  SELECT(?CWProto)
  ACCEPT
    CASE ACCEPTED()
    OF ?CWProto ; Tabs2Spaces(CWproto)
    OF ?MangleBtn  OROF ?MangleBtn2
        if ~AddEXPHeader
            clear(ExpProto)
        else
            ExpProto='NAME dllname GUI' & |
                     '<13,10>IMAGE_BASE 05000000h  ;--use address 0500 to 6FF0 ending with 0000 to base your DLL uniquely' & |
                     '<13,10>EXPORTS'
        end
        Clear(NamedSymbolList)
        Clear(CProto)
        if ~CWProto then cycle.         !Carl added since blanking would hang things
        clear(InClass   )
        clear(pClassname)

        If AllowFlatten
          FlattenPrototypes(CWProto)
          display                           !Carl added or control not updated and lines are stale
        end
        Loop Ndx = 1 to ?CWProto{PROP:LineCount}
          !carl CWProtoLine =  Clip(Left(?cwproto{PROP:Line,Ndx}))
          CWProtoLine =  Clip(?cwproto{PROP:Line,Ndx})              !Carl - I want to preserve indent of END?
          If NOT CWProtoLine then cycle.
          If LEFT(CWProtoLine,1)='!' then cycle.            !06/08/22 no !Comments
          !Carl  Do RemoveProcFunc
          !Carl  CWProtoLine = All(' ',IndentLevel) & CWProtoLine
          CWExpLine = Clip(Clw2Exp(CWProtoLine))
          AddCWExpLine(CWExpLine, CWProtoLine)

          If CProto
            CProto = CProto & '<13,10>' & Clip(CConverter.Convert(CWProtoLine))
          else
            CProto = Clip(CConverter.Convert(CWProtoLine))
          end
        end
        if NamedSymbolList
            ExpProto=clip(ExpProto) & '<13,10>  ; NamedSymbols: ' & NamedSymbolList
        end
        ExpProto=clip(ExpProto) & '<13,10>'
        Display 
        SELECT(?ExpProto)                      !10/22/17 switch tabs to show Mangle 

    of ?PasteButton       
         ! 06/21/22 was: CWProto = ClipBoard() ; Tabs2Spaces(CWproto)
         LenIn = LEN(CLIP(CLIPBOARD()))
         IF LenIn = 0 THEN
            Message('Clipboard is Empty','Paste') ; CYCLE 
         ELSIF LenIn <= MaxProto THEN
            CWProto = CLIPBOARD()
         ELSE   

            BtnNo=MESSAGE('Clipboard contains ' & LenIn &' bytes.' & |
                         '<13,10>The maximum size is ' & MaxProto & ' bytes.' & |
                         '||Select if you would like to process part of the paste.' & |
                         '||Note: There is an 80 byte overlap.|Check first and last lines for truncation.' & |
                         '','Paste Too Big',ICON:Asterisk, |
                         'Cancel' & |                                        !Button 1 Cancel
                               '|First ' & MaxProto & |                      !Button 2 First
                               '|Second ' & MaxProto & |                     !Button 3 
                         CHOOSE(LenIn < MaxProto * 3 - 80,'','|Third') & |   !Button 4
                         CHOOSE(LenIn < MaxProto * 4 - 80,'','|Fourth') & |  !Button 5
                         CHOOSE(LenIn < MaxProto * 5 - 80,'','|Fifth') )     !Button 6 5th
            CASE BtnNo
            OF 1 ; CYCLE 
            OF 2      ; CWProto = CLIPBOARD()  !test SUB(CLIPBOARD(),1,MaxProto)
            OF 3 TO 6 ; CWProto = SUB(CLIPBOARD(),1 + MaxProto * (BtnNo-2)-80,MaxProto)  
                        !Message('BtnNo=' & BtnNo & '|Start=' & 1 + MaxProto * (BtnNo-2)-80 )
            END
            LenIn=LEN(CLIP(CWProto))
        END
        ?{PROP:Tip}=LenIn & ' bytes pasted at ' & Format(Clock(),@t4)
        Tabs2Spaces(CWproto) 

        ShowInterfaceMsg = 0
        Post(Event:Accepted,?MangleBtn)
    of ?Refreshbutton 
    orof ?RefreshButton2      ;  Post(Event:Accepted,?MangleBtn)
    of ?CopyExportButton      ;  SetClipBoard(ExpProto)
    of ?CopyCPrototypeButton  ;  SetClipBoard(CProto)
    of ?CopyProtoButton       ;  SetClipBoard(Cwproto)
    of ?CleanProtoBtn         ;  DO CleanCWProtoRtn     ; DISPLAY 
    of ?UpperProtoBtn         ;  Cwproto=upper(Cwproto) ; DISPLAY !; Post(Event:Accepted,?CWProto)
    of ?LowerProtoBtn         ;  Cwproto=lower(Cwproto) ; DISPLAY !; Post(Event:Accepted,?CWProto)
    of ?LeftJustifyBtn        ;  DO LeftJustifyRtn ; DISPLAY
    of ?ExpTextEditOk         ;  ?ExpProto{PROP:Color}=CHOOSE(~ExpTextEditOk,COLOR:BTNFACE,COLOR:None) 
                              ;  ?ExpProto{PROP:ReadOnly}=CHOOSE(~ExpTextEditOk) ; DISPLAY 
    of ?ReRunBtn              ;  RUN(COMMAND('0'))
    END !Accepted

    CASE Event()
    OF Event:Accepted orof Event:NewSelection
      Case Field()
      of   ?IndentLevel  orof ?OrdinalColumn orof ?OmitComments  orof ?ProtoComments
      orof ?AddEXPHeader orof ?CaseProcName  orof ?CaseClassName orof ?CaseSelfName
            POST(Event:Accepted,?MangleBtn)
      end
    OF Event:CloseWindow    ;  DO PutIniRtn
    OF Event:Rejected       ;  DISPLAY(?) ; SELECT(?)
    END
  END
  RETURN 

GetIniRtn ROUTINE
    IniFile = LongPath('.\cbPro2Exp.INI') 
    IndentLevel   = GetINI('Defaults','IndentLevel',    2,IniFile)
    OrdinalColumn = GetINI('Defaults','OrdinalColumn',  0,IniFile) ; IF OrdinalColumn>99 THEN OrdinalColumn=99. !06/21/22 was 60
    AllowFlatten  = GetINI('Defaults','AllowFlatten',   1,IniFile)
    ProtoComments = GetINI('Defaults','ProtoComments',  0,IniFile)
    OmitComments  = GetINI('Defaults','OmitComments',   1,IniFile)  !06/21/22 was 0, these are confusing so omit by default
    AddEXPHeader  = GetINI('Defaults','AddEXPHeader',   1,IniFile)
    CaseProcName  = GetINI('Defaults','CaseProcName',   0,IniFile)
    CaseClassName = GetINI('Defaults','CaseClassName',  0,IniFile)
    CaseSelfName  = GetINI('Defaults','CaseSelfName',   0,IniFile)
    EXIT 
PutIniRtn ROUTINE
    PutINI('Defaults','IndentLevel',    IndentLevel  ,IniFile)
    PutINI('Defaults','OrdinalColumn',  OrdinalColumn,IniFile)
    PutINI('Defaults','AllowFlatten',   AllowFlatten ,IniFile)
    PutINI('Defaults','ProtoComments',  ProtoComments,IniFile)
    PutINI('Defaults','OmitComments',   OmitComments ,IniFile)
    PutINI('Defaults','AddEXPHeader',   AddEXPHeader ,IniFile)
    PutINI('Defaults','CaseProcName',   CaseProcName ,IniFile)        !should I preserve these Case things, could get in trouble
    PutINI('Defaults','CaseClassName',  CaseClassName,IniFile)
    PutINI('Defaults','CaseSelfName',   CaseSelfName ,IniFile)
    EXIT
    
CleanCWProtoRtn     routine
    data
Cln         like(CWProto)
    code
    Loop Ndx = 1 to ?CWProto{PROP:LineCount}
          CWProtoLine =  CleanCWCode(?cwproto{PROP:Line,Ndx})
          if CWProtoLine then Cln=clip(Cln) & choose(~val(cln[1]),'','<13,10>') & clip(CWProtoLine).
    end
    CWProto=Cln
    display
    exit
LeftJustifyRtn     routine
    data
Cln         like(CWProto)
    code
    Loop Ndx = 1 to ?CWProto{PROP:LineCount}
          CWProtoLine =  LEFT(?cwproto{PROP:Line,Ndx}) 
          CASE UPPER(CWProtoLine)
          OF 'END' OROF '.' ; CWProtoLine=?cwproto{PROP:Line,Ndx} !Leave as is
          END 
          if CWProtoLine then Cln=clip(Cln) & choose(~val(cln[1]),'','<13,10>') & clip(CWProtoLine).
    end
    CWProto=Cln
    display
    exit

AddCWExpLine    procedure(string line2add,<STRING Protoline>)
llen        long,auto
pCRLF       pstring(4) 
OrdColAdj   LIKE(OrdinalColumn)             !10/23/17 adjust for Indent
SemiPos     ushort  
    code
    OrdColAdj = OrdinalColumn - IndentLevel - 1                     !10/23/17 
    if OrdColAdj <= IndentLevel THEN OrdColAdj = IndentLevel + 2.   !10/23/17 
    llen=len(clip(line2add))
    if ~llen then return.
    if  VAL(ExpProto[1])>0 then pCRLF='<13,10>'.
    SemiPos=instring(';', line2add,1) !02/28/21 Double ;; are omittable 
    if SemiPos AND line2add[1+SemiPos]=';' then  !is =';;' 
       if ~OmitComments 
           line2add=sub(line2add,1,SemiPos) & |  !Change ;; to ;
                    sub(line2add,SemiPos+2,llen) 
           llen -= 1
           ExpProto = ExpProto & pCRLF & All(' ',IndentLevel) & line2add[1 : llen] 
       end 

    elsif SemiPos AND line2add[1+SemiPos]='!' then  !is =';!' a Warning! line
          ExpProto = ExpProto & pCRLF &' '& line2add[1 : llen]  !Indent 1 ' '
          
    elsif SemiPos then !left(line2add,1)=';'  " ; comment"
          ExpProto = ExpProto & pCRLF & All(' ',IndentLevel) & line2add[1 : llen] 

    else  
          IF ProtoComments AND ~OMITTED(Protoline) AND Protoline THEN   !10/23/17 see how Proto looks in EXP as ; Comment
             ExpProto = ExpProto & pCRLF & All(' ',IndentLevel) &'  ; ' & CLIP(Protoline)   !10/23/17 
             pCRLF='<13,10>'                                                        !10/23/17 
          END 
          ExpProto = ExpProto & pCRLF & All(' ',IndentLevel) & |
                     line2add[1 : llen] & Choose(OrdColAdj <= llen,' ','') & ALL(' ',OrdColAdj - llen) & '@?'
                     !line2add[1 : llen] & Choose(OrdinalColumn <= llen,' ','') & ALL(' ',OrdinalColumn - llen) & '@?'
    end
    return


Clw2Exp PROCEDURE(*CSTRING ins)
CwLine          string(1000),auto
TempUpr         string(1000),auto  !don't try to have UPPER CW line, need to keep synching as CWLine changes
CWLineLen       long,auto
XX              long,auto
CloseParen      long,auto
CwLabel         string(255)         !10/23/17 renamed CW+Label
ClarionType     string(16),auto
Keyword         string(20)    !for keyword after label like PROCEDURE or CLASS or FUNCTION
KeyWordPos      long
KeyWordEnd      long
KeyWordCharAfter  string(1) 
TYPE_definition LONG        !Locaton of TYPE on line
CwUpr           string(1000)
ePROCEDURE      equate('PROCEDURE')
eFUNCTION       equate('FUNCTION')
eCLASS          equate('CLASS')
eINTERFACE      equate('INTERFACE')
!pFilePRE        PSTRING(20)          !10/23/17 try to do FILE quick-and-fast
!pFileLabel      PSTRING(64)          !10/23/17 try to do FILE quick-and-fast
  CODE
    IF DbIt THEN DB('----- Clw2Exp ----- ' & ins).
    If LEFT(ins,1)='!' then return ''.              !06/08/22 no !Comments
    CwLine=CleanCWCode(ins)                         !compresses and removes comments
    CWLineLen=len(clip(CWLine))
    if ~CWLineLen then return ''.

    !2 possible line formats, 1=new style  2=old    3=old but not indented
    !1:label      keyword(xxxxx
    !2:           label(xxxxx       << old procedure declarartion
    !3:label(xxxxx                  old style that starts in damn column 1
    if ~CwLine[1]                                       !No label in column 1?
        case upper(left(CwLine))
        of '.' orof 'END' orof '!'
            return ''
        end
    elsif inlist(upper(left(CwLine)),'END','.')
        return ''                                       !07/11/07 Chris test had END in column 1 and it seems to work
    else !CwLine[1]                                     !Has to start in column 1 or old style prototype
       XX=instring(' ',CwLine,1)
       if XX > CWLineLen              |                 !Just has label, must be an oldStyle proc w/o parms
       or inrange(instring('(',CwLine,1),1,XX) |        !or if space after (
       or inrange(instring(',',CwLine,1),1,XX)                   !or if psace after comma
          CwLine=' ' & CwLine                           !move out of column 1
          goto OldStyleProcDeclareLabel
       end
       !if XX > CWLineLen                                !Just has label, must be an oldStyle
       !     return ' ; what? ' & clip(CwLine)
       !end
       CwLabel = sub(CWLine,1,XX)                         !Label is text up until the first space
       loop XX = XX to CWLineLen+1                  
            if ~KeyWordPos and CwLine[XX] > ' ' then    !find the first character after the space
                KeyWordPos = XX
            elsif KeyWordPos                            !find the first delim after the keyword
                case CwLine[XX]
                of ',' orof '(' orof ' '
                   KeyWordEnd       = XX - 1
                   KeyWordCharAfter = CwLine[XX]
                   break
                end

            end
       end
       if ~KeyWordEnd or ~KeyWordPos                    !can't see how this could happen but lets not blow up
            return '  ; what2? ' & CwLine
       end
       Keyword=upper(CwLine[KeyWordPos : KeyWordEnd])  !e.g. QUEUE GROUP ANY
       TYPE_definition=STRPOS(CwLine,', *TYPE{{$|[ ,]}',1)
    IF DbIt THEN DB('Keyword=' & Keyword &'  TYPE=' & TYPE_definition).
       if Keyword = ePROCEDURE                                  !change to old style Proc declaration
          CwLine=' ' & |                                        !move out of column 1 the label column
                sub(CwLine,1,KeyWordPos-1) & |                  !take the label
                choose(KeyWordCharAfter<>'(','()','') & |       !change procedure,long to procedure(),long
                sub(CwLine,KeyWordPos+len(ePROCEDURE),999)      !add what's after the word PROCEDURE
          
       elsif Keyword = eFUNCTION
          !CwLine=' ' & sub(CwLine,1,KeyWordPos-1) & sub(CwLine,KeyWordPos+len(eFUNCTION),999)
          CwLine=' ' & |                                        !move out of column 1 the label column
                sub(CwLine,1,KeyWordPos-1) & |                  !take the label
                choose(KeyWordCharAfter<>'(','()','') & |       !change procedure,long to procedure(),long
                sub(CwLine,KeyWordPos+len(eFUNCTION),999)      !add what's after the word PROCEDURE

       elsif Keyword = eCLASS
            InClass = 1
            InInterface = 0                                         !If new CLASS after Interface
            if instring('IMPLEMENTS',UPPER(CwLine),1) AND ~ShowInterfaceMsg
               ShowInterfaceMsg = 1
               message('If a Class IMPLEMENTS(Interface) you must: {30}' & |
                        '||1. Paste the full Interface declaration after the Class' & |
                        '|2. If the Interface(parent) inherits/derives include the Parent Interface'  & |
                        '|3. On the Parent INTERFACE line code as INTERFACE!^,CHILD=InheritorName' & |
                        '|4. Remove the IMPLEMENTS() from the CLASS line to stop this message' & |
                        '||Code: ' & CWLine, 'IMPLEMENTS() Detected', ICON:Asterisk )
            end
            AddCWExpLine(';={20} Class: ' & clip(CwLabel) &' ={20}') !02/28/21 Mark top of == Class == in EXP
            pClassname = clip(choose(~CaseClassName,upper(CwLabel),CwLabel))
            AddCWExpLine(' VMT$' & pClassname, CwLine)
            AddCWExpLine(' TYPE$' & pClassname)
            pClassname = clip(choose(~CaseSelfName,upper(CwLabel),CwLabel))  !Now pClass will be used in the Proc Prototypes as SELF, typically upper
            return ''  !'VMT$' & pClassname & '   @?<13,10>' & All(' ',IndentLevel) & 'TYPE$' & pClassname

       elsif Keyword = eINTERFACE
            if ~InClass
                Message('An INTERFACE declaration must put after the Class that IMPLEMENTS it.')
            else
                InInterface = 1
                if instring(eINTERFACE&'(',UPPER(CwLine),1) and ~instring(',CHILD=',UPPER(CWProto),1)
                   message('An Interface that inherrits must:' & |
                            '|1. Include the complete Parent interface declaration after the Child' & |
                            '|2. Have "!^,CHILD=ChildInterfaceName" on the Parent INTERFACE line end', |
                            '|3. The Parent should be added once for each child that inherits it', |
                            '||' & CWLine )
                end
                pInterfaceName = clip(CwLabel)
                pInterfaceSelf = clip(CwLabel)
                !A special Hack for an Interface that gets inherited e.g.
                !iGDI          interface
                !iLegend             interface(iGDI)
                !change the iGDI line to be
                !iGDI          interface,CHILD=iLegend
                XX=instring('CHILD=',UPPER(CwLine),1)
                if XX>0
                   !pInterfaceSelf = clip(CwLabel)                        !Parent/Base Interface name is SELF
                   pInterfaceName=clip(Left(sub(CWLine,XX+6,255)))      !Child/Inheritor name is Interface name used in method name mangle
                end
                if ~CaseClassName then pInterfaceName = upper(pInterfaceName).   !This not not work unless also check "Case Proc"
                if ~CaseSelfName  then pInterfaceSelf = upper(pInterfaceSelf).
                return ''
            end
!                  Method_@_Interface_@_Class@FInterfaceSelfxxx
!                  Method_@_InterfaceChild_@_Class@FInterfaceParentSelfxxx
!                  CPOLYGON_@_INODE_@_GRAPHCLASS@F5INODEddddl9GFILLTYPE    @?
!                  CIRCLE_@_INODE_@_GRAPHCLASS@F5INODEddd9GFILLTYPE        @?
!                  ERRCODE_@_INODE_@_GRAPHCLASS@F4IGDIOl                   @?  << INODE is Child, IGDI is Parent

       else
            if InClass = 1 then return('  ;; CLASS Data is NOT exported ==> ' & CwLine).
            CwLabel=clip(choose(~CaseClassName,upper(CwLabel),CwLabel))            !Typically UPPER but follow what ever Class does
            case upper(KeyWord)
            of 'GROUP'                       !note OVER is not exported
               if ~TYPE_definition then 
                  AddCWExpLine('  TYPE$' & CwLabel)
                  AddCWExpLine('  $' &     CwLabel) 
               else
                  AddCWExpLine(';! TYPE Group is NOT exported - Remove from first tab: '& CLIP(CwLabel) &' GROUP')
               end
               return '  ;; GROUP data is NOT exported, remove any from first tab'
            of 'QUEUE'
               if ~TYPE_definition then 
                  AddCWExpLine('  TYPE$' & CwLabel)
                  AddCWExpLine('  TCB$' &  CwLabel)
                  AddCWExpLine('  $' &     CwLabel)
               else
                  AddCWExpLine(';! TYPE Queue is NOT exported - Remove from first tab: '& CLIP(CwLabel) &' QUEUE')
               end
               return '  ;; QUEUE data is NOT exported, remove any from first tab'
            of 'FILE'
                AddCWExpLine('  ; File exports $File plus Keys and Record, Memos and Blobs, but no fields - ' & CwLine)     !10/23/17 add FILE 
                AddCWExpLine('  $' &     CwLabel, CwLine)     !10/23/17 add FILE 
                pFileLabel = CLIP(CwLabel) &'$'
                pFilePRE = CLIP(CwLabel) &':'
                XX=INSTRING('PRE(',UPPER(CwLine),1)
                IF XX THEN 
                   pFilePRE = LEFT(SUB(CwLine,XX+4,99)) & ':'
                   XX=INSTRING(')',UPPER(pFilePRE),1) 
                   IF XX THEN pFilePRE=SUB(pFilePRE,1,XX-1) & ':'.
                END 
                pFilePRE = UPPER(pFilePRE)
                RETURN('') 
                !return('  ; File exports $File plus Keys and Record, Memos and Blobs, but no fields - ' & CwLine)
            of 'KEY' orof 'INDEX' orof 'MEMO' orof 'BLOB'
                AddCWExpLine('  ' & pFileLabel & pFilePRE & CwLabel)     !10/23/17 add FILE  
                RETURN('') 
                !return(' >  ; File Key exports File$Pre:Key - ' & CwLine)
            of 'RECORD'
                AddCWExpLine('  ' & pFileLabel & pFilePRE & CwLabel)     !10/23/17 add FILE  
                AddCWExpLine('  ' & pFileLabel & 'TYPE$' & pFilePRE & CwLabel)     !10/23/17 add FILE  
                RETURN('')             
                return(' >  ; File Record exports File$Pre:Record and File$TYPE$Pre:Record - ' & CwLine)
            of 'MEMO'
                return(' >  ; File Record exports File$Pre:Memo - ' & CwLine)
            of 'BLOB'
                return(' >  ; File Record exports File$Pre:Blob - ' & CwLine)

            of 'EQUATE'                      !Allow the code to contain equates to deal with special data types
                !in Source can define Equates so get the right Clarion types in Mangling
                ClarionType='?'
                if KeyWordEnd and KeyWordCharAfter='('                      !Is the line LABEL EQUATE(xxxx)?
                   ClarionType=upper(CwLine[KeyWordEnd+2 : size(CwLine)])   !get xxxxx)
                   XX=instring(')',ClarionType,1)
                   IF XX then ClarionType=UPPER(LEFT(sub(ClarionType,1,XX-1))).  !Should be the original Clarion type
!                   !First is this Equate replacing an existing one?
                    IF DbIt THEN DB('Equate in Source: ' & clip(CwLabel) & ' EQUATE(' & clip(ClarionType) & ')' ).
!                IF DbIt THEN DB('   CwLine=' & CwLine ).

                   EquQ:LabelType = UPPER(CwLabel)
                   GET(EquateTypeQ,EquQ:LabelType)
                   IF ~ERRORCODE() THEN
                       IF DbIt THEN DB('   Equate DELETE: ' & clip(EquQ:LabelType) & ' Equate(' & EquQ:ClaType ) .
                       DELETE(EquateTypeQ)                           !User redefined what I already did e.g. HWND (HANDLE)
                   END
                   LOOP 50 times                    !translate equate into Clarion type, can do HWND=>HANDLE=>SIGNED=>LONG
                        EquQ:LabelType = ClarionType
                        GET(EquateTypeQ,EquQ:LabelType)
                        IF ERRORCODE() THEN return ''.  !BREAK.         !if not EQUATE (a clarion type) then forget it
                        IF EquQ:ClaType = EquQ:LabelType THEN BREAK.    !Eqts queue has Clarion base type (i.e. LONG Equate(LONG))
                        IF DbIt THEN DB('   Equate ' & clip(ClarionType) & ' redefined to ' & EquQ:ClaType ) .
                        ClarionType = EquQ:ClaType                    !e.g. have HWND Equate(HANDLE) and just found HANDLE Equate(LONG)
                   END
                   EquQ:LabelType = UPPER(CwLabel)
                   EquQ:ClaType = UPPER(ClarionType)
                   ADD(EquateTypeQ,EquQ:LabelType)
                   IF DbIt THEN DB('   Equate ADD: ' & clip(EquQ:LabelType) & ' Equate(' & EquQ:ClaType ) .
                   return '  ;; Equate ' & Clip(CwLabel) & ' as ' & ClarionType
                end
                return ''
            else
                AddCWExpLine('  $' &     CwLabel)
                return ''
            end
            return('  ; No Data? ' & CwLine)
           
       end
    end
OldStyleProcDeclareLabel
    !make sure we have ()'s in the declaration
       XX=instring('(',CwLine,1)                                !find ( in  name(
       if ~XX then                                              !no ( found?
          XX=instring(',',CwLine,1)                             !is it: ProcName,return
          if ~XX then XX=len(clip(CwLine)).                     !nope, must be just ProcName
          CWLine=sub(CWLine,1,XX) & '()' & sub(CWLine,XX+1,999) !Add the ()
          XX += 1                                               !Xx now = (
       end

    if InClass and ~InInterface                                 !If in a Class then need to add class to prototype
       XX=instring('(',CwLine,1)                                !find ( in  name(
       CWLine=sub(CWLine,1,XX) & |                              !has  ProcedureName(
                pClassname & ' self' & |                         !add  classname self
                choose(CWLine[XX+1]=')','',',') & |             !add                ,
                sub(CWLine,XX+1,999)                            !add  ),hhh
       TempUpr = UPPER(CWLine)

       !Class member that are Private are NOT exported (unless virtual)  VIRTUAL DERIVED
       CloseParen=instring(')',TempUpr,1)
       if   instring(',PRIVATE',TempUpr,1,CloseParen) |
       and ~instring(',VIRTUAL',TempUpr,1,CloseParen) |         !Private,VIRTUAL is in EXP
       and ~instring(',DERIVED',TempUpr,1,CloseParen) |         !Private,Derived is same as Virtual
       and ~inlist(UPPER(CwLabel),'CONSTRUCT','DESTRUCT')         !Private Construct/Destruct goes into EXP
            return '  ;; Private not exported => ' & clip(ins)
       end

    elsif InClass and InInterface                               !If in an Interface
       !At this point we have an old style procedure declaration  name(parms)
       !MethodName_@_InterfaceName_@_Class then normal function
       XX=instring('(',CwLine,1)                                !find ( in  name( so can add SELF after
       CWLine=clip(sub(CWLine,1,XX-1)) & |                      !has  ProcedureName
                '_@_' & pInterfaceName & |
                '_@_' & pClassName & |
                '(' & |
                pInterfaceSelf & ' self' & |                    !add  InterfaceName self, this is not correct for inherited
                choose(CWLine[XX+1]=')','',',') & |             !add                ,
                sub(CWLine,XX+1,999)                            !add  ),hhh
!       XX=instring(' ',CwLine,1)                                !find space after name
!       CWLine=sub(CWLine,1,XX-1) & |                            !has  ProcedureName
!              sub(CWLine,XX,999)
!       TempUpr = UPPER(CWLine)
    end
    IF DbIt THEN DB('xxxxxCWCode: ' & CwLine).
    ins=clip(CwLine)
            
    RETURN ExpConverter.Convert(ins)

IsKeyword PROCEDURE(*string CodeLine, string Keyword2Find)!,long !Check if there is keyword followed by delim, return Pos
Rtn long,auto
    code
    Keyword2Find=upper(Keyword2Find)
    Rtn=instring(Keyword2Find,upper(CodeLine),1)
    if Rtn then
       if ~instring(sub(CodeLine,Rtn+size(Keyword2Find),1),' ,(',1) then        !Needs to be Keyword followed by delimeter , (
           Rtn=0
       end
    end
    RETURN Rtn

CleanCWCode     procedure(string CWCode)!,string                 !compresses and removes comment
Cln     string(1000)
XOut    long
CWCodeLen   long,auto
XIn         long,auto
InQuote short
AQuote  EQUATE('<39>')
Char    string(1)
    code
    IF DbIt THEN DB('CleanCWCode: ' & CWCode) .
    CWCodeLen = len(clip(CWCode))
    loop Xin = 1 to CWCodeLen
         Char = CWCode[Xin]
         if Char=AQuote then InQuote=1 - InQuote.        !Toggle InQuote indicator
         if ~InQuote
             Case Char
             of '!'
                    if Xin<CWCodeLen and CWCode[Xin+1]<>'^' then break.  !10/23/17 IF<Len  !hit a comment, all done  (keep comments that are !^ so can have !^,CHILD=
             of '|' ; break                             !line continue means done but should not see if Jeff flattened
             of ''                                      !smash out unneeded spaces
                if XOut |                               !any output chars
                and inlist(Cln[XOut],'(',')',',',' ','<<','*')  !10/22/17 added "<"   !if prev out character is a delim then do not need this space
                    cycle
                end
                if Xin < CWCodeLen |
                and inlist(CWCode[Xin+1],'(',',',')','>')       !10/22/17 added ">"   !if next character is a delim then do not need this space
                    cycle
                end
               
             end !case Char
         end !if ~Inqt
         XOut += 1
         Cln[XOut]=Char
    end
    IF DbIt THEN DB('CleanedCode: ' & Cln).
    return clip(Cln)
!-------------------------------
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('Pro2Exp: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+3),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage) & '<13,10>'
  OutputDebugString( sz )
  return
!-------------------------------
Converter.StoreName   PROCEDURE(string s,byte PreserveCase)
  CODE
   ! Self.hold = Clip(s)
    Self.Hold = Clip(choose(~PreserveCase,UPPER(s),s))         !Carl my cwHH prototypes were UPLOW

Converter.StoreSym    PROCEDURE(Byte b,byte b1,string s)
  CODE

Converter.StoreResult PROCEDURE(Byte b)
  CODE

Converter.EndProc     PROCEDURE
  CODE

Converter.StartProc     PROCEDURE
  CODE

Converter.Convert FUNCTION(ins)
Gn       SIGNED,AUTO
EndP     SIGNED
Symbol   CSTRING(80)
SymbolP  SIGNED,AUTO
SymVal   BYTE
TVal     BYTE
NameX    LONG
sName    string(255),auto
  CODE
    Gn = INSTRING('(',ins)
    IF ~Gn THEN
      Self.StoreName(ins,CaseProcName)
      Self.NoMangle = 1
    ELSE
      Self.StoreName(ins[1:Gn-1],CaseProcName)
      !carl LOOP EndP = LEN(Ins) TO 1 BY -1     searching backwards runs into a NAME() attribute incorrectly
      !carl UNTIL Ins[EndP] = ')'
      LOOP EndP = Gn TO LEN(Ins)
      UNTIL Ins[EndP] = ')'

      ! Raw mode causes great C++ linkname compatability
      Self.IsRaw = CHOOSE( EndP AND Instring(',RAW',UPPER(Ins),1,EndP),1,0)
      ! An ill-formed prototype is not mangled
      Self.NoMangle = CHOOSE( ~EndP OR Instring(',PASCAL',UPPER(Ins),1,EndP) OR Instring(',C,',UPPER(Ins),1,EndP) OR Instring(',C ',UPPER(Ins)&' ',1,EndP),2,0)
      ! More, need return type

      NameX=Instring(',NAME(''',UPPER(Ins),1,EndP)          !Carl 12/21/2006  a NAME('externalname') does no Mangling
 IF DbIt THEN db('NameX=' & NameX & ' EndP=' & EndP & '  ins=' & ins).
      if NameX                                              !did I find NAME('
         sName=sub(Ins,NameX+7,255)                         !now have: extname')
         NameX=instring(chr(39),sName,1)
         if NameX then sName=sub(sName,1,NameX-1).          !now have: extname
         Self.StoreName(sName,1)                            !Carl NAME() always keeps original Case
         Self.NoMangle = 2                                  !2=no @F on function with Name()
      end
    END
    Self.StartProc()
    IF ~Self.NoMangle THEN
      LOOP UNTIL Ins[Gn] = ')'  
 IF DbIt THEN DB('Ins=' & Ins) .
        DO GetSymbol
 IF DbIt THEN DB('GetSymbol=' & Symbol &'   SymbolP=' & SymbolP) .
        IF ~Symbol THEN BREAK .
!        IF INLIST( UPPER(Symbol),'SIGNED','UNSIGNED','BOOL')            !Some Clarion equated Longs
!             Symbol='LONG'
!
!        ELSIF INLIST( UPPER(Symbol),'HANDLE','HRESULT','DWORD','COLORREF','LPVOID','LPCVOID','PLONG', |
!                                    'HWND','HINSTANCE','HMODULE','HMENU','HDC','HICON','HCURSOR','HBRUSH','HBITMAP','HGDIOBJ','HFONT','HRGN','HGLOBAL','HPEN','HRESULT')
!             Symbol='LONG'                      !Some Common Windows LONGs, I hope these aren't a problem (could add a switch)
!
!        ELSIF INLIST( UPPER(Symbol),'DWORD','DWORD')
!             Symbol='ULONG'                     !this one could be equated more then one way?
!
!        ELSIF INLIST( UPPER(Symbol),'WORD','WORD')
!             Symbol='USHORT'                 !change some equated data types used in ABC classes
!
!        END

        !Trablate Equated type to Clarion
        EquQ:LabelType = UPPER(Symbol)
        GET(EquateTypeQ,EquQ:LabelType)
        IF ~ERRORCODE() and EquQ:ClaType<>EquQ:LabelType THEN           !Is this something like UNSIGNED EQUATE(LONG)?
            IF DbIt THEN DB('Equate Lookup ' & Symbol & ' ==> ' &  EquQ:ClaType ) .
            Symbol = EquQ:ClaType                                       !then the real type is LONG
        END                          
                                     ! 1 Bf      2 Bb      3 Bk  4 Bq    5 Br     6 Bw      7 Bi   8 Ba
        SymVal = INLIST(UPPER(Symbol),'FILE',   'BLOB',   'KEY','QUEUE','REPORT','WINDOW', 'VIEW','APPLICATION') 
        CASE UPPER(Symbol)
        OF 'INDEX' ; SymVal = 3 !INDEX same as KEY
        END
        
        TVal = INLIST(UPPER(Symbol),     'BYTE',    'SHORT',  'LONG',   'USHORT','ULONG', 'SREAL',  'REAL',   'DATE', 'TIME',   'DECIMAL',|
                                         'PDECIMAL','BFLOAT4','BFLOAT8','?',     'STRING','PSTRING','CSTRING','GROUP','BSTRING','ASTRING')
        CASE UPPER(Symbol) 
        OF 'ANY' ; TVal = 14    !ANY same as ?
        END 
        Self.StoreSym(SymVal,TVal,Symbol) ! 1         2         3         4        5        6         7         8       9         10
      END                                 !11        12        13        14       15       16        17        18      19         20
    END
    Self.EndProc
    RETURN Self.Hold

! Gn comes in pointing to last seperator;  Exits pointing to next seperator;  Symbol has type name from prototype
GetSymbol ROUTINE 
  LOOP                        !Remove leading spaces
    Gn += 1
  WHILE Ins[Gn]= ' '
  IF UPPER(SUB(Ins,Gn,6))='CONST ' THEN Gn += 6.    !10/22/17 Skip over CONST in (CONST *STRING S) so not a NAME Type

  IF Ins[Gn]='<<' THEN        !Found "<" at start of <Omit>
    Gn+= 1
    Self.IsOmitable = 1
    IF UPPER(SUB(Ins,Gn,6))='CONST '  THEN Gn += 6.    !10/22/17 Skip over CONST in (<CONST *LONG L>) so not a NAME Type
   ! IF UPPER(SUB(Ins,Gn,7))=' CONST ' THEN Gn += 7.   !10/22/17 fixed Clean Code to remove spaces after < so no need
  ELSE
    Self.IsOmitable = 0
  END
  IF Ins[Gn]='*' THEN         !Found *Address
    Self.IsAddress = 1
    Gn += 1
  ELSE
    Self.IsAddress = 0
  END
  
  SymbolP = 1
  LOOP UNTIL INSTRING(Ins[Gn],',= >)[')     !Find Symbol NAME in (TYPE SymNAME)
    Symbol[SymbolP] = Ins[Gn]
    Gn += 1
    SymbolP += 1
  END
  Symbol[SymbolP] = '<0>' 

  Self.Adims = 0
  IF Ins[Gn]='[' THEN       !Found Array[] 
    Self.Adims += 1         !10/22/17 the "[" gets you 1 "A"
    LOOP
      !10/22/17 Self.Adims += 1     ![space] was being counted so [ ] => AA
      Gn+=1  
      IF Ins[Gn] = ',' THEN Self.Adims += 1.    !10/22/17 each "," gets you +1 "A"
    UNTIL Ins[Gn] = ']'
  END
  LOOP UNTIL INSTRING(Ins[Gn],',)')
    Gn += 1
  END

!ExpConverter.StoreName   PROCEDURE(string nam,byte PreserveCase=0)
!  CODE
!    Self.Hold = Clip(choose(~CaseProcName,UPPER(Nam),Nam))         !Carl my cwHH prototypes were UPLOW

ExpConverter.StoreSym    PROCEDURE(Byte EVal,Byte TVal,string symbol)
  CODE
    !    IF DbIt THEN DB('StoreSym Symbol=' & Symbol & ' TVal=' & TVal & 'EVal=' & EVal ).
    IF EVal THEN                              !  1   2   3   4   5   6   7   8
      Self.Hold = Self.Hold & 'B' & CHOOSE(EVal,'f','b','k','q','r','w','i','a')
    ELSIF TVal THEN
      DO Preamble                        ! 1    2   3    4    5    6    7    8     9    10
      Self.Hold = Self.Hold & CHOOSE(TVal,'Uc','s','l' ,'Us','Ul','f' ,'d' ,'bd' ,'bt','e',|
                                          'p','b4','b8','u' ,'sb','sp','',  '',   'sw','sa')
      CASE UPPER(Symbol)
      OF 'CSTRING'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'c','sc')
      OF 'GROUP'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'v','g')
      END
    ELSE
      !This is some Named Symbol
      Self.Hold = Self.Hold & LEN(Symbol) & choose(~CaseSelfName,UPPER(Symbol),Symbol)      !Carl If Self is preserved then do all the parms
      if ~instring(' ' & CLIP(UPPER(Symbol)) & ' ',NamedSymbolList,1)
          NamedSymbolList=clip(NamedSymbolList) &' '& Symbol                    !A list so I can spot bad symbol names or missed equates
      end
    END
    IF DbIt THEN DB('StoreSym Self.Hold=' & Self.Hold ) .

Preamble ROUTINE
  IF Self.IsAddress OR Self.Adims THEN
    Self.Hold = Self.Hold & CHOOSE(Self.IsOmitable,'P','R')
  ELSIF Self.IsOmitable THEN
    Self.Hold = Self.Hold & 'O'
  END
  IF Self.Adims THEN
    Self.Hold = Self.Hold & ALL('A',Self.Adims)
  END

ExpConverter.StartProc     PROCEDURE
  CODE
    IF Self.NoMangle < 2 THEN
      Self.Hold = Self.Hold & '@F'
    END

CConverter.StoreSym    PROCEDURE(Byte EVal, Byte TVal,string symbol)
I UNSIGNED,AUTO
  CODE
  
    IF Self.Hold[LEN(Self.Hold)]<>'(' THEN
      Self.Hold = Self.Hold & ','
    END
    IF EVal THEN
      Self.Hold = Self.Hold & CHOOSE(EVal,'void *','void *,unsigned bnum','void *','void *','unsigned','unsigned','void *','unsigned','**MORE**')
    ELSE
      IF TVal THEN
        LOOP I = 1 TO Self.Adims
          Self.Hold = Self.Hold & 'unsigned dim' & I & ','
          Self.IsAddress = 1
        END
        Self.Hold = Self.Hold & CHOOSE(TVal,'byte','short','long',|
         'unsigned short','unsigned long','float','double','long','long', '?')
        I = LEN(Self.Hold)
        IF Self.Hold[I] = '?' THEN
          CASE UPPER(Symbol)
          OF 'DECIMAL'
          OROF 'PDECIMAL'
            Self.Hold[i] = '<0>'
            Self.Hold = Self.Hold & 'unsigned prec,'
            DO LenChar
          OF 'STRING'
            IF ~Self.IsAddress THEN
               !Clarion passes STRING with Cla$PushString so no simple C prototype
              Self.Hold[i] = '<0>'
              Self.Hold = Self.Hold & 'n/a' 
              RETURN
            END
          OROF 'PSTRING'
          OROF 'CSTRING'
            Self.Hold[i] = '<0>'
            DO LenChar
          OF 'GROUP'
            Self.Hold[i] = '<0>'
            DO LenChar
            IF ~Self.IsRaw THEN
              Self.Hold = Self.Hold & ',void *tpe'
            END
          END
        ELSE
          IF Self.IsAddress THEN
            Self.Hold = Self.Hold & CHOOSE( Self.IsOmitable, '*', '&' )
          ELSIF Self.IsOmitable THEN
            Self.Hold = Self.Hold & ',unsigned char omit'
          END
        END
      ELSE
        Self.Hold = Self.Hold & Symbol & '&'
      END
    END

LenChar ROUTINE
  IF ~Self.IsRaw THEN
    Self.Hold = Self.Hold & 'unsigned len,'
  END
  Self.Hold = Self.Hold & 'char *'

CConverter.StartProc     PROCEDURE
  CODE
    Self.Hold = Self.Hold & '('

CConverter.EndProc     PROCEDURE
  CODE
    Self.Hold = Self.Hold & ')'

FlattenProtoTypes Procedure(*CSTRING pPrototypes)
LOC:Prototypes CString(MaxProto+1)
Ndx2           Short
L              Short
IsComment      Byte
  CODE
  Ndx  = 1
  Ndx2 = Instring('|',pPrototypes,1,1)      !Carl Note, this could not work right. It could find a | in 'quotes' or !comments. I wrote better flatten in CWA_WindowText
  If NOT Ndx2 > 2 then return.

  Clear(LOC:Prototypes)
  L = Len(pPrototypes)
  Loop
    LOC:ProtoTypes = Clip(LOC:ProtoTypes) & pProtoTypes[Ndx : Ndx2 - Choose(Ndx2=L,0,1)]
    If Ndx2 >= L then break.
    Loop Ndx = Ndx2+1 to L
      Case pPrototypes[Ndx]
      of '!'
        IsComment = TRUE
      of '<13>' orof '<10>'
        IsComment = FALSE
        Cycle
      of ' '
        Cycle
      else
        If NOT IsComment
          Break
        end
      end
    end
    Ndx2 = Instring('|',pPrototypes,1,Ndx)
    If NOT Ndx2
      Ndx2 = L
    end
  end
  pProtoTypes = LOC:ProtoTypes
  RETURN 
  
Tabs2Spaces PROCEDURE(*CSTRING pStr)
X LONG 
    CODE
    LOOP X=1 TO LEN(pStr)
        IF VAL(pStr[X])=9 THEN pStr[X]=''.
    END
    RETURN 


Load_EquateTypeQ    procedure()               !Load EquateTypeQ
!    EquateTypeQ     QUEUE,PRE(EquQ)
!    EquQ:LabelType          string(64)          !The type that the developer called it
!    EquQ:ClaType            string(64)          !aka Clarion type, the longest is probably APPLICATION
!                    END
    !Make these load from a file, or allow some to load from a file
ChNdx    long
    code
    !First load all the Clarion types so I have them in the queue, makes it easy to know when I am done looking up because I found a Clarion type
    loop ChNdx = 1 to 99
         EquQ:LabelType=choose(ChNdx, |
                             'FILE',    'BLOB',   'KEY',    'QUEUE', 'REPORT','WINDOW',  'VIEW','APPLICATION', |
                             'BYTE',    'SHORT',  'LONG',   'USHORT','ULONG', 'SREAL',   'REAL',   'DATE', 'TIME',   'DECIMAL',|
                             'PDECIMAL','BFLOAT4','BFLOAT8','STRING','PSTRING','CSTRING','GROUP','BSTRING','ASTRING', |
                             '')                                                             !Last Blank ends the loop
         if ~EquQ:LabelType then break.
         EquQ:ClaType = EquQ:LabelType                 !Label=Cla
         add(EquateTypeQ)
    end

    EquQ:ClaType = 'LONG'                           !Add the Windows API long types
    loop ChNdx = 1 to 99
         EquQ:LabelType=choose(ChNdx, |
                             'SIGNED','UNSIGNED','BOOL' , |                                        !Clarion types, DO NOT remove these
                             'HANDLE','HRESULT','DWORD','COLORREF','LPVOID','LPCVOID','PLONG',|
                             'HWND','HINSTANCE','HMODULE','HMENU','HDC','HICON','HCURSOR','HBRUSH', |
                             'HBITMAP','HGDIOBJ','HFONT','HRGN','HGLOBAL','HPEN', |
                             '')                                                           !Last Blank ends the loop
         if ~EquQ:LabelType then break.
         add(EquateTypeQ)
    end

!    EquQ:LabelType='DWORD' ;  EquQ:ClaType='ULONG' ;  add(EquateTypeQ)        !This might not always be so maybe I shouldn't and make user Equate
    EquQ:LabelType='WORD' ;  EquQ:ClaType='USHORT' ;  add(EquateTypeQ)

    loop ChNdx = 1 to records(EquateTypeQ)                  !make sure they are all UPPER as it is assumed by other code
         GET(EquateTypeQ,ChNdx)
         EquQ:LabelType = upper(EquQ:LabelType)
         EquQ:ClaType   = upper(EquQ:ClaType)
         PUT(EquateTypeQ)
         ! IF DbIt THEN DB(EquQ:LabelType[1:32] & EquQ:ClaType).
    end
    SORT(EquateTypeQ,EquQ:LabelType)
    return

LoadWindowTxt   procedure(*CSTRING Protoz, *STRING Rulez, *STRING Aboutz, *STRING Cw71z)  !Move big strings to bottom
    CODE
    Protoz ='StringPass  PROCEDURE(STRING S1, *STRING S2, <<STRING S3>, <<*STRING S4>)' &|
     '<13,10>PassArray   PROCEDURE(*LONG[,] A2dim) ' &|
     '<13,10>CityStZip   PROCEDURE(STRING CSZ, *STRING City,*STRING State,*STRING Zip,BYTE ' &|
     'Plus4=1),STRING' &|
     '<13,10>NextTab     PROCEDURE(LONG SheetFEQ, BOOL Wrap=0, <<*LONG TabFEQ>),LONG' &|
     '<13,10>' &|
     '<13,10>MyLong  LONG' &|
     '<13,10>!MyQue   QUEUE,TYPE  !Type not in EXP' &|
     '<13,10>!MyGroup GROUP,TYPE' &|
     '<13,10>NormalQandG  PROCEDURE(QUEUE Q1, GROUP G1)' &|
     '<13,10>NamedQandG   PROCEDURE(MyQue Q1, MyGroup G1)  !Named Queue / Group' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>!Example of Class with INTERFACE. ' &|
     '<13,10>' &|
     '<13,10>HlpEngCls   CLASS,IMPLEMENTS(HelpEngine) ,EXPORT ' &|
     '<13,10>Match              PROCEDURE(CONST *CSTRING),BOOL,VIRTUAL' &|
     '<13,10>HelpCmd            PROCEDURE(UNSIGNED, <<CONST *CSTRING>, UNSIGNED, LONG=0),BOOL,PROC,V' &|
     'IRTUAL' &|
     '<13,10>            END   ' &|
     '<13,10>' &|
     '<13,10>!An INTERFACE after a CLASS in this Window is Implemented, there is no need for ' &|
     'IMPLEMENTED()' &|
     '<13,10>' &|
     '<13,10>HelpEngine       INTERFACE' &|
     '<13,10>Match              PROCEDURE(CONST *CSTRING),BOOL' &|
     '<13,10>HelpCmd            PROCEDURE(UNSIGNED, <<CONST *CSTRING>, UNSIGNED, LONG=0),BOOL,PROC' &|
     '<13,10>                 END'


    Cw71z = 'You don''t have to use a tool like this to mangle prototypes ' &|
     'and edit an EXP file, but IMO most developers still prefer to have an EXP file to see exports. ' &|
     'Also there is no _flag_ for EXPORT(_flag_) so to have one INC file allow both Export and External requires using Omit/Compile. ' &|
     '<13,10>' &|
     '<13,10>CW 7.1 added Linker Directive EXPORT to the Clarion ' &|
     'Language. This is similar to the Microsoft C/C++ __declspec(dllexport). ' &|
     '<13,10>' &|
     '<13,10>Simply add EXPORT to any PROCEDUE, CLASS, DATA, or FILE and ' &|
     'the linker will write the mangled exports to the DLL / LIB file. This ' &|
     'is especially helpful with CLASS, FILEs, QUEUE and GROUP as they generate ' &|
     'multiple lines. EXPORT does NOT update the EXP, but you can view the ' &|
     'mangled exports using LibMaker, and export an EXP from there with the mangled exports. ' &|
     '<13,10>' &|
     '<13,10>Examples:' &|
     '<13,10>   ReverseName(*STRING _Name),STRING     ,EXPORT' &|
     '<13,10>   ExpQue1  QUEUE,TYPE                   ,EXPORT' &|
     '<13,10>   HlpEng1  CLASS,IMPLEMENTS(HelpEngine) ,EXPORT' &|
     '<13,10>   AscFile  FILE,DRIVER(),PRE(),NAME()   ,EXPORT' &|
     '<13,10>   GlobalVariable  LONG                  ,EXPORT' &|
     '<13,10>' &|
     '<13,10>Another feature copied from the Microsoft .DEF file specification ' &|
     'is the ability to generate a "Substitution Export" by adding to the ' &|
     'EXP file a line like:' &|
     '<13,10>   SubstituteName=MangledName  @?' &|
     '<13,10>   _StringPass=STRINGPASS@FsbRsbOsbPsb   @?' &|
     '<13,10>' &|
     '<13,10>If you have "StringPass() ,EXPORT" then you get both the Mangled ' &|
     'name and your Substition name. If you only desire the unmangled substitutio' &|
     'n name is is best to use NAME(''_StringPass''),EXPORT so you do not ' &|
     'need to edit the EXP file.' &|
     '<13,10>' &|
     '<13,10>--------------------------------------------------------------------' &|
     '<13,10>http://clarionsharp.com/blog/71-preliminary-release-notes-and-a-happ' &|
     'y-thanksgiving' &|
     '<13,10>' &|
     '<13,10>FEATURE: ' &|
     '<13,10>New attribute for procedure prototypes and static variables ' &|
     'declarations: EXPORT. The EXPORT attribute forces the variable or ' &|
     'procedure to be added to the export list of the DLL even if it is ' &|
     'not listed in the EXPORTS section of the EXP file.' &|
     '<13,10>' &|
     '<13,10>FEATURE: ' &|
     '<13,10>Ability to use substitution of exported name in the EXPORTS ' &|
     'section of the EXP file. The entry in the EXPORTS section can have ' &|
     'format [<<substitution name>=]<<external name> @? <13,10>'

 Aboutz = 'This is based on Pro2Exp shipped as a Source Example that did only one procedure.' &|
     '<13,10>' &|
     '<13,10>Lee G. White wrote Mangle.exe that Jeff used as his starting ' &|
     'point' &|
     '<13,10>' &|
     '<13,10>Jeff Slarve and Lee White in 2005 made many enhancements:' &|
     '<13,10> - TEXT control with multiple prototype mangling' &|
     '<13,10> - Flatten Prototypes to handle multi-line declarations ' &|
     '<13,10> - User settings to format the ouput' &|
     '<13,10>' &|
     '<13,10>My enhancments:' &|
     '<13,10> - New style MAP Procedure declarations starting in Column ' &|
     '1' &|
     '<13,10> - BSTRING as "sw", ASTRING as "sa"' &|
     '<13,10> - CONST Constant in prototype, no effect on mangle but not ' &|
     'a named type. PROC handled' &|
     '<13,10> - CLASS Exports, paste in entire class into window and does ' &|
     'export' &|
     '<13,10> - CLASS INTERFACE Exports Interfaces _@_Interface_@_Class' &|
     '<13,10> - NAME() is used in EXP, no mangle' &|
     '<13,10> - EQUATE''s can be pasted in for type Conversion e.g. HDC ' &|
     'EQUATE(LONG)' &|
     '<13,10> - LONG equates for SIGNED UNSIGNED BOOL' &|
     '<13,10> - GROUP and QUEUE exports, but not FILE' &|
     '<13,10> - FILE exports, also explained on the Rules tab' &|
     '<13,10> - API equates for HANDLE HRESULT DWORD WORD HWND HINSTANCE ' &|
     'HMODULE HMENU HDC HICON HCURSOR HBRUSH HBITMAP HGDIOBJ HFONT HRGN ' &|
     'HGLOBAL HPEN  LPVOID LPCVOID PLONG '

 Rulez = 'These Clarion Mangle rules are to aid in understanding the mangling process, and to help in reverse engineer a Mangled Export back to a Prototype. ' &|
     '<13,10>' &|
     '<13,10>Generally a single parameter stars with optional prepend UPPERCASE byte(s) (BROPA) and ends with a single lowercase letter. There can be 2 lowercase letters with "b" or "s" e.g. ' &|
     '"sb" is a STRING, except a SHORT is an "s" alone.' &|
     '<13,10>' &|
     '<13,10>Procedure mangled format is: "ProcedureName(UPPER)" + "@F" + Mangled (Parameters). e.g. GetExt(BYTE,LONG),STRING => GETEXT@FUcl' &|
     '<13,10>' &|
     '<13,10>Procedure name exports are case sensitive under Windows. Clarion works around this by UPPERing the Procedure Names, but the mangling codes for parameter data are mixed case so case ' &|
     'sensitive. ' &|
     '<13,10>' &|
     '<13,10>E.g. "PassAstring2opt(<<*Astring A>, *USHORT U)" mangles as "PASSASTRING2OPT@FPsaRUs" note that the Procedure name was UPPER CASED but the (<<*Astring A>, *USHORT U) is mixed case ' &|
     '"@FPsaRUs".' &|
     '<13,10>' &|
     '<13,10>Binary Objects - Always by Address and will never have Prepend "R". These can be omittable (e.g. <<Report>) but that does NOT change the mangle and prepend "P".' &|
     '<13,10>' &|
     '<13,10>Object         Mangle           C Proto' &|
     '<13,10>-----------    ------           -------' &|
     '<13,10>FILE           Bf               void *' &|
     '<13,10>BLOB           Bb               void *,unsigned bnum' &|
     '<13,10>KEY            Bk               void *' &|
     '<13,10>INDEX          Bk               void *' &|
     '<13,10>QUEUE          Bq               void *' &|
     '<13,10>REPORT         Br               unsigned' &|
     '<13,10>WINDOW         Bw               unsigned' &|
     '<13,10>VIEW           Bi               void *' &|
     '<13,10>APPLICATION    Ba               unsigned  - Compiler rejects use WINDOW' &|
     '<13,10>' &|
     '<13,10>Prepend one of these (Except "B" types above):' &|
     '<13,10>----------------------------------------------' &|
     '<13,10>  R = By *Address Required      e.g. *STRING   => Rsb' &|
     '<13,10>  P = By *Address <<*Omittable>  e.g. <<*STRING> => Psb  ' &|
     '<13,10>  O = By Value    <<Omittable>   e.g. <<STRING>  => Osb' &|
     '<13,10>    = By Value Required         e.g. STRING    => sb   Nothing Prepended' &|
     '<13,10>' &|
     '<13,10>After above can also Prepend an Array Declarator:' &|
     '<13,10>-------------------------------------------------' &|
     '<13,10>  A = Array, one "A" per Dim    e.g. *LONG[,]  => RAAl  or  <<*LONG[,]> => PAAl' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>Type         Mangle   RAW     C Proto' &|
     '<13,10>---------    ------   ---     -----------' &|
     '<13,10>BYTE         Uc               byte' &|
     '<13,10>SHORT        s                short' &|
     '<13,10>USHORT       Us               unsigned short' &|
     '<13,10>LONG         l                long' &|
     '<13,10>ULONG        Ul               unsigned long' &|
     '<13,10>DATE         bd               long' &|
     '<13,10>TIME         bt               long' &|
     '<13,10>DECIMAL      e                char *       w/o RAW: unsigned len,char *' &|
     '<13,10>PDECIMAL     p                char *       w/o RAW: unsigned len,char *' &|
     '<13,10>SREAL        f                float' &|
     '<13,10>REAL         d                double' &|
     '<13,10>BFLOAT4      b4               n/a' &|
     '<13,10>BFLOAT8      b8               n/a' &|
     '<13,10>?            u                n/a' &|
     '<13,10>ANY          u                n/a' &|
     '<13,10>*?           Ru               n/a' &|
     '<13,10>*ANY         Ru' &|
     '<13,10>STRING       sb               n/a          STRING not passed by address N/A' &|
     '<13,10>GROUP        g        v       char *       w/o RAW: unsigned len,char *,void *tpe' &|
     '<13,10>CSTRING      sc       c       char *       w/o RAW: unsigned len,char *' &|
     '<13,10>PSTRING      sp               char *       w/o RAW: unsigned len,char *' &|
     '<13,10>ASTRING      sa' &|
     '<13,10>BSTRING      sw' &|
     '<13,10>INT          i                32-bit Integer seen only in C Prototypes' &|
     '<13,10>PROCEDURE    Fmangle_         TYPE procedure => "F" + "prototype mangle" + "_"' &|
     '<13,10>' &|
     '<13,10>Named Types:' &|
     '<13,10>NAMED_TYPES  ##TYPENAME       ## = Length of the Name + Type Name UPPER e.g. NAMED_TYPES => 10NAMED_TYPES' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>Examples       Mangle  Raw' &|
     '<13,10>------------   ------  ---' &|
     '<13,10>*DECIMAL       Re' &|
     '<13,10>*PDECIMAL      Rp' &|
     '<13,10>' &|
     '<13,10>STRING         sb' &|
     '<13,10><<STRING>       Osb' &|
     '<13,10>*STRING        Rsb' &|
     '<13,10><<*STRING>      Psb' &|
     '<13,10>' &|
     '<13,10>PSTRING        sp' &|
     '<13,10>*PSTRING       Rsp' &|
     '<13,10><<*PSTRING>     Psp' &|
     '<13,10>' &|
     '<13,10>CSTRING        sc       c' &|
     '<13,10>*CSTRING       Rsc' &|
     '<13,10><<*CSTRING>     Psc' &|
     '<13,10>' &|
     '<13,10>GROUP          g        v' &|
     '<13,10><<GROUP>        Og       Ov' &|
     '<13,10>*GROUP         Rg       Rv' &|
     '<13,10><<*GROUP>       Pg       Pv' &|
     '<13,10>' &|
     '<13,10>BSTRING        sw' &|
     '<13,10><<BSTRING>      Osw' &|
     '<13,10>*BSTRING       Rsw' &|
     '<13,10><<*BSTRING>     Psw' &|
     '<13,10>' &|
     '<13,10>ASTRING        sa' &|
     '<13,10><<ASTRING>      Osa' &|
     '<13,10>*ASTRING       Rsa' &|
     '<13,10><<*ASTRING>     Psa' &|
     '<13,10>' &|
     '<13,10>Miscellaneous -------------------' &|
     '<13,10>CONST has no affect on Clarion Mangle, it does affect C mangling, see below.' &|
     '<13,10>' &|
     '<13,10>GROUP  Exports 2 Symbols: $GROUPNAME and TYPE$GROUPNAME' &|
     '<13,10>QUEUE  Exports 3 Symbols: $QUEUENAME and TYPE$QUEUENAME and TCB$QUEUENAME' &|
     '<13,10>CLASS  Exports 2 Symbols: VMT$ClassName and TYPE$ClassName' &|
     '<13,10>              +1 /method: MethodName + @F + ##ClassName + Method Parameters Mangled ' &|
     '<13,10>                          e.g. HlpEngCls.Match PROCEDURE(CONST *CSTRING) => MATCH@F9HLPENGCLSRsc   ' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>FILE Exports: ' &|
     '<13,10>------------------------' &|
     '<13,10>This tool does not do FILE Exports, but here are the rules.' &|
     '<13,10>    FILE exports 1 $FILENAME symbol' &|
     '<13,10>      +1 Symbol  for each KEY, INDEX, BLOB, MEMO: FILENAME$PRE:NAME' &|
     '<13,10>      +2 Symbols for Record: FILENAME$PRE:RECORD and FILENAME$TYPE$PRE:RECORD' &|
     '<13,10>    ' &|
     '<13,10>    Example:' &|
     '<13,10>    ---------' &|
     '<13,10>    DataFile              FILE,DRIVER(''TOPSPEED''),PRE(DaF),CREATE,BINDABLE ,EXPORT !Data for Popup choices from POPUP.TXT' &|
     '<13,10>    Key1                     KEY(DF:Type,DF:Seq),NOCASE,OPT,PRIMARY                   ' &|
     '<13,10>    Index1                   INDEX(DF:Item)' &|
     '<13,10>    Blob1                    BLOB' &|
     '<13,10>    Memo1                    MEMO(5000)' &|
     '<13,10>    Record                   RECORD,PRE()' &|
     '<13,10>    ' &|
     '<13,10>    Exports:' &|
     '<13,10>    ------------------' &|
     '<13,10>    $DATAFILE' &|
     '<13,10>    DATAFILE$DAF:BLOB1' &|
     '<13,10>    DATAFILE$DAF:INDEX1' &|
     '<13,10>    DATAFILE$DAF:KEY1' &|
     '<13,10>    DATAFILE$DAF:MEMO1' &|
     '<13,10>    DATAFILE$DAF:RECORD' &|
     '<13,10>    DATAFILE$TYPE$DAF:RECORD' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>Value-Types:' &|
     '<13,10>   BYTE  SHORT  USHORT  LONG  ULONG  SREAL  REAL  DATE  TIME  STRING  BSTRING  ASTRING' &|
     '<13,10>' &|
     '<13,10>Address "R" Types (or "P" if <<omittble>):' &|
     '<13,10>   *BYTE  *SHORT  *USHORT  *LONG  *ULONG  *SREAL  *REAL  *BFLOAT4  *BFLOAT8' &|
     '<13,10>   *DECIMAL  *PDECIMAL   *DATE   *TIME  *STRING  *PSTRING  *CSTRING  *GROUP  *BSTRING  *ASTRING' &|
     '<13,10>' &|
     '<13,10>Entity-parameters "B" Types:' &|
     '<13,10>   FILE   VIEW   KEY   INDEX   QUEUE   WINDOW   REPORT   BLOB' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>Other ways to get mangling:' &|
     '<13,10>---------------------------' &|
     '<13,10>The best way is to use EXPORT and then look in LibMaker.' &|
     '<13,10>You can prototype the import side and the next build in the MAP will show all the missing imports complete with mangling.' &|
     '<13,10>Templates off ' &|
     '<13,10>' &|
     '<13,10>' &|
     '<13,10>TopSpeed C++ Mangling from the Programmers Guide' &|
     '<13,10>These may behelpful decoding RUN DLL Exports.' &|
     '<13,10>---------------------------------------------------' &|
     '<13,10>    Type Encode          Modifiers      Declarators' &|
     '<13,10>    ---- ------          - --------     - ---------' &|
     '<13,10>    c    char            C const        P pointer' &|
     '<13,10>    d    double          S signed       R reference' &|
     '<13,10>    e    ellipse         U unsigned     F function  F<<args>_<<type> Function taking <<Args> Returning <<Type>' &|
     '<13,10>    f    float           V volatile     A array     A<<size>_<<type> Array of specified <<Size> of <<Type>' &|
     '<13,10>    i    int 16/32                      M Member    M<<type-1><<type-2> Pointer to member of class <<type-1> of <<type-2>' &|
     '<13,10>    l    long' &|
     '<13,10>    s    short' &|
     '<13,10>    r    long double' &|
     '<13,10>    v    void' &|
     '<13,10>' &|
     '<13,10>    Named types are encoded: _ + (Length of Name) + Name + _   e.g.  _7Complex_' & |
     '<13,10>'

                                
    RETURN                                 