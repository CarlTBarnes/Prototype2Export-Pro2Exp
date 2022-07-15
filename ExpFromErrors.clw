    PROGRAM
    INCLUDE('TplEqu.CLW'),ONCE
    INCLUDE('KeyCodes.CLW'),ONCE
!---------------------------------------------------------------------------------------
! ExpFromErrors by Carl Barnes - July 2022
! Old code with numerous Export errors due to missing EXP lines, all hand code.
! Found it useful to be able to clean up those MAP Errors and view them sorted,
! plus paste them into EXP to find problem.
! This will also let you paste an EXP and view sorted by Class.
! Parse EXP Q tab allows column sorting.
!---------------------------------------------------------------------------------------
! History
! 22-July-12    Original
!---------------------------------------------------------------------------------------
! TODO
! [ ]
!---------------------------------------------------------------------------------------
    map
EXPfromErrorsMain  PROCEDURE()
DB    PROCEDURE(STRING DebugMsg)
      module('api')
        OutputDebugString(*CSTRING cMsg),PASCAL,DLL(1),RAW,NAME('OutputDebugStringA')
        DebugBreak(),PASCAL,DLL(1)  !If running under Debug forces the debugger to popup
        GetLastError(),LONG,PASCAL,DLL(1)
      end
    end

SortClass_Exp CLASS,TYPE,PRIVATE  !06/13/22 rename CBSortClass with _WnPv
QRef        &QUEUE
FEQ         LONG
ColumnNow   SHORT
ColumnLast  SHORT
QFieldNow   SHORT
QFieldLast  SHORT
Who1st      STRING(128)
Who2nd      STRING(129)
Init        PROCEDURE(QUEUE ListQueue, LONG ListFEQ, SHORT SortColumnNow=0)
SetSortCol  PROCEDURE(SHORT SortColNow)
HeaderPressed PROCEDURE(SHORT ForceSortByColumn=0) !Call in OF EVENT:HeaderPressed for LIST
    END

    code
    EXPfromErrorsMain()
    return

EXPfromErrorsMain        procedure
LenIn       LONG
MaxTxtSize  EQUATE(164000)   !Had an EXP with 125k so make this big. Seems like >64k can have RTL trouble
InTxt       STRING(164000)   !
InEXP       STRING(164000)   !cut to EXP
OtEXP       STRING(164000)   !the Concat will go way faster if carefully add
SortByLine   BYTE            !Keep the EXP in Line No Order
BtnNo       BYTE

InputIsEXP  BYTE           !Input are EXP lines not MAP Errors

ParseQ  QUEUE,PRE(ParQ)
Parent      STRING(64)     !ParQ:Parent            !Col 1   Class or File or Queue Parent for Sort
AtF_Dollar  STRING(1)      !ParQ:AtF_Dollar        !    2
Symbol      STRING(256)    !ParQ:Symbol            !    3
SymbolTip   STRING(256)    !ParQ:SymbolTip
LenSymbol   LONG           !ParQ:LenSymbol         !    4
LineNo      LONG           !ParQ:LineNo            !    5
ErrLine     STRING(1024)   !ParQ:ErrLine           !    6
        END
COL_ParQ:Parent    EQUATE(1)
COL_ParQ:LineNo    EQUATE(4)

LenSymGrp   GROUP,PRE(LenSym)      !Total Symbols and Statistics to help with settings in Pro2Exp
Count           LONG          !LenSym:Count
TotLength       LONG          !LenSym:TotLength
AvgLength       LONG          !LenSym:AvgLength
MaxLength       LONG          !LenSym:MaxLength
MinLength       LONG          !LenSym:MinLength
MaxSymbol       STRING(256)   !LenSym:MaxSymbol
MinSymbol       STRING(256)   !LenSym:MinSymbol
MaxLineNo       LONG          !LenSym:MaxLineNo
MinLineNo       LONG          !LenSym:MinLineNo
StdMean         REAL          !LenSym:StdMean       Average as a REAL
StdDevSum       REAL          !LenSym:StdDevSum
StdDeviaton     REAL          !LenSym:StdDeviaton
            END

Window WINDOW('EXP From MAP Errors'),AT(,,360,200),CENTER,GRAY,SYSTEM,ICON('ExpFromErrors.ico'), |
            FONT('Segoe UI',9),RESIZE
        SHEET,AT(1,1),FULL,USE(?SHEET1)
            TAB(' &In MAP Error Lines '),USE(?TabInTxt)
                PROMPT('Paste MAP Unresolved External Error Lines'),AT(5,33),USE(?InTxt:Prompt)
                TEXT,AT(4,45),FULL,USE(InTxt),HVSCROLL,FONT('Consolas')
                BUTTON('P&aste && Parse'),AT(6,18,,12),USE(?PasteBtn),SKIP
                BUTTON('&Parse Symbols'),AT(72,18,,12),USE(?ParseBtn)
                CHECK('Sort by Line'),AT(252,20),USE(SortByLine),TIP('Leave the EXP in original line' & |
                        ' order<13,10>Do not try to sort by Class or File Parent')
                CHECK('Input is EXP, not MAP Errors'),AT(145,20),USE(InputIsEXP),TIP('The text below' & |
                        ' is EXP not MAP Errors<13,10>Allows using this tool to sort and group')
                PROMPT('Or Paste EXP and Check Above'),AT(145,33),USE(?FYI_CanPasteEXP)
                BUTTON('&Copy'),AT(321,29,,11),USE(?CopyInpBtn),SKIP,TIP('Copy below to clipboard')
            END
            TAB(' &Out EXP Lines '),USE(?TabOtEXP)
                BUTTON('&Copy'),AT(6,18,,12),USE(?CopyExpBtn),SKIP,TIP('Copy EXP lines')
                BUTTON('Align @?'),AT(44,18,,12),USE(?AlignAtBtn),SKIP
                CHECK('Sort by Line (Not Class)'),AT(97,19),USE(SortByLine,,?SortByLine_Out), |
                        TIP('Leave the EXP in original order, do not try to sort by Class')
                STRING('Lines...'),AT(201,15),USE(?Stats_Lines),TRN
                STRING('Bytes...'),AT(201,23),USE(?Stats_Bytes),TRN
                TEXT,AT(2,34),FULL,USE(OtEXP),HVSCROLL,FONT('Consolas')
            END
            TAB(' Parse EXP Q '),USE(?TabParseQ)
                LIST,AT(2,20),FULL,USE(?LIST:ParseQ),HVSCROLL,VCR,FROM(ParseQ),FORMAT('80L(2)|MF~Par' & |
                        'ent - Class / File~@s64@15C|MF~F $~@s1@150L(2)|FMP~Symbol~@s255@22R(2)|M~Le' & |
                        'n~C(0)@n_4@Q''Symbol Length''22R(2)|M~Line~C(0)@n_4@200L(2)|M~Input Line~')
            END
            TAB(' Parse OU '),USE(?TabBoth),HIDE,TIP('Over / Under Parse Compare')
                TEXT,AT(2,18,,100),FULL,USE(InTxt,, ?InTxtOU),HVSCROLL,FONT('Consolas')
                TEXT,AT(2,122),FULL,USE(InEXP,, ?InEXPOU),HVSCROLL,FONT('Consolas')
            END
            TAB(' Parse SS '),USE(?TabBothSBS),TIP('Side-by-Side Parse Compare')
                TEXT,AT(2,18,272),FULL,USE(InTxt,, ?InTxtSBS),HVSCROLL,FONT('Consolas')
                TEXT,AT(280,18),FULL,USE(InEXP,, ?InEXPSBS),HVSCROLL,FONT('Consolas')
            END
            TAB(' Stats '),USE(?TabStats)
                PROMPT('I wanted to know how big of Symbols are created so added these statistics. ' & |
                        '<0Dh,0Ah>This helps size variables and controls in my tools. It''s kind of ' & |
                        'overkill.'),AT(11,18,241,18),USE(?FYI_Stats)
                ENTRY(@n10),AT(12,40,50,10),USE(LenSym:Count),RIGHT,READONLY
                PROMPT('Count       '),AT(71,40),USE(?Stat1:Prompt1)
                ENTRY(@n10),AT(12,52,50,10),USE(LenSym:TotLength),RIGHT,READONLY
                PROMPT('Total Length  '),AT(71,52),USE(?Stat2:Prompt1)
                ENTRY(@n10),AT(12,64,50,10),USE(LenSym:AvgLength),RIGHT,READONLY
                PROMPT('Average       '),AT(71,64),USE(?Stat3:Prompt1)
                ENTRY(@n10),AT(12,76,50,10),USE(LenSym:MaxLength),RIGHT,READONLY
                PROMPT('Max Length  '),AT(71,76),USE(?Stat4:Prompt1)
                ENTRY(@n10),AT(12,88,50,10),USE(LenSym:MinLength),RIGHT,READONLY
                PROMPT('Min Length  '),AT(71,88),USE(?Stat5:Prompt1)
                ENTRY(@n11.3),AT(12,100,50,10),USE(LenSym:StdMean),RIGHT,READONLY
                PROMPT('Std Dev Mean'),AT(71,100),USE(?Stat6:Prompt1)
                ENTRY(@n10),AT(12,112,50,10),USE(LenSym:StdDevSum),RIGHT,READONLY
                PROMPT('Std Dev Sum '),AT(71,112),USE(?Stat7:Prompt1)
                ENTRY(@n11.3),AT(12,124,50,10),USE(LenSym:StdDeviaton),RIGHT,TIP('About 68% of data ' & |
                        'falls<13,10>within +/- 1 Std Deviation of the Mean<13,10>95% within +/- 2 S' & |
                        'td Deviations of the Mean'),READONLY
                PROMPT('Std Deviation'),AT(71,124),USE(?Stat8:Prompt1)
                ENTRY(@s4),AT(122,76,18,10),USE(LenSym:MaxLineNo),TIP('Line number of Max Symbol'), |
                        READONLY
                ENTRY(@s4),AT(122,88,18,10),USE(LenSym:MinLineNo),TIP('Line number of Min Symbol'), |
                        READONLY
                ENTRY(@s255),AT(144,76,,10),FULL,USE(LenSym:MaxSymbol),TIP('Longest Symbol'),READONLY
                ENTRY(@s255),AT(144,88,,10),FULL,USE(LenSym:MinSymbol),TIP('Shorest Symbol'),READONLY
            END
        END
        BUTTON('Run Again'),AT(290,1,,11),USE(?RunAgainBtn),TIP('Run another instance')
    END
SortCls  SortClass_Exp

    CODE
    SYSTEM{PROP:PropVScroll}=1
    OPEN(Window)
    ?SHEET1{PROP:TabSheetStyle}=2
    0{PROP:text}=clip(0{PROP:text}) &' - Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    Intxt = '<13,10>   Paste Errors from MAP like below: ' & |
     '<13,10><13,10>      Unresolved External MILESDRIVEN@F5EICGLlll in eic_cust.obj' & |
     '<13,10><13,10>   Then click Parse Symbols and view the Out EXP tab.'
    SortCls.Init(ParseQ,?List:ParseQ,1)
    ACCEPT
        CASE EVENT()
        END
        CASE ACCEPTED()
        OF ?PasteBtn
           LenIn = LEN(CLIP(CLIPBOARD()))
           IF LenIn = 0 THEN
              Message('Clipboard is Empty','Paste') ; CYCLE
           ELSIF LenIn <= MaxTxtSize THEN
              Intxt = CLIPBOARD()
           ELSE
              BtnNo=MESSAGE('Clipboard contains ' & LenIn &' bytes.' & |
                           '<13,10>The maximum size is ' & MaxTxtSize & ' bytes.' & |
                           '||Select if you would like to process part of the paste.' & |
                           '||Note: There is an 80 byte overlap.|Check first and last lines for truncated symbols.' & |
                           '','Paste Too Big',ICON:Asterisk, |
                           'Cancel' & |                                          !Button 1 Cancel
                                 '|First ' & MaxTxtSize & |                      !Button 2 First
                                 '|Second ' & MaxTxtSize & |                     !Button 3
                           CHOOSE(LenIn < MaxTxtSize * 3 - 80,'','|Third') & |   !Button 4
                           CHOOSE(LenIn < MaxTxtSize * 4 - 80,'','|Fourth') & |  !Button 5
                           CHOOSE(LenIn < MaxTxtSize * 5 - 80,'','|Fifth') )     !Button 6 5th
              CASE BtnNo
              OF 1 ; CYCLE
              OF 2      ; Intxt = CLIPBOARD()  !test SUB(CLIPBOARD(),1,MaxTxtSize)
              OF 3 TO 6 ; Intxt = SUB(CLIPBOARD(),1 + MaxTxtSize * (BtnNo-2)-80,MaxTxtSize)  !
              END

           END
           DISPLAY
           DO ParseRtn

        OF ?ParseBtn    ; DO ParseRtn
        OF ?CopyInpBtn  ; SETCLIPBOARD(InTxt)
        OF ?CopyExpBtn  ; SETCLIPBOARD(OtEXP)
        OF ?AlignAtBtn  ; DO AlignAtRtn
        OF ?SortByLine OROF ?SortByLine_Out ; IF RECORDS(ParseQ) THEN DO BuildExpTextRtn.
        OF ?RunAgainBtn ; RUN(COMMAND('0'))
        END
        CASE FIELD()
        OF ?LIST:ParseQ
           IF EVENT()=EVENT:HeaderPressed THEN SortCls.HeaderPressed().
        END
    end
    CLOSE(Window)
    RETURN
!--------------------------
ParseRtn ROUTINE
    DATA
Lin1    STRING(1024)
Upr1    STRING(1024)
Symbol  STRING(256)
LnX     LONG
SpaceX  LONG
Did1    BOOL
OrdX    LONG,AUTO
EndX    LONG,AUTO
InCnt   LONG
BlanksContig LONG
Unresolved_External     EQUATE('UNRESOLVED EXTERNAL')
Size_UnEx               EQUATE(SIZE(Unresolved_External))
QX   LONG
DolPos  LONG            !$  Position
AtFPos  LONG            !@F Position
AtFLen  LONG            !the @F## the ##
    CODE
! Parsing lines like these
!    Unresolved External TRUCK_ISVALID@F14INVOICEACTIONSl in eic_cust.obj
!    Unresolved External EXTENDEDNETWEIGHTINVOICELINE@F8EICINVENlOUc in eic_cust.obj
!    Unresolved External ALLOCATE@F8EICINVENsbUcldd in eic_cust.obj
!    Unresolved External PO_PICKUPS_LOADDATES@F9POACTIONSOlOl in eic_cust.obj
! But can be just EXP lines like FUNCTION@F @?

    FREE(ParseQ) ; CLEAR(ParseQ)
    CLEAR(OtEXP)
    CLEAR(InEXP)
    CLEAR(LenSymGrp)
    InCnt = ?InTxt{PROP:LineCount}
    LOOP LnX = 1 TO InCnt
        Lin1=LEFT(?InTxt{PROP:Line,LnX})
        IF ~Lin1 THEN CYCLE.
        ParQ:ErrLine = Lin1
        Upr1=UPPER(Lin1)
        IF ~InputIsEXP AND LnX < 16 AND Upr1='EXPORTS ' THEN    !I forget to check the box testing
            CASE Message('This data contains an "EXPORTS" line.' & |
                       '||This appears to be an EXP file and not MAP Errrors' & |
                       '|Would you like to treat it as an EXP file?','EXP File?',ICON:Question,'EXP File|Errors|Cancel')
            OF 1 ; InputIsEXP=1 ; DISPLAY
            OF 2
            OF 3 ; BREAK
            END
        END
        IF ~InputIsEXP THEN         !this is MAP Unresolved External
           IF SUB(Upr1,1,Size_UnEx) <> Unresolved_External THEN
              InEXP = CHOOSE(~InEXP,'',CLIP(InEXP) & '<13,10>') &'         Not "' & Unresolved_External &'"'
              CYCLE
           END
           Lin1 = LEFT(SUB(Lin1,Size_UnEx+1,9999))

        ELSE !InputIsEXP
           Lin1=LEFT(Lin1)
           Upr1=LEFT(Upr1)                           ! 12345678                12345678                 12345678901
           IF Upr1[1]='' OR Upr1[1]=';' OR  Upr1[1:8]='EXPORTS ' OR Upr1[1:8]='LIBRARY ' OR Upr1[1:11]='IMAGE_BASE '
              InEXP = CHOOSE(~InEXP,'',CLIP(InEXP) & '<13,10>') &'         Not Symbol "' & Upr1[1:8] &'"'
              CYCLE
           END
           IF ~INSTRING('@',Upr1,1) THEN
              InEXP = CHOOSE(~InEXP,'',CLIP(InEXP) & '<13,10>') &'         No @ Export: ' & CLIP(Lin1)
              CYCLE
           END

        END
        SpaceX = INSTRING(' ',Lin1,1)      !Find space after Symbol so cutoff rest
        Lin1=SUB(Lin1,1,SpaceX)            !Cutoff  " in xxxxx.obj"
        Symbol=Lin1

        ParQ:Symbol = Symbol        !Same symbol duplicated in multiple errors need to remove
        GET(ParseQ,ParQ:Symbol)
        IF ~ERRORCODE() THEN
            InEXP = CHOOSE(~InEXP,'',CLIP(InEXP) & '<13,10>') &'         Duplicate "' & CLIP(Symbol) &'"'
            CYCLE
        END

        ParQ:Parent='-- No @ or $ --'
        ParQ:AtF_Dollar='~'

        AtFPos = INSTRING('@',Symbol)              !look for ALLOCATE@F8EICINVENsbUcldd
        IF AtFPos THEN
           ParQ:AtF_Dollar = '@'
           ParQ:Parent=SUB(Symbol,AtFPos+1,999)     !Take after @ should be 'F'
!  message('ParQ:Parent=' & CLIP(ParQ:Parent) & |
!        '|ParQ:Parent[1]=' & ParQ:Parent[1] & |
!        '|ParQ:Parent[2:3]=' & ParQ:Parent[2:3] & |
!        '|Numeric[2:3]=' & NUMERIC(ParQ:Parent[2:3]) & |
!        '|Numeric[2]=' & NUMERIC(ParQ:Parent[2]) & |
!        '','Parse')
           IF ParQ:Parent[1]='F' THEN             ! ALLOCATE@F8EICINVENsbUcldd
              ParQ:AtF_Dollar = 'F'
              IF NUMERIC(ParQ:Parent[2:3]) THEN            !Was it @F##   Class MATCHUPR@F10EICSTRINGS
                 AtFLen=ParQ:Parent[2:3]
                 ParQ:Parent = SUB(ParQ:Parent,4,AtFLen)
              ELSIF NUMERIC(ParQ:Parent[2]) THEN           !Was it @F#    Class ALLOCATE@F8EICINVEN
                 AtFLen=ParQ:Parent[2]
                 ParQ:Parent = SUB(ParQ:Parent,3,AtFLen)
              ELSE
                 ParQ:Parent='@Functions'                  !Was it '@F '  Not Class e.g. BROWSEPRF002P@F @?
              END

           END
        ELSE
           DolPos = INSTRING('$',Symbol)
           IF DolPos THEN
              ParQ:AtF_Dollar = '$'
              ParQ:Parent='$  Data'
              CASE SUB(Symbol,1,DolPos)
              OF   'VMT$'                       !Class has  VMT$ClassName
              OROF 'TYPE$'                      !Class has  TYPE$ClassName
              OROF 'TCB$'                       !Queue has  TCB$QueueName
                   ParQ:Parent=SUB(Symbol,DolPos+1,99)
              END
!              ELSE
!                ParQ:Parent=SUB(Symbol,1,DolPos-1)
!              END
           END
        END
!Think about FILE and Parent Grouping, hmmm mix of symbols hard to group
!  $BDF100A @?                          FILE $
!  BDF100A$ACTMSTA:AutoNoKey @?         KEY
!  BDF100A$ACTMSTA:AcctNoKey @?
!  BDF100A$ACTMSTA:DescKey @?
!  BDF100A$ACTMSTA:DeptKey @?
!  BDF100A$ACTMSTA:NewAcctNoKey @?
!  BDF100A$ACTMSTA:PriorYrAcctNoKey @?
!  BDF100A$ACTMSTA:RECORD @?
!  BDF100A$TYPE$ACTMSTA:RECORD @?       RECORD is Group

!        IF ~AtFPos THEN AtFPos = INSTRING('$',Lin1).
!        IF ~AtFPos THEN
!            ParQ:Parent='-- No @ or $ --'
!        ELSE
!            ParQ:Parent=SUB(Lin1,AtFPos,999)
!        END

        ParQ:Symbol    = Symbol
        ParQ:SymbolTip = Symbol
        ParQ:LenSymbol = LEN(CLIP(Symbol))
        ParQ:LineNo = LnX
        ADD(ParseQ)

        InEXP = CHOOSE(~InEXP,'',CLIP(InEXP) & '<13,10>') &'    '& CLIP(Symbol) &'  @?'

        LenSym:Count     += 1
        LenSym:TotLength += ParQ:LenSymbol
        IF ParQ:LenSymbol > LenSym:MaxLength THEN
           LenSym:MaxLength=ParQ:LenSymbol
           LenSym:MaxSymbol=ParQ:Symbol
           LenSym:MaxLineNo=ParQ:LineNo
        END
        IF ParQ:LenSymbol < LenSym:MinLength OR ~LenSym:MinLength THEN
           LenSym:MinLength=ParQ:LenSymbol
           LenSym:MinSymbol=ParQ:Symbol
           LenSym:MinLineNo=ParQ:LineNo
        END
        IF LenSym:Count=30 THEN         !Let user see use making progress
           SELECT(?TabParseQ)
           DISPLAY
        END
    END
    DISPLAY
    LenSym:AvgLength = LenSym:TotLength / LenSym:Count + .5
    DO BuildExpTextRtn
    DO StandardDeviationRtn
    DISPLAY
    SELECT(?TabOtEXP)
    EXIT
StandardDeviationRtn ROUTINE
    DATA
QX  LONG
    CODE
    !Sum the (Value - Mean) ^ 2 i.e. squared
    !Then take the Average of that Sum
    !Std Dev is the Square Root of the Average
    LenSym:StdMean = LenSym:TotLength / LenSym:Count
    LenSym:StdDevSum = 0
    LOOP QX=1 TO RECORDS(ParseQ)
        GET(ParseQ,QX)
        LenSym:StdDevSum += (LenSym:StdMean - ParQ:LenSymbol)^2
    END
    LenSym:StdDeviaton = SQRT(LenSym:StdDevSum / LenSym:Count)
    EXIT
!-----------------
BuildExpTextRtn ROUTINE
    DATA
QX  LONG
Last_Parent LIKE(ParQ:Parent)
    CODE
    IF SortByLine
       SORT(ParseQ,ParQ:LineNo)
       SortCls.SetSortCol(COL_ParQ:LineNo)
    ELSE
       SORT(ParseQ,ParQ:Parent,ParQ:AtF_Dollar,ParQ:Symbol)
       SortCls.SetSortCol(COL_ParQ:Parent)
    END
    OtEXP = ''
    LOOP QX=1 TO RECORDS(ParseQ)
        GET(ParseQ,QX)
        IF ~SortByLine AND Last_Parent <>ParQ:Parent THEN
           IF OtEXP THEN OtEXP = CLIP(OtEXP) & '<13,10>' .
           OtEXP = CLIP(OtEXP) & '; -{9}  '& CLIP(ParQ:Parent) &'  -{9}' & '<13,10>'
           Last_Parent = ParQ:Parent
        END
        OtEXP = CLIP(OtEXP) &'    '& CLIP(ParQ:Symbol) &'  @?' & '<13,10>'
        IF QX=30 THEN         !Let user see us making progress
           SELECT(?TabOtEXP)
           DISPLAY
        END
    END
    DO StatsSetRtn
    DISPLAY
    EXIT
!-----------------
StatsSetRtn ROUTINE
    DATA
InCnt   LONG
OtCnt   LONG
InByt   LONG
OtByt   LONG
    CODE
    InCnt=?InTxt{PROP:LineCount}
    OtCnt=RECORDS(ParseQ)  !?OtEXP{PROP:LineCount}
    InByt=LEN(CLIP(InTxt))
    OtByt=LEN(CLIP(OtEXP))
    ?Stats_Lines{PROP:Text}='Lines: ' & InCnt &' In, ' & OtCnt &' Out, ' & InCnt-OtCnt & ' removed'
    ?Stats_Bytes{PROP:Text}='Bytes: ' & InByt &' In, ' & OtByt &' Out, ' & InByt-OtByt & ' removed'
    EXIT
!-----------------
AlignAtRtn ROUTINE
    DATA
Lin1    STRING(1024)
LnX     LONG
OtCnt   LONG
AlnTxt  LIKE(OtEXP) !Aligned Text
MaxAt  LONG
AtPos  LONG
Minus1 LONG
At_Question   EQUATE(' @?')
    CODE
    CLEAR(AlnTxt)
    LOOP LnX = 1 TO ?OtEXP{PROP:LineCount}  !Find Highest Equal
        Lin1=?OtEXP{PROP:Line,LnX}
        IF LEFT(Lin1,1)=';' OR ~Lin1 THEN CYCLE.
        AtPos = INSTRING(At_Question,Lin1,1)
        IF ~AtPos THEN CYCLE.
        IF AtPos AND AtPos > MaxAt THEN MaxAt = AtPos.
    END
    IF MaxAt < 2 THEN
        Message('There is no ' & At_Question)
        EXIT
    END

    LOOP LnX = 1 TO ?OtEXP{PROP:LineCount}  !Align at Highest Equal
        Lin1=?OtEXP{PROP:Line,LnX}
        AtPos = INSTRING(At_Question,Lin1,1)
        IF AtPos > 1 AND LEFT(Lin1,1)<>';' THEN
           Minus1 = 0
           Lin1 = SUB(Lin1,1,AtPos-1) & ALL(' ',MaxAt-AtPos-Minus1 ) & SUB(Lin1,AtPos,9999)
        END
        OtCnt += 1
        AlnTxt = CHOOSE(OtCnt=1,'',CLIP(AlnTxt) & '<13,10>') & Lin1
    END
    OtEXP = AlnTxt
    DISPLAY
    SELECT(?TabOtEXP)
    EXIT

!===============================
DB   PROCEDURE(STRING xMsg)
Prfx EQUATE('ExpErr: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMsg)+3),AUTO
  CODE
  sz=Prfx & CLIP(xMsg) & '<13,10>'
  OutputDebugString(sz)
  RETURN
!==========================================================
SortClass_Exp.Init PROCEDURE(QUEUE ListQueue, LONG ListFEQ, SHORT SortColNow=0)
    CODE
    SELF.QRef &= ListQueue
    SELF.FEQ=ListFEQ
    ListFEQ{PROPLIST:HasSortColumn}=1
    SELF.FEQ{PROPLIST:HdrSortBackColor}=8000001Bh  !GradientActiveCaption
    SELF.FEQ{PROPLIST:HdrSortTextColor}=Color:CaptionText
    !Brighter {PROPLIST:HdrSortBackColor}=COLOR:Highlight / {PROPLIST:HdrSortTextColor}=COLOR:HighlightText
    IF SortColNow THEN SELF.SetSortCol(SortColNow).
    RETURN
!-----------------------------------
SortClass_Exp.SetSortCol PROCEDURE(SHORT SortColNow)
    CODE
    SELF.ColumnNow=SortColNow
    SELF.FEQ{PROPLIST:Locator,ABS(SortColNow)}=1
    SELF.QFieldLast=SELF.FEQ{PROPLIST:FieldNo,ABS(SortColNow)}
    SELF.Who1st=CHOOSE(SortColNow<0,'-','+') & WHO(SELF.QRef,SELF.QFieldLast) ; SELF.Who2nd=''
    SELF.FEQ{PROPLIST:SortColumn}=ABS(SortColNow)
    RETURN
!-----------------------------------
SortClass_Exp.HeaderPressed PROCEDURE(SHORT ForceSortByColumn=0)
QRecord    STRING(SIZE(SELF.QRef)),AUTO
LChoice    LONG,AUTO
X          LONG,AUTO
ColumnNow  &SHORT
ColumnLast &SHORT
QFieldNow  &SHORT
QFieldLast &SHORT
Who1st     &STRING
Who2nd     &STRING
    CODE
    ColumnNow&=SELF.ColumnNow;ColumnLast&=SELF.ColumnLast;QFieldNow&=SELF.QFieldNow;QFieldLast&=SELF.QFieldLast;Who1st&=SELF.Who1st;Who2nd&=SELF.Who2nd
    LChoice = CHOICE(SELF.FEQ)
    IF LChoice THEN GET(SELF.QRef, LChoice) ; QRecord=SELF.QRef.
    ColumnNow=CHOOSE(~ForceSortByColumn, SELF.FEQ{PROPList:MouseDownField}, ForceSortByColumn)
    QFieldNow=SELF.FEQ{PROPLIST:FieldNo,ColumnNow}
    IF QFieldNow<>ABS(QFieldLast) AND Who1st THEN Who2nd=',' & Who1st.
    Who1st=CHOOSE(QFieldNow=QFieldLast,'-','+') & WHO(SELF.QRef,QFieldNow)
    !Debug tip SELF.FEQ{PROP:Tip}='Sort ColumnNow:'& ColumnNow  &' QNow=' & QFieldNow &' QLast='& QFieldLast &' Who=' & CLIP(Who1st) & Who2nd
    SORT(SELF.QRef,CLIP(Who1st) & Who2nd)
    SELF.FEQ{PROPLIST:Locator,ColumnNow}=1
    ColumnLast=CHOOSE(QFieldNow=ABS(QFieldLast),-1*ColumnLast,ColumnNow)
    QFieldLast=CHOOSE(QFieldNow=ABS(QFieldLast),-1*QFieldLast,QFieldNow)
    IF LChoice THEN !Reselect the LChoice that was selected
       LOOP X=1 TO RECORDS(SELF.QRef) ; GET(SELF.QRef,X)
          IF SELF.QRef=QRecord THEN SELF.FEQ{PROP:Selected}=X ; BREAK.
       END
    END
    DISPLAY
    RETURN
!============================