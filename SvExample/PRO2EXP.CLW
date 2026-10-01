  PROGRAM
  
  MAP
    Clw2Exp(*cstring ins),string
  END
  INCLUDE('KEYCODES.CLW')
CWproto  cstring(400)
ExpProto cstring(400)
CProto   cstring(400)
OldCWProto cstring(400)

w   WINDOW('Convert Prototype to Export Name'),AT(,,398,122),CENTER,GRAY,IMM,AUTO, |
            ICON('CLARION.ICO'),FONT('Segoe UI',10,,FONT:regular),RESIZE
        GROUP('&Clarion Prototype'),AT(6,4,384,29),USE(?Group3),BOXED
            TEXT,AT(14,16,351,10),USE(CwProto),FONT('Consolas'),SINGLE
            BUTTON,AT(368,13,17,16),USE(?PasteCwProtoBtn),SKIP,ICON(ICON:Paste),TIP('Paste Clarion P' & |
                    'rototype from Clipboard'),FLAT
        END
        GROUP('Export Procedure Name'),AT(6,36,384,29),USE(?Group1),BOXED
            TEXT,AT(14,48,351,10),USE(ExpProto),FONT('Consolas'),COLOR(COLOR:BTNFACE),READONLY,SINGLE
            BUTTON,AT(368,45,17,16),USE(?CopyExpProtoBtn),SKIP,ICON(ICON:Copy),TIP('Copy Clarion Pro' & |
                    'totype to Clipboard'),FLAT
        END
        GROUP('C Prototype'),AT(6,68,384,29),USE(?Group2),BOXED
            TEXT,AT(14,80,351,10),USE(CProto),FONT('Consolas'),COLOR(COLOR:BTNFACE),READONLY,SINGLE
            BUTTON,AT(368,77,17,16),USE(?CopyCProtoBtn),SKIP,ICON(ICON:Copy),TIP('Copy C Prototype t' & |
                    'o Clipboard'),FLAT
        END
        BUTTON('&Done'),AT(180,102,38,14),USE(?Button1),KEY(EscKey)
    END

Converter CLASS,TYPE
Hold        CSTRING(1000)
IsRaw       BYTE
NoMangle    BYTE
IsOmitable  BYTE
IsAddress   BYTE
Adims       BYTE
Convert     FUNCTION(*cstring),STRING
StoreName   PROCEDURE(string),VIRTUAL
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StoreResult PROCEDURE(Byte),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
EndProc     PROCEDURE,VIRTUAL
          END

ExpConverter CLASS(Converter)
StoreName   PROCEDURE(string),VIRTUAL
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
          END

CConverter CLASS(Converter)
StoreSym    PROCEDURE(Byte,byte,string),VIRTUAL
StartProc   PROCEDURE,VIRTUAL
EndProc     PROCEDURE,VIRTUAL
          END
  PRAGMA('compile(ResCode.CLW)')
  INCLUDE('ResDef.CLW'),ONCE  
WinResize            WindowResizeType          
  CODE
  OPEN(W)
  WinResize.Init(AppStrategy:Surface,Resize:SetMinSize) !Not sure what's bets but this works ok
  WinResize.SetStrategy(?Button1, Resize:FixXCentre , Resize:LockSize)
  0{PROP:MaxHeight}=0{PROP:Height}
  
  CLEAR(OldCWProto)
  SELECT(?CWProto)
  if w{PROP:At,4} - ?Group3{PROP:At,4} < 0 THEN
    beep
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:Sized ; POST(EVENT:DoResize)   !Required for Resize Class
    END
    CASE ACCEPTED()
    OF ?CWProto
      IF CWProto AND CWProto <> OldCWProto THEN
        DO NewStyleMapInColumn1Rtn      !Change  "Fct PROCEDURE(LONG X)" to "Fct(LONG X)"
        ExpProto = Clw2Exp(cwproto)
        CProto = CConverter.Convert(cwproto)
        OldCWProto = CWProto
        SELECT(?CWProto)
      END
    OF ?PasteCwProtoBtn ; DO PasteCwProtoBtnRtn
    OF ?CopyExpProtoBtn ; IF ExpProto THEN SETCLIPBOARD(ExpProto).
    OF ?CopyCProtoBtn   ; IF CProto   THEN SETCLIPBOARD(CProto).
    OF ?Button1         ; BREAK 
    END
  END

PasteCwProtoBtnRtn ROUTINE  !Take Clipboard then check for <13,10> and cut off
    DATA
Eol13 LONG    
    CODE 
    IF ~CLIPBOARD() THEN EXIT.
    CWProto=LEFT(CLIPBOARD())
    Eol13=INSTRING(CHR(13),CWProto,1)
    IF Eol13 THEN CWProto=SUB(CWProto,1,Eol13-1).
    POST(EVENT:Accepted,?CWProto)
    EXIT

NewStyleMapInColumn1Rtn ROUTINE !Is it new style "ProcLabel PROCEDURE" or "ProcLabel FUNCTION"
    DATA                 !e.g.: NextTab PROCEDURE(LONG SheetFEQ, LONG Wrap=0, <*LONG TabFEQ>),LONG
Space1  LONG             !e.g.: NextTab FUNCTION (LONG SheetFEQ, LONG Wrap=0, <*LONG TabFEQ>),LONG
Paren1  LONG
    CODE
    CWProto = LEFT(CWProto)     !Old code should have LEFTed
    IF ~MATCH(UPPER(CWProto),'^[^ ()]+ +{{PROCEDURE|FUNCTION} *(',Match:Regular) THEN EXIT.  !Not New Type   ProcLabel PROCEDURE
    Space1 = INSTRING(' ',CLIP(CWProto),1)   !      Space     Paren
    IF Space1 < 2 THEN EXIT.                 !     123456789012
    Paren1 = INSTRING('(',CLIP(CWProto),1)   !     F PROCEDURE(   minimum
    IF Paren1 < 12  OR Paren1 < Space1 THEN EXIT.
    CWProto=SUB(CWProto,1,Space1-1) & SUB(CWproto,Paren1,999)
    DISPLAY
    EXIT

Clw2Exp PROCEDURE(ins)
  CODE
    return ExpConverter.Convert(ins)

Converter.StoreName   PROCEDURE(string s)
  CODE
    Self.hold = s

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
  CODE
    Gn = INSTRING('(',ins)
    IF ~Gn THEN
      Self.StoreName(ins)
      Self.NoMangle = 1
    ELSE
      Self.StoreName(ins[1:Gn-1])
      LOOP EndP = LEN(Ins) TO 1 BY -1
      UNTIL Ins[EndP] = ')'
      ! Raw mode causes greate C++ linkname compatability
      Self.IsRaw = CHOOSE( EndP AND Instring(',RAW',UPPER(Ins),1,EndP),1,0)
      ! An ill-formed prototype is not mangled
      Self.NoMangle = CHOOSE( ~EndP OR Instring(',PASCAL',UPPER(Ins),1,EndP) OR Instring(',C',UPPER(Ins),1,EndP),2,0)
      ! More, need return type
    END
    Self.StartProc
    IF ~Self.NoMangle THEN
      LOOP UNTIL Ins[Gn] = ')'
        DO GetSymbol
        IF ~Symbol THEN BREAK .
        CASE UPPER(Symbol)
        OF   'SIGNED'          !Equates.CLW Types
        OROF 'UNSIGNED' 
        OROF 'BOOL' 
        OROF 'POINTER_T' 
        OROF 'COUNT_T' 
              Symbol='LONG'
        OF 'INDEX'
              Symbol='KEY'
        OF 'DATETIME' 
              Symbol='DECIMAL'
        END        
        SymVal = INLIST(UPPER(Symbol),'FILE','BLOB','KEY','QUEUE','REPORT','WINDOW', |
                        'VIEW','APPLICATION')
        TVal = INLIST(UPPER(Symbol),'BYTE','SHORT','LONG','USHORT','ULONG','SREAL',|
                      'REAL','DATE','TIME','DECIMAL','PDECIMAL','BFLOAT4', |
                      'BFLOAT8','?','STRING','PSTRING','CSTRING','GROUP', |
                      'ANY','ASTRING','BSTRING','USTRING')
        Self.StoreSym(SymVal,TVal,Symbol)
      END
    END
    Self.EndProc
    RETURN Self.Hold


! Gn comes in pointing to last seperator
! Exits pointing to next seperator
! Symbol has type name from prototype
GetSymbol ROUTINE
  LOOP
    Gn += 1
  WHILE Ins[Gn]= ' '
  IF UPPER(Ins[Gn : Gn+5]) = 'CONST ' THEN   ! CONST is a qualifier, not the type
    Gn += 5
    LOOP
      Gn += 1
    WHILE Ins[Gn] = ' '
  END
  IF Ins[Gn]='<<' THEN
    Gn+= 1
    Self.IsOmitable = 1
  ELSE
    Self.IsOmitable = 0
  END
  IF Ins[Gn]='*' THEN
    Self.IsAddress = 1
    Gn += 1
  ELSE
    Self.IsAddress = 0
  END
  SymbolP = 1
  LOOP UNTIL INSTRING(Ins[Gn],',= >)[')
    Symbol[SymbolP] = Ins[Gn]
    Gn += 1
    SymbolP += 1
  END
  Symbol[SymbolP] = '<0>'
  Self.Adims = 0
  IF Ins[Gn]='[' THEN
    LOOP
      Self.Adims += 1
      Gn+=1
    UNTIL Ins[Gn] = ']'
  END
  LOOP UNTIL INSTRING(Ins[Gn],',)')
    Gn += 1
  END

ExpConverter.StoreName   PROCEDURE(string nam)
  CODE
    Self.Hold = UPPER(Nam)

ExpConverter.StoreSym    PROCEDURE(Byte EVal,Byte TVal,string symbol)
  CODE
    IF EVal THEN
      Self.Hold = Self.Hold & 'B' & CHOOSE(EVal,'f','b','k','q','r','w','i','a')
    ELSIF TVal THEN
      DO Preamble
      Self.Hold = Self.Hold & CHOOSE(TVal,'Uc','s','l','Us','Ul','f','d','bd','bt',|
                              'e','p','b4','b8','u','sb','sp','','','u','sa','sw','')   ! 17 CSTRING + 18 GROUP take their letter in the CASE below
      CASE UPPER(Symbol)
      OF 'CSTRING'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'c','sc')
      OF 'GROUP'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'v','g')
      OF 'USTRING'
        ! sz for every non-RAW form (value, *, <>, <*>, CONST *, arrays), z for RAW
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'z','sz')
      END
    ELSE
      Self.Hold = Self.Hold & LEN(Symbol) & UPPER(Symbol)
    END

Preamble ROUTINE
  IF Self.IsAddress OR Self.Adims THEN                        ! * or []
    Self.Hold = Self.Hold & CHOOSE(Self.IsOmitable,'P','R')   !P=<*>  R=*
  ELSIF Self.IsOmitable THEN
    Self.Hold = Self.Hold & 'O'                               !O=< >
  END
  IF Self.Adims THEN
    Self.Hold = Self.Hold & ALL('A',Self.Adims)               !A=[] AA=[,] AAA=[,,]
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
         'unsigned short','unsigned long','float','double','long','long','?')
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
          OF 'USTRING'
            Self.Hold[i] = '<0>'
            IF ~Self.IsRaw THEN
              Self.Hold = Self.Hold & 'unsigned len,'
            END
            Self.Hold = Self.Hold & 'wchar_t *'
          OF 'ANY'
            Self.Hold[i] = '<0>'
            Self.Hold = Self.Hold & 'void *'
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
