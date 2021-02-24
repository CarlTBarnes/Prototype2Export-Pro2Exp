  PROGRAM

!This is the CW\EXAMPLES\SRC\PRO2EXP program with some user interface enhancements.
!Inspired by an earlier version of Mangle.exe written by Lee G. White.
!Written by Jeff Slarve Feb-17-2005
!Enhancements include: 1. Mangling of multiple prototypes at once.
!                      2. Saving/Restoring of user preferences
!                      3. Support of multi-line prototypes (by use of the "FlattenPrototypes" function.
!                      4. Level of indent setting
!                      5. Column of ordinal setting
!                      6. Use of TEXT controls instead of STRING controls so clipboard maybe used.
! Feb-19-2005 version .091
!                      7. Use of courier instead of FixedSys (Lee White)
!                      8. RemoveProcFunc Routine (Lee White)
!                      9. Increased string size (Lee White)
!
! Please report any bugs or enhancements to jeff.slarve@jssoftware.com
!

  MAP
    Clw2Exp(*cstring ins),string
    FlattenProtoTypes(*CSTRING pPrototypes)
  END


CWproto       CString(30000)
ExpProto      CString(30000)
CProto        CString(30000)
CWProtoLine   CString(30000)
CWExpLine     CString(30000)
Ndx           Short
OrdinalColumn Byte
IndentLevel   Byte
AllowFlatten  Byte
IniFile       String(File:MaxFilePath)

W    WINDOW('Convert Prototype to Export Name'),AT(,,527,273),FONT('MS Sans Serif',8,,FONT:regular),CENTER, |
         ICON('CLARION.ICO'),GRAY,DOUBLE
       STRING('Clarion Prototypes'),AT(7,1),USE(?ClarionPrototypesLabel),TRN
       TEXT,AT(5,11,499,116),USE(Cwproto),HVSCROLL,FONT('Courier New',9,,FONT:regular,CHARSET:ANSI)
       BUTTON,AT(508,10,17,16),USE(?PasteButton),FLAT,TIP('Paste Clarion prototype(s) from clipboard'), |
           ICON(ICON:Paste)
       BUTTON,AT(508,27,17,14),USE(?RefreshButton),FLAT,TIP('Refresh'),ICON(ICON:VCRplay)
       PROMPT('Indent Level:'),AT(5,260),USE(?IndentPrompt)
       SPIN(@n3b),AT(52,260,43,10),USE(IndentLevel),TIP('Number of columns to indent'),RANGE(1,20),STEP(1)
       PROMPT('Ordinal Column:'),AT(105,260),USE(?OrdinalPrompt)
       SPIN(@n3b),AT(159,260,43,10),USE(OrdinalColumn),TIP('The number of columns over that you want the "@?"')
       CHECK('Allow "Flattening" of prototypes'),AT(214,260),USE(AllowFlatten),TIP('Allows the conversion of multiline prototypes into single line prototypes before' &|
           ' processing.<13,10>E.G. This:<13,10,13,10>FormatAddress(<<String pStreetAddress>,|<13,10> {11}' &|
           '   <<String pCity>,|<13,10> {14}<<String pState>,|<13,10> {14}<<String pZIP' &|
           '>,|<13,10> {14}<<String pCountry>),String<13,10,13,10>Becomes This:<13,10,13,10>FormatAddress(<<' &|
           'String pStreetAddress>,<<String pCity>,<<String pState>,<<String pZIP>,<<String pCou' &|
           'ntry>),String')
       BUTTON('X'),AT(508,256,17,16),USE(?CloseButton),FLAT,FONT(,,,FONT:bold,CHARSET:ANSI),TIP('Close'), |
           STD(STD:CLOSE)
       SHEET,AT(7,127,169,17),USE(?Sheet1),NOSHEET
         TAB('Mangled Clarion Prototypes'),USE(?Tab1)
           TEXT,AT(5,145,499,110),USE(ExpProto),HVSCROLL,FONT('Courier New',9,,FONT:regular,CHARSET:ANSI), |
               COLOR(COLOR:BTNFACE),READONLY
           BUTTON,AT(508,145,17,16),USE(?CopyExportButton),FLAT,TIP('Copy Clarion .EXP text.'),ICON(ICON:Copy)
         END
         TAB('C Prototypes'),USE(?CPrototypesTab)
           TEXT,AT(5,145,499,110),USE(CProto),HVSCROLL,FONT('Courier New',9,,FONT:regular,CHARSET:ANSI), |
               COLOR(COLOR:BTNFACE),READONLY
           BUTTON,AT(508,145,17,16),USE(?CopyCPrototypeButton),FLAT,TIP('Copy C Prototype text.'),ICON(ICON:Copy)
         END
       END
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
  CODE

  IniFile = LongPath('.\jsPro2xp.ini')

  IndentLevel   = GetINI('Defaults','IndentLevel',2,IniFile)
  OrdinalColumn = GetINI('Defaults','OrdinalColumn',60,IniFile)
  AllowFlatten  = GetINI('Defaults','AllowFlatten',1,IniFile)

  OPEN(W)
  SELECT(?CWProto)
  ACCEPT
    CASE ACCEPTED()
    OF ?CWProto
        Clear(ExpProto)
        Clear(CProto)
        If AllowFlatten
          FlattenPrototypes(CWProto)
        end
        Loop Ndx = 1 to ?CWProto{PROP:LineCount}
          CWProtoLine =  Clip(Left(?cwproto{PROP:Line,Ndx}))
          If NOT CWProtoLine then cycle.
          Do RemoveProcFunc
          CWProtoLine = All(' ',IndentLevel) & CWProtoLine
          CWExpLine = Clip(Clw2Exp(CWProtoLine))
          If ExpProto
            ExpProto = ExpProto & '<13,10>' & CWExpLine & Choose(OrdinalColumn <= Len(CWExpline),' ','') & ALL(' ',OrdinalColumn - Len(CWExpLine)) & '@?'
          else
            ExpProto = CWExpLine & Choose(OrdinalColumn <= Len(CWExpline),' ','') & ALL(' ',OrdinalColumn - Len(CWExpLine)) & '@?'
          end
          If CProto
            CProto = CProto & '<13,10>' & Clip(CConverter.Convert(CWProtoLine))
          else
            CProto = Clip(CConverter.Convert(CWProtoLine))
          end
        end
        Display
    of ?PasteButton
      CWProto = ClipBoard()
      Post(Event:Accepted,?CWProto)
    of ?Refreshbutton
      Post(Event:Accepted,?CWProto)
    of ?CopyExportButton
      SetClipBoard(ExpProto)
    of ?CopyCPrototypeButton
      SetClipBoard(CProto)
    END
    Case Event()
    of Event:Accepted orof Event:NewSelection
      Case Field()
      of ?IndentLevel orof ?OrdinalColumn
        Post(Event:Accepted,?CWProto)
      end
    of Event:CloseWindow
      PutINI('Defaults','IndentLevel',IndentLevel,IniFile)
      PutINI('Defaults','OrdinalColumn',OrdinalColumn,IniFile)
      PutINI('Defaults','AllowFlatten',AllowFlatten,IniFile)
    end
  END

RemovePROCFUNC ROUTINE
  !
  ! remove "procedure" and/or "function" from CWProtoLine for column one prototypes
  !
  !  map
  !proc1 procedure(params)
  !func1 function(params)
  !  end
  !
  DATA
posSpace    LONG
posParen    LONG
  CODE
  posSpace = INSTRING(' ', CWProtoLine, 1, 1)
  posParen = INSTRING('(', CWProtoLine, 1, 1)
  IF posSpace AND posSpace < posParen AND INLIST( UPPER(CLIP(LEFT( CWProtoLine[ posSpace+1 : posParen-1 ] ))), 'PROCEDURE', 'FUNCTION' )
    CWProtoLine = CWProtoLine[ 1 : posSpace-1 ] & CWProtoLine[ posParen : LEN(CWProtoLine) ]
  END
  EXIT


Clw2Exp PROCEDURE(ins)
  CODE
    return ExpConverter.Convert(ins)

Converter.StoreName   PROCEDURE(string s)
  CODE
    Self.hold = Clip(s)

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
        SymVal = INLIST(UPPER(Symbol),'FILE','BLOB','KEY','QUEUE','REPORT','WINDOW', |
                        'VIEW','APPLICATION')
        TVal = INLIST(UPPER(Symbol),'BYTE','SHORT','LONG','USHORT','ULONG','SREAL',|
                      'REAL','DATE','TIME','DECIMAL','PDECIMAL','BFLOAT4', |
                      'BFLOAT8','?','STRING','PSTRING','CSTRING','GROUP')
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
    Self.Hold = Clip(UPPER(Nam))

ExpConverter.StoreSym    PROCEDURE(Byte EVal,Byte TVal,string symbol)
  CODE
    IF EVal THEN
      Self.Hold = Self.Hold & 'B' & CHOOSE(EVal,'f','b','k','q','r','w','i','a')
    ELSIF TVal THEN
      DO Preamble
      Self.Hold = Self.Hold & CHOOSE(TVal,'Uc','s','l','Us','Ul','f','d','bd','bt',|
                              'e','p','b4','b8','u','sb','sp','')
      CASE UPPER(Symbol)
      OF 'CSTRING'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'c','sc')
      OF 'GROUP'
        Self.Hold = Self.Hold & CHOOSE(Self.IsRaw,'v','g')
      END
    ELSE
      Self.Hold = Self.Hold & LEN(Symbol) & UPPER(Symbol)
    END

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
LOC:Prototypes CString(30000)
Ndx2           Short
L              Short
IsComment      Byte

  Code

  Ndx  = 1
  Ndx2 = Instring('|',pPrototypes,1,1)
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




