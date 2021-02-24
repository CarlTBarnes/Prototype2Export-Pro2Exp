!Pro2EXP from Clarion 5
  PROGRAM
  MAP
    Clw2Exp(*cstring ins),string
  END
  INCLUDE('KEYCODES.CLW')
CWproto cstring(200)
ExpProto cstring(200)
CProto   cstring(200)
OldCWProto cstring(200)

w    WINDOW('Convert Prototype to Export Name - C5 Pro2Exp'),AT(,,379,122),FONT('Segoe UI',9,,FONT:regular),CENTER, |
         ICON('CLARION.ICO'),GRAY
       GROUP('Clarion Prototype'),AT(6,4,367,29),USE(?Group3),BOXED
         ENTRY(@s200),AT(14,16,351,10),USE(Cwproto)
       END
       GROUP('Export File Name'),AT(6,36,367,29),USE(?Group1),BOXED
         STRING(@s150),AT(14,48,351,10),USE(ExpProto)
       END
       GROUP('C Prototype'),AT(6,68,367,29),USE(?Group2),BOXED
         STRING(@s150),AT(14,80,351,10),USE(CProto)
       END
       BUTTON('&Done'),AT(332,101,38,14),KEY(EscKey),USE(?Button1)
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
  OPEN(W)
  CLEAR(OldCWProto)
  SELECT(?CWProto)
  if w{PROP:At,4} - ?Group3{PROP:At,4} < 0 THEN
    beep
  END
  ACCEPT
    CASE ACCEPTED()
    OF ?CWProto
      IF CWProto AND CWProto <> OldCWProto
        ExpProto = Clw2Exp(cwproto)
        CProto = CConverter.Convert(cwproto)
        OldCWProto = CWProto
        SELECT(?CWProto)
      END
    OF ?Button1
      BREAK
    END
  END

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
    Self.Hold = UPPER(Nam)

ExpConverter.StoreSym    PROCEDURE(Byte EVal,Byte TVal,string symbol)
  CODE
    IF EVal THEN 
    !SymVal = INLIST(UPPER(Symbol),'FILE','BLOB','KEY','QUEUE','REPORT','WINDOW','VIEW','APPLICATION')
      Self.Hold = Self.Hold & 'B' & CHOOSE(EVal,'f','b','k','q','r','w','i','a')
    ELSIF TVal THEN
      DO Preamble
      
   !INLIST(UPPER(Symbol),'BYTE','SHORT','LONG','USHORT','ULONG','SREAL',|        'Uc','s','l','Us','Ul','f'
   !                   'REAL','DATE','TIME','DECIMAL','PDECIMAL','BFLOAT4', |    'd','bd','bt','e','p','b4'
   !                   'BFLOAT8','?','STRING','PSTRING','CSTRING','GROUP')       'b8','u','sb','sp','')
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
  IF Self.IsAddress OR Self.Adims THEN                        ! * or []
    Self.Hold = Self.Hold & CHOOSE(Self.IsOmitable,'P','R')   !P=<*>  R=*
  ELSIF Self.IsOmitable THEN                                  !O=< >
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
