This is the Pro2Exp that ships as an example. 

SV updated it to handle USTRING and posted in the Beta Forum. I made the below changes. The original CLW is included so a DIFF can show exact changes.
___
The Window was MS Sans Serif font. The Mangle is in a STRING control so no way to get on the clipboard.
 Revised to be Segoe UI with the `code` in Consolas. The Mangle is in a TEXT with Copy button. Resizable using ResCode.clw.
___
The DATETIME type was not handled nor Equate LONG types so added this code to change Type before the Mangle lookup was done. In the screen capture you'll see the mangle as named types `8DATETIME` `4BOOL` not `l`.

```clarion
        IF ~Symbol THEN BREAK .
        CASE UPPER(Symbol)
        OF   'SIGNED'          !Equates.CLW Types
        OROF 'UNSIGNED' 
        OROF 'BOOL' 
        OROF 'POINTER_T' 
        OROF 'COUNT_T' 
              Symbol='LONG'
        OF 'INDEX'
              Symbol='KEY'   !Edit 10/1 added 'INDEX' same as 'KEY' => 'Bk'
        OF 'DATETIME' 
              Symbol='DECIMAL'
        END
```
___
If a new style column 1 prototype is entered this routine converts it e.g. 
Input: `  NextTab PROCEDURE(LONG SheetFEQ)` 
Output: `NextTab(LONG SheetFEQ)`

```clarion
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
```
