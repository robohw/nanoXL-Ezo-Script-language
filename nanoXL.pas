program NanoXL; // Restructured, partially optimized - 24.11.23 - Procedural version
uses sysutils;

type
  TLabel = record
    Name: string;
    Addr: Byte;
  end;

var
  Code: array of string;
  Tokens: array of string;
  Labels: array of TLabel;
  Vars: array['B'..'Z'] of LongInt;
  Line    : string;  
  LineNum, Stack: Byte;   // LineNum: Current line, Stack: Pseudo_stack for RET
  Trace: Boolean = False;
  Counter: LongInt = 0;   // Instruction counter
  InF, OutF: text;        // Input and output files
  i,j: Integer;

procedure PrintState;
var
  idx: Integer;
begin
  writeln(OutF, ' '); 
  writeln(OutF, '------------- (', Counter, ' lines done) - Code:');
  for idx := 1 to High(Code) do
    writeln(OutF, idx:2, '  ', Code[idx]);
    
  writeln(OutF, ' ');  
  writeln(OutF, '------------- Variables (B..R as num, S..Z as char):');
  for idx := Ord('B') to Ord('Z') do
    writeln(OutF, Chr(idx), ': ', Vars[Chr(idx)]);
  Close(OutF);
end;

procedure Err(Msg: string);
begin
  writeln(OutF, 'Err [in line ', j, ']: ', Msg);
  if Trace then PrintState;
  Halt(1);
end;

function KeyWord(kw: string): Boolean;
begin
   KeyWord := false;
   case kw of
     'IF', 'JMP', 'PRN', 'RET', 'INP': KeyWord := true;
   end;
end;

procedure AddToken(Tokn: string; Increment: Boolean);
begin
  SetLength(Tokens, Length(Tokens) + 1);
  Tokens[High(Tokens)] := Tokn;
  if Increment then Inc(i);
end;

function Tokenize(line: string): string;
var
  Tokn: string;

  procedure AddChar;
  begin
    Tokn := Tokn + Line[i];
    Inc(i);
  end;
begin
  Tokenize := 'OK';
  i := 1;
  SetLength(Tokens, 0);

  while i <= Length(Line) do
  begin
    Tokn := '';
    case Line[i] of
      'B'..'Z':
        begin
          while (i <= Length(Line)) and (Line[i] in ['B'..'Z', '0'..'9']) do AddChar;
          if (Length(Tokn) < 2) or KeyWord(Tokn) then
            AddToken(Tokn, False)
          else
            Err('Invalid variable ID: ' + Tokn);
        end;
      '0'..'9', '-':
        begin
          if Line[i] = '-' then
          begin
            Tokn := '-';
            Inc(i);
          end;
          while (i <= Length(Line)) and (Line[i] in ['0'..'9']) do AddChar;
          AddToken(Tokn, False);
        end;
      '+', '*', '/', '!', '%', '<', '=': if (Line[i+1] in [' ','-','A'..'Z','0'..'9']) then 
        AddToken(Line[i],True) else Err('Unknown logic operator: '+Line[i]+Line[i+1]);
      '.':
        begin
          while (i <= Length(Line)) and (Line[i] <> ' ') do AddChar;
          AddToken(Tokn, False);
        end;
      else Inc(i);
    end;
  end;
end;

procedure SetLabelAddr(const Name: string; Addr: Byte);
begin
  if Length(Name) < 2 then Err('Too short (label): ' + Name);
  for i := 0 to High(Labels) do
    if Labels[i].Name = Name then Err('Label already exists: ' + Name);
  SetLength(Labels, Length(Labels) + 1);
  Labels[High(Labels)].Name := Name;
  Labels[High(Labels)].Addr := Addr;
end;

function GetLabelAddr(const Name: string): Byte;
begin
  for i := 0 to High(Labels) do
    if Labels[i].Name = Name then Exit(Labels[i].Addr);
  Err('Label not found: ' + Name);
end;

function GetValue(Index: Byte): Integer;
var
  Token: string;
begin
  Token := Tokens[Index];
  case Token[1] of
    'B'..'Q': GetValue := Vars[Token[1]];
    'R':      GetValue := Random(Vars['R'] + 1);
    'S'..'Z': GetValue:= Byte(Vars[Token[1]]);
  else
    GetValue := StrToInt(Token);
  end;
end;

function Inputs(n: Byte): LongInt;
var
  InputStr: string;
begin
  repeat
    Write(Tokens[n], ': ');
    ReadLn(InputStr);
  until TryStrToInt(InputStr, Inputs);
end;

procedure SetValue(n: Byte);
var
  VarName, Operat: Char;
  Value: LongInt;
begin
  VarName := Tokens[n][1];  
  Operat := Tokens[n + 1][1]; 

  if (Operat in ['+','-','*','/','!']) then // assign with syntactic sugar
  begin
  case Operat of
    '+': Inc(Vars[VarName]);
    '-': Dec(Vars[VarName]);
    '*': Vars[VarName] := Vars[VarName] * 2; 
    '/': Vars[VarName] := Vars[VarName] div 2;
    '!': Vars[VarName] := not Vars[VarName];
  else
    Err('Unexpected operator: ' + Operat);
  end;
  exit;
  end; 
  
  if Operat <> '=' then Err('Invalid operator: ' + Operat); // checkers 
  if Tokens[n + 2] = 'INP' then Vars[VarName] := Inputs(n) else // assign with INPUT 
   
  if Length(Tokens) > n + 3 then                     // assign with complex math 
  begin    
    case Tokens[n + 3][1] of
      '+': Value := GetValue(n + 2) + GetValue(n + 4);
      '-': Value := GetValue(n + 2) - GetValue(n + 4);
      '*': Value := GetValue(n + 2) * GetValue(n + 4);
      '%': Value := GetValue(n + 2) MOD GetValue(n + 4);
      '/': if GetValue(n + 4)=0 then Err('Div by 0') else Value:= GetValue(n+2) div GetValue(n+4);
    else
      Err('Invalid math op: ' + Tokens[n + 3]);
    end;
    Vars[VarName] := Value;
  end
  else  
    Vars[VarName] := GetValue(n + 2);                // assign with = 
end;
     
function Condition: Boolean;
var
  LogicOp: Char;
begin
  LogicOp := Tokens[2][1];  
  case LogicOp of
    '<': Condition := GetValue(1) < GetValue(3);
    '=': Condition := GetValue(1) = GetValue(3);
    '!': Condition := GetValue(1) <>GetValue(3);
  else
    Err('Unknown logic operator: ' + LogicOp);
  end;
end;

procedure HandlePRN(const n: Byte);
begin
  for i := n to High(Tokens) do
    if Tokens[i][1] in ['B'..'R'] then Write(OutF, Vars[Tokens[i][1]])  else 
    if Tokens[i][1] in ['S'..'Z'] then Write(OutF, Chr(Vars[Tokens[i][1]])) else Err('PRN!');
end;

procedure ExecuteLine;
begin
  j:= LineNum; 
  if Tokenize(Code[LineNum]) <> 'OK' then Err('error in line ' + IntToStr(LineNum));
  Inc(LineNum);
  if Tokens[0][1] = '.' then Exit;

  case Tokens[0] of
    'IF': if Condition then
            case Tokens[4] of
              'JMP': begin Stack := LineNum; LineNum := GetLabelAddr(Tokens[5]); end;
              'PRN': HandlePRN(5);
              'RET': if Stack = 0 then Err('No RET available') else LineNum := Stack;
              else SetValue(4);
            end;
    'JMP': begin Stack:= LineNum; LineNum := GetLabelAddr(Tokens[1]); end;
    'PRN': HandlePRN(1);
    'RET': if Stack = 0 then Err('No RET available') else LineNum := Stack;
    else   SetValue(0);
  end;

  Inc(Counter);
  if Counter > 999999 then Err('Potential endless loop detected');
end;

procedure Init;
begin
  Randomize;
  Vars['R'] := 99; // Random cap
  Vars['S'] := 32; // Space (as ASCII)
  Vars['T'] := 10; // Newline (as ASCII)
  SetLength(Code, 0);
  SetLength(Labels, 0);
end;

procedure LoadProgram;
begin
  Init;
  
  while not Eof(InF) do
  begin
    ReadLn(InF, Line);
    Line := UpCase(Trim(Copy(Line, 1, Pos(';', Line + ';') - 1))); // Remove comments
    if Line = '' then Continue;
    if Line = 'TRC' then Trace := True
    else
    begin
      if Line[1] = '.' then SetLabelAddr(Line, LineNum + 1);
      Inc(LineNum);
      SetLength(Code, LineNum + 1);
      Code[LineNum] := Line;
    end;
  end;
end;

begin
  if ParamStr(1) = '' then
  begin
    writeln('No input file. Usage: nanoXL.exe <YourScript>');
    Halt(1);
  end;

  Assign(InF, ParamStr(1));
  Reset(InF);
  Assign(OutF, ParamStr(1) + '.out');
  Rewrite(OutF);

  LoadProgram;
  LineNum := 1;
  while (LineNum <= High(Code)) do ExecuteLine;

  if Trace then PrintState;
  Close(OutF);
end. // 297

