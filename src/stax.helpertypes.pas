unit stax.helpertypes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface
uses
  SysUtils;

type
  ENoValueSetException = class(Exception);
  TNoneType = record
  end;

  { TOptional }

  generic TOptional<T> = record
  public type
    PData = ^T;
    TSpecializedOptional = specialize TOptional<T>;
  private
    FHasValue: Boolean;
    FValue: T;

    class operator Initialize(var opt: TSpecializedOptional);
  public
    function Mutable: PData;
    function Value: T; {$IFDEF INLINING}inline;{$ENDIF}
    function GetOrDefault(constref DefaultValue: T): T;{$IFDEF INLINING}inline;{$ENDIF}
    function HasValue: Boolean; {$IFDEF INLINING}inline;{$ENDIF}

    constructor Create(constref AValue: T);
    class function Empty: TSpecializedOptional; static;

    class operator :=(constref AValue: T): TSpecializedOptional; {$IFDEF INLINING}inline;{$ENDIF}
    class operator :=(const None: TNoneType): TSpecializedOptional; {$IFDEF INLINING}inline;{$ENDIF}
    class operator :=(constref AOpt: TSpecializedOptional): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator not(constref AOpt: TSpecializedOptional): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
  end;

  TUnionType = (utNone, utFirst, utSecond);

  { TUnion }

  generic TUnion<TFirst, TSecond> = record
  public type
    TSpecializedUnion = specialize TUnion<TFirst, TSecond>;
    PFirst = ^TFirst;
    PSecond = ^TSecond;
  private
    FType: TUnionType;
    FFirst: TFirst;
    FSecond: TSecond;

    class operator Initialize(var union: TSpecializedUnion);
  public
    function GetType: TUnionType; {$IFDEF INLINING}inline;{$ENDIF}
    function HasValue: Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    function isFirst: Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    function isSecond: Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    function FirstMutable: PFirst;
    function First: TFirst; {$IFDEF INLINING}inline;{$ENDIF}
    function FirstOrDefault(constref DefaultValue: TFirst): TFirst;{$IFDEF INLINING}inline;{$ENDIF}
    function SecondMutable: PSecond;
    function Second: TSecond; {$IFDEF INLINING}inline;{$ENDIF}
    function SecondOrDefault(constref DefaultValue: TSecond): TSecond;{$IFDEF INLINING}inline;{$ENDIF}


    constructor FromFirst(constref AValue: TFirst);
    constructor FromSecond(constref AValue: TSecond);
    class function Empty: TSpecializedUnion; static;

    class operator :=(constref AValue: TFirst): TSpecializedUnion; {$IFDEF INLINING}inline;{$ENDIF}
    class operator :=(constref AValue: TSecond): TSpecializedUnion; {$IFDEF INLINING}inline;{$ENDIF}
    class operator :=(const None: TNoneType): TSpecializedUnion; {$IFDEF INLINING}inline;{$ENDIF}
    class operator =(constref RHS: TSpecializedUnion; LHS: TUnionType): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator =(constref RHS: TUnionType; LHS: TSpecializedUnion): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator <>(constref RHS: TSpecializedUnion; LHS: TUnionType): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator <>(constref RHS: TUnionType; LHS: TSpecializedUnion): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator :=(constref AOpt: TSpecializedUnion): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    class operator not(constref AOpt: TSpecializedUnion): Boolean; {$IFDEF INLINING}inline;{$ENDIF}
  end;

  { TPair }

  generic TPair<TFirst, TSecond> = record
    First: TFirst;
    Second: TSecond;

    constructor Create(AFirst: TFirst; ASecond: TSecond);
  end;

function EmptyOptional: TNoneType;
function EmptyUnion: TNoneType;
generic function Pair<TFirst, TSecond>(AFirst: TFirst; ASecond: TSecond): specialize TPair<TFirst, TSecond>;

implementation

{$Warnings OFF}function EmptyOptional: TNoneType;
begin
  //noop
end;

function EmptyUnion: TNoneType;
begin
  //noop
end;
{$Warnings ON}

generic function Pair<TFirst, TSecond>(AFirst: TFirst; ASecond: TSecond): specialize TPair<TFirst, TSecond>;
begin
  Result.First := AFirst;
  Result.Second := ASecond;
end;

{ TPair }

constructor TPair.Create(AFirst: TFirst; ASecond: TSecond);
begin
  First := AFirst;
  Second := ASecond;
end;

{ TUnion }

class operator TUnion.Initialize(var union: TSpecializedUnion);
begin
  union.FType:=utNone;
  union.FFirst := Default(TFirst);
  union.FSecond := Default(TSecond);
end;

function TUnion.GetType: TUnionType;
begin
  Result := FType;
end;

function TUnion.HasValue: Boolean;
begin
  Result := FType <> utNone;
end;

function TUnion.isFirst: Boolean;
begin
  Result := FType = utFirst;
end;

function TUnion.isSecond: Boolean;
begin
  Result := FType = utSecond;
end;

function TUnion.FirstMutable: PFirst;
begin
  if not isFirst then
    raise ENoValueSetException.Create('No first value set in this Union');
  Result := @FFirst;
end;

function TUnion.First: TFirst;
begin
  Result := FirstMutable^;
end;

function TUnion.FirstOrDefault(constref DefaultValue: TFirst): TFirst;
begin
  if isFirst then
    Result := FFirst
  else
    Result := DefaultValue;
end;

function TUnion.SecondMutable: PSecond;
begin
  if not isSecond then
    raise ENoValueSetException.Create('No second value set in this Union');
  Result := @FSecond;
end;

function TUnion.Second: TSecond;
begin
  Result := SecondMutable^;
end;

function TUnion.SecondOrDefault(constref DefaultValue: TSecond): TSecond;
begin
  if isSecond then
    Result := FSecond
  else
    Result := DefaultValue;
end;

constructor TUnion.FromFirst(constref AValue: TFirst);
begin
  FType := utFirst;
  FFirst := AValue;
  FSecond := Default(TSecond);
end;

constructor TUnion.FromSecond(constref AValue: TSecond);
begin
  FType := utSecond;
  FFirst := Default(TFirst);
  FSecond := AValue;
end;

class function TUnion.Empty: TSpecializedUnion;
begin
  Result.FType := utNone;
  Result.FFirst := Default(TFirst);
  Result.FSecond := Default(TSecond);
end;

class operator TUnion.:=(constref AValue: TFirst): TSpecializedUnion;
begin
  Result := TSpecializedUnion.FromFirst(AValue);
end;

class operator TUnion.:=(constref AValue: TSecond): TSpecializedUnion;
begin
  Result := TSpecializedUnion.FromSecond(AValue);
end;

class operator TUnion.:=(const None: TNoneType): TSpecializedUnion;
begin
  Result := TSpecializedUnion.Empty;
end;

class operator TUnion.=(constref RHS: TSpecializedUnion; LHS: TUnionType
  ): Boolean;
begin
  Result := RHS.FType = LHS;
end;

class operator TUnion.=(constref RHS: TUnionType; LHS: TSpecializedUnion
  ): Boolean;
begin
  Result := RHS = LHS.FType;
end;

class operator TUnion.<>(constref RHS: TSpecializedUnion; LHS: TUnionType
  ): Boolean;
begin
  Result := RHS.FType <> LHS;
end;

class operator TUnion.<>(constref RHS: TUnionType; LHS: TSpecializedUnion
  ): Boolean;
begin
  Result := RHS <> LHS.FType;
end;

class operator TUnion.:=(constref AOpt: TSpecializedUnion): Boolean;
begin
  Result := AOpt.HasValue;
end;

class operator TUnion.not(constref AOpt: TSpecializedUnion): Boolean;
begin
  result := not AOpt.HasValue;
end;

{ TOptional }

class operator TOptional.Initialize(var opt: TSpecializedOptional);
begin
  opt.FHasValue := False;
  opt.FValue := Default(T);
end;

function TOptional.Mutable: PData;
begin
  if not FHasValue then
    raise ENoValueSetException.Create('Trying to access an empty optional');
  Result := @FValue;
end;

function TOptional.Value: T;
begin
  Result := Mutable^;
end;

function TOptional.GetOrDefault(constref DefaultValue: T): T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := DefaultValue;
end;

function TOptional.HasValue: Boolean;
begin
  Result := FHasValue;
end;

constructor TOptional.Create(constref AValue: T);
begin
  FHasValue := True;
  FValue := AValue;
end;

class function TOptional.Empty: TSpecializedOptional;
begin
  Result.FHasValue := False;
  Result.FValue := Default(T);
end;

class operator TOptional.:=(constref AValue: T): TSpecializedOptional;
begin
  Result := TSpecializedOptional.Create(AValue);
end;

class operator TOptional.:=(const None: TNoneType): TSpecializedOptional;
begin
  Result := TSpecializedOptional.Empty;
end;

class operator TOptional.:=(constref AOpt: TSpecializedOptional): Boolean;
begin
  Result := AOpt.HasValue;
end;

class operator TOptional.not(constref AOpt: TSpecializedOptional): Boolean;
begin
  Result := not AOpt.FHasValue;
end;

end.

