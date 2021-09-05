unit stax.helpertypes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$IfDef RELEASE}
{$define inlining}
{$EndIf}

interface
uses
  SysUtils, gvector;

type
  ENoValueSetException = class(Exception);
  TNoneType = record
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

  EIndexOutOfBoundException = class(Exception);

  { TMinHeap }

  generic TMinHeap<T, Comparator> = class
  public type
    PT = ^T;
    TDataList = specialize TVector<T>;
  private
    FData: TDataList;
    function IsLeaf(AIndex: SizeInt): Boolean; inline;
    class function IsRoot(AIndex: SizeInt): Boolean; static; inline;
    class function Parent(AIndex: SizeInt): SizeInt; static; inline;
    class function LChild(AIndex: SizeInt): SizeInt; static; inline;
    class function RChild(AIndex: SizeInt): SizeInt; static; inline;

    procedure Swap(AIndex: SizeInt; BIndex: SizeInt);
    procedure Heapify(AIndex: SizeInt);
  public
    constructor Create;
    destructor Destroy; override;

    function Insert(constref AElem: T): SizeInt;
    procedure Delete(AIndex: SizeInt);
    procedure Pop; inline;

    function Empty: Boolean; inline;

    function First: PT; inline;
    function ExtractFirst: T;

    property Elements: TDataList read FData;
  end;

function EmptyUnion: TNoneType;
generic function Pair<TFirst, TSecond>(AFirst: TFirst; ASecond: TSecond): specialize TPair<TFirst, TSecond>;

implementation

{$Push}{$Warnings OFF}
function EmptyUnion: TNoneType;
begin
  //noop
end;
{$Pop}

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

{ TMinHeap }

function TMinHeap.IsLeaf(AIndex: SizeInt): Boolean;
begin
  Result := AIndex >= (FData.Size div 2);
end;

class function TMinHeap.IsRoot(AIndex: SizeInt): Boolean;
begin
  Result := AIndex = 0;
end;

class function TMinHeap.Parent(AIndex: SizeInt): SizeInt;
begin
  Result := (AIndex - 1) div 2;
end;

class function TMinHeap.LChild(AIndex: SizeInt): SizeInt;
begin
  Result := AIndex * 2 + 1;
end;

class function TMinHeap.RChild(AIndex: SizeInt): SizeInt;
begin
  Result := AIndex * 2 + 2;
end;

constructor TMinHeap.Create;
begin
  FData := TDataList.Create;
end;

destructor TMinHeap.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TMinHeap.Swap(AIndex: SizeInt; BIndex: SizeInt);
var
  tmp: T;
begin
  // Use move to avoid overhead of managed types
  Move(FData.Mutable[AIndex]^, tmp, SizeOf(T));
  Move(FData.Mutable[BIndex]^, FData.Mutable[AIndex]^, SizeOf(T));
  Move(tmp, FData.Mutable[BIndex]^, SizeOf(T));
end;

procedure TMinHeap.Heapify(AIndex: SizeInt);
var
  SwapChild: SizeInt;
begin
  // use pointer access so constref can be used in the comparator
  while not IsLeaf(AIndex) do
  begin
    if Comparator.C(FData.Mutable[LChild(AIndex)]^, FData.Mutable[RChild(AIndex)]^) then
      SwapChild := LChild(AIndex)
    else
      SwapChild := RChild(AIndex);
    // if we reach the point where we are smaller than the smallest child
    // we are finished
    if Comparator.C(FData.Mutable[AIndex]^, FData.Mutable[SwapChild]^) then
      Break;
    // Otherwise swap with child
    Swap(AIndex, SwapChild);
    // and continue checking that new place
    AIndex := SwapChild;
  end;
end;

function TMinHeap.Insert(constref AElem: T): SizeInt;
begin
  FData.PushBack(AElem);
  Result := FData.Size - 1;
  while (Result > 0) and Comparator.C(FData.Mutable[Result]^, FData.Mutable[Parent(Result)]^) do
  begin
    Swap(Result, Parent(Result));
    Result := Parent(Result);
  end;
end;

procedure TMinHeap.Delete(AIndex: SizeInt);
begin
  if (AIndex < 0) or (AIndex > FData.Size) then
    raise EIndexOutOfBoundException.Create('Index out of bounds');
  Swap(AIndex, FData.Size - 1);
  FData.PopBack;
  Heapify(AIndex);
end;

procedure TMinHeap.Pop;
begin
  Delete(0);
end;

function TMinHeap.Empty: Boolean;
begin
  Result := FData.Size = 0;
end;

function TMinHeap.First: PT;
begin
  Result := FData.Mutable[0];
end;

function TMinHeap.ExtractFirst: T;
begin
  Result := FData.Mutable[0]^;
  Self.Delete(0);
end;

end.

