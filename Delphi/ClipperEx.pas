unit ClipperEx;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  20 November 2018                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2017                                         *
* Purpose   :  Remove micro self-intersections and rechecks path orientation   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$IFDEF FPC}
  {$DEFINE INLINING}
{$ELSE}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
  {$IF CompilerVersion >= 18}         //Delphi 2007
    //While Inlining has been supported since D2005, both D2005 and D2006
    //have an Inline codegen bug (QC41166) so ignore Inline until D2007.
    {$DEFINE INLINING}
    {$IF CompilerVersion >= 25.0}     //Delphi XE4+
      {$LEGACYIFEND ON}
    {$IFEND}
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Clipper;

type
  PScanLine = ^TScanLine;
  TScanLine = record
    X          : Int64;
    Next       : PScanLine;
  end;

  PLocalMin = ^TLocalMin;

  PActive2 = ^TActive2;
  TActive2 = record
    currY      : Int64;
    op         : TOutPt;
    top        : TPoint64;
    dy         : double;
    outRec     : TOutRec;
    lm         : PLocalMin;
    goingFwd   : Boolean;
    next       : PActive2;
    prev       : PActive2;
    nextVert   : PActive2;
  end;

  TLocalMin = record
    X          : Int64;
    op         : TOutPt;
    outRec     : TOutRec;
    aTop       : PActive2;
    aBot       : PActive2;
  end;

  TClipperEx = class(TClipper)
  private
    FLocMinList: TList;
    FLocMinIdx : integer;
    FScanLine  : PScanLine;
    FActives2  : PActive2;
    FVert      : PActive2;
    FOutRec    : TOutRec;
    FRedo      : Boolean;
    function UpdateActive(a: PActive2; X: Int64): Boolean;
    procedure InsertScanLine(const X: Int64);
    function PopScanLine(out X: Int64): Boolean;
    function PopLocalMin(X: Int64;
      out localMin: PLocalMin): Boolean;
    procedure InsertIntoAel1(a: PActive2);
    procedure InsertIntoAel2(a, a2: PActive2);
    procedure SplitPath(op1, op2: TOutPt);
    procedure InsertLocalMinIntoAEL(const X: Int64);
    function PrepareSweep(idx: Integer): Boolean;
    function ProcessSweep: Boolean;
    procedure SwapPositionsInAEL(a1, a2: PActive2);
    function ProcessVertical(vert: PActive2; X : Int64): Boolean;
    function ProcessHorizontal(X : Int64): Boolean;
    function AddLocMinOp(op: TOutPt; outRec: TOutRec): Boolean;
    procedure GetLocMins;
    procedure PushVert(a: PActive2); {$IFDEF INLINING} inline; {$ENDIF}
    function PopVert(out a: PActive2): Boolean;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure DisposeActive2(a: PActive2);
    function DoFix(a1, a2: PActive2): Boolean;
    procedure CleanUpPath;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute(clipType: TClipType; out closedPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; override;
    function Execute(clipType: TClipType; out closedPaths, openPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; override;
    function Execute(clipType: TClipType;
      var polytree: TPolyTree; out openPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; override;
  end;

  EClipperExLibException = class(Exception);

resourcestring
  rsClipperEx_error = 'ClipperEx error';

implementation

uses Math;

const
  HORIZONTAL = 0;
  VERTICAL   = NegInfinity;
  ROUNDING_TOL: Int64 = 1;
  DIST_SQ_TOL = 2.0;

type
  TOrientation = (oNone, oCW, oCCW);

{$WARN SYMBOL_PLATFORM OFF}

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function PointCount(op: TOutPt): Integer; {$IFDEF INLINING} inline; {$ENDIF}
var
  p: TOutPt;
begin
  Result := 0;
  if not Assigned(op) then Exit;
  p := op;
  repeat
    Inc(Result);
    p := p.Next;
  until p = op;
end;
//------------------------------------------------------------------------------

function OpToPath(op: TOutPt): TPath;
var
  i, cnt: integer;
begin
  result := nil;
  cnt := PointCount(op);
  if cnt < 3 then Exit;
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
  begin
    Result[i] := op.Pt;
    op := op.Next;
  end;
end;
//------------------------------------------------------------------------------

function ReBuildPaths(OutRecList: TList): TPaths;
var
  i, j: integer;
  outRec: TOutRec;
begin
  j := 0;
  setLength(result, OutRecList.Count);
  for i := 0 to OutRecList.Count -1 do
  begin
    outRec := TOutRec(OutRecList[i]);
    if outRec.Pts = nil then continue;
    result[j] := OpToPath(outRec.pts);
    if assigned(result[j]) then inc(j);
  end;
  setLength(result, j);
end;
//------------------------------------------------------------------------------

procedure RaiseError; {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipperExLibException.Create(rsClipperEx_error);
end;
//------------------------------------------------------------------------------

function IsPolygon(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
    result := Assigned(op) and (op <> op.Next) and (op.Prev <> op.Next);
end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPoint64): Double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dx, dy: Double;
begin
  dx := (pt1.X - pt2.X);
  dy := (pt1.Y - pt2.Y);
  result := (dx*dx + dy*dy);
end;
//------------------------------------------------------------------------------

function PointsAreClose(const pt1, pt2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := DistanceSqrd(pt1, pt2) <= DIST_SQ_TOL;
end;
//------------------------------------------------------------------------------

function DistanceFromSegmentSqrd(const pt, seg1, seg2: TPoint64): double;
var
  q, dx, dy, onSegX, onSegY: double;
begin
  if (seg1.X = seg2.X) and (seg1.Y = seg2.Y) then RaiseError;

  dx := seg2.X-seg1.X; dy := seg2.Y-seg1.Y;
  q := ((pt.X-seg1.X)*dx + (pt.Y-seg1.Y)*dy) / (sqr(dx) + sqr(dy));
  if q < 0 then q := 0; if q > 1 then q := 1;
  onSegX := (1 - q) * seg1.X + (q * seg2.X);
  onSegY := (1 - q) * seg1.Y + (q * seg2.Y);
  result := DistanceSqrd(pt, Point64(onSegX, onSegY));
end;
//------------------------------------------------------------------------------

function PointIsCloseToSegment(const pt, seg1, seg2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := DistanceFromSegmentSqrd(pt, seg1, seg2) <= DIST_SQ_TOL;
end;
//------------------------------------------------------------------------------

function IsVertical(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.dy = VERTICAL;
end;
//------------------------------------------------------------------------------

function GetPrevOp(a: PActive2): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if a.goingFwd then result := a.op.Prev else result := a.op.Next;
end;
//------------------------------------------------------------------------------

function GetPrevOp(op: TOutPt; goingForward: Boolean): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if goingForward then result := op.Prev else result := op.Next;
end;
//------------------------------------------------------------------------------

function GetNextOp(op: TOutPt; goingForward: Boolean): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if goingForward then result := op.Next else result := op.Prev;
end;
//------------------------------------------------------------------------------

function GetNextOp(a: PActive2): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := GetNextOp(a.op, a.goingFwd);
end;
//------------------------------------------------------------------------------

function GetNextNextOp(a: PActive2): TOutPt; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if a.goingFwd then result := a.op.Next.Next else result := a.op.Prev.Prev;
end;
//------------------------------------------------------------------------------

function IsHardMinima(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.lm.op = a.op;
end;
//------------------------------------------------------------------------------

function IsSoftMinima(a: PActive2; X: Int64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (X = a.lm.X) and (a.op.Pt.X = X); //accommodates vertical minima
end;
//------------------------------------------------------------------------------

function IsHardMaxima(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
var
  op: TOutPt;
begin
  if a.goingFwd then //this returns a hard maxima
  begin
    op := GetNextNextOp(a);
    while (op <> a.op) and (op.Pt.X = a.top.X) do op := op.Next;
    result := (op <> a.op) and (op.Pt.X < a.top.X);
  end else
    result := a.op.Prev.Prev.Pt.X < a.top.X;
end;
//------------------------------------------------------------------------------

function IsSoftMaxima(a: PActive2): Boolean;
var
  op: TOutPt;
  X: Int64;
begin
  X := a.top.X;
  //result will also be true with vertical maxima ...
  if a.goingFwd then
  begin
    op := a.op.Next.Next;
    while (op.Pt.X = X) do op := op.Next;
  end else
  begin
    op := a.op.Prev.Prev;
    while (op.Pt.X = X) do op := op.Prev;
  end;
  result := (op.Pt.X < X);
end;
//------------------------------------------------------------------------------

function GetDy(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dx: Int64;
begin
  dx := (pt2.X - pt1.X);
  if dx = 0 then result := VERTICAL
  else result := (pt1.Y - pt2.Y)/dx;
end;
//------------------------------------------------------------------------------

function TopY(a: PActive2; const X: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (X >= a.top.X) or (a.dy = HORIZONTAL) then Result := a.top.Y
  else if (X <= a.op.Pt.X) then Result := a.op.Pt.Y
  else Result := a.op.Pt.Y + Round(a.dy*(a.op.Pt.X - X));
end;
//------------------------------------------------------------------------------

function TopY(const pt1, pt2: TPoint64; const X: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dy: double;
begin
  if (X = pt1.X) then Result := pt1.Y
  else if (X = pt2.X) then Result := pt2.Y
  else
  begin
    dy := GetDy(pt1, pt2);
    if (dy = VERTICAL) or (dy = HORIZONTAL) then Result := pt2.Y
    else Result := pt1.Y + Round(dy * (pt1.X - X));
  end;
end;
//------------------------------------------------------------------------------

procedure SetDy(a: PActive2); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  a.dy := GetDy(a.op.Pt, a.top);
end;
//------------------------------------------------------------------------------

procedure SetDy(a: PActive2; X: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  a.dy := GetDy(a.op.Pt, a.top);
  a.currY := TopY(a, X);
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
begin
  //sort in X ascending order.
  //nb: result > 0 will arange item2 before item1
  result := PLocalMin(item1).X - PLocalMin(item2).X;
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: TOutPt): TOutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  //nb: this function is used before LocMins have been created
  result := op.Prev;
  op.Prev.Next := op.Next;
  op.Next.Prev := op.Prev;
  op.Free;
end;
//------------------------------------------------------------------------------

function CheckDisposeOutPt(op: TOutPt; outRec: TOutRec): TOutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (op = outRec.pts) then outRec.pts := op.Prev;
  result := DisposeOutPt(op);
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(op: TOutPt);  {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: TOutPt;
begin
  op.Prev.Next := nil;
  while Assigned(op) do
  begin
    tmpPp := op;
    op := op.Next;
    tmpPp.Free;
  end;
end;
//------------------------------------------------------------------------------

function IsTriangle(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := op.Next.Next.Next = op;
end;
//------------------------------------------------------------------------------

function IsSmallTriangle(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := IsTriangle(op) and
    ((DistanceSqrd(op.Pt, op.Prev.Pt) = 1) or
    (DistanceSqrd(op.Pt, op.Next.Pt) = 1) or
    (DistanceSqrd(op.Prev.Pt, op.Next.Pt) = 1));
end;
//------------------------------------------------------------------------------

function TurnsLeft(const pt1, pt2, pt3: TPoint64): boolean;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt2.X - pt3.X;
  y2 := pt2.Y - pt3.Y;
  result := (x1 * y2 - y1 * x2) > 0; //cross-product
end;
//---------------------------------------------------------------------------

function TurnsLeftEqual(const pt1, pt2, pt3: TPoint64): boolean;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt2.X - pt3.X;
  y2 := pt2.Y - pt3.Y;
  result := (x1 * y2 - y1 * x2) >= 0; //cross-product
end;
//---------------------------------------------------------------------------

function GetIntersectPoint(a1, a2: PActive2): TPoint64;
var
  b1, b2, m: Double;
begin
  if a1.dy = HORIZONTAL then
  begin
    Result.Y := a1.op.Pt.Y;
    if IsVertical(a2) then
      Result.X := a2.op.Pt.X
    else
    begin
      with a2^ do b2 := op.Pt.X + op.Pt.Y / dy;
      Result.X := round(Result.Y / -a2.dy + b2);
    end;
  end
  else if a2.dy = HORIZONTAL then
  begin
    Result.Y := a2.op.Pt.Y;
    if IsVertical(a1) then
      Result.X := a1.op.Pt.X
    else
    begin
      with a1^ do b1 := op.Pt.X + op.Pt.Y / dy;
      Result.X := round(Result.Y / -a1.dy + b1);
    end;
  end
  else if IsVertical(a1) then
  begin
    Result.X := a1.top.X;
    b2 := a2.op.Pt.Y + a2.op.Pt.X * a2.dy;
    Result.Y := round(-a2.dy * Result.X + b2);
  end
  else if IsVertical(a2) then
  begin
    Result.X := a2.top.X;
    b1 := a1.op.Pt.Y + a1.op.Pt.X * a1.dy;
    Result.Y := round(-a1.dy * Result.X + b1);
  end else
  begin
    b1 := a1.op.Pt.Y + a1.op.Pt.X * a1.dy;
    b2 := a2.op.Pt.Y + a2.op.Pt.X * a2.dy;
    m := (b2-b1)/(a2.dy - a1.dy);
    Result.X := round(m);
    if Abs(a1.dy) < Abs(a2.dy) then
      Result.Y := round(-a1.dy * m + b1) else
      Result.Y := round(-a2.dy * m + b2);
  end;
end;
//------------------------------------------------------------------------------

function Orientation(lm: PLocalMin): TOrientation;
var
  opTop, opBot: TOutPt;
  y: Int64;
begin
  y := 0;
  if IsSmallTriangle(lm.op) then Exit(oNone);
  opTop := lm.op.Next;
  opBot := lm.op.Prev;
  while (opTop.Pt.X >= opTop.Prev.Pt.X) and
    (opBot.Pt.X >= opBot.Next.Pt.X) do
  begin
    if opTop = opBot then Exit(oNone);
    if opTop.Pt.X = opBot.Pt.X then
    begin
      y := opBot.Pt.Y - opTop.Pt.Y;
      if y <> 0 then break;
      opTop := opTop.Next;
      opBot := opBot.Prev;
    end
    else if opTop.Pt.X < opBot.Pt.X then
    begin
      y := TopY(lm.op.Pt, opBot.Pt, opTop.Pt.X) - opTop.Pt.Y;
      if y <> 0 then break;
      opTop := opTop.Next;
    end else
    begin
      y := opBot.Pt.Y - TopY(lm.op.Pt, opTop.Pt, opBot.Pt.X);
      if y <> 0 then break;
      opBot := opBot.Prev;
    end;
  end;
  if y = 0 then
  begin
    //one edge has reached its maxima ...
    if (opTop.Pt.X < opTop.Prev.Pt.X) then
    begin
      if TurnsLeft(opTop.Prev.Prev.Pt, opTop.Prev.Pt, opTop.Pt) then
        result := oCW else result := oCCW;
    end else
    begin
      if TurnsLeft(opBot.Pt, opBot.Next.Pt, opBot.Next.Next.Pt) then
        result := oCW else result := oCCW;
    end;
  end
  else if y > 0 then result := oCW
  else result := oCCW;
end;
//------------------------------------------------------------------------------

function CreateActive(lm: PLocalMin; goingForward: Boolean) : PActive2;
begin
  new(result);
  result.op := lm.op;
  result.goingFwd := goingForward;
  result.outRec := lm.outRec;
  Result.lm := lm;
  if goingForward then
  begin
    Result.top := lm.op.Next.Pt;
    lm.aTop := Result;
  end else
  begin
    Result.top := lm.op.Prev.Pt;
    lm.aBot := Result;
  end;
  result.currY := lm.op.Pt.Y;
  SetDy(result);
end;
//------------------------------------------------------------------------------

procedure CheckFixDupsColinear(var op: TOutPt; outRec: TOutRec);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  while op.Prev <> op.Next do //loop while removing OutPts
  begin
    if PointsEqual(op.Pt, op.Next.Pt) or //duplicate or colinear
    ((op.Pt.X = op.Next.Pt.X) and (op.Pt.X = op.Prev.Pt.X)) or   //vert colinear
    ((op.Pt.Y = op.Next.Pt.Y) and (op.Pt.Y = op.Prev.Pt.Y)) then //horz colinear
    begin
      if op.Next = op then Exit;
      op := CheckDisposeOutPt(op, outRec);
    end
    else if PointsEqual(op.Prev.Pt, op.Next.Pt) then            //colinear spike
    begin
      op := CheckDisposeOutPt(op, outRec);
    end else
      break;
  end;
end;
//------------------------------------------------------------------------------

function InternalValidate(a1, a2: PActive2): Boolean;
var
  op, op1, op2, op1b, op2b: TOutPt;
  Y: Int64;
begin
  //precondition: a1.currY == a2.currY;
  op1b := a1.op; op2b := a2.op;
  while true do
  begin
    op1 := GetNextOp(op1b, a1.goingFwd);
    op2 := GetNextOp(op2b, a2.goingFwd);
    if (op1 = op2) then
    begin
      result := GetDy(op1b.Pt, op1.Pt) <= GetDy(op2b.Pt, op1.Pt);
      break;
    end
    else if op1.Pt.X <= op2.Pt.X then
    begin
      Y := op1.Pt.Y - TopY(op2b.Pt, op2.Pt, op1.Pt.X);
      result := Y <= 0;
      if Y <> 0 then Exit;
      if op2.Pt.X = op1.Pt.X then
      begin
        op2b := op2;
        op2 := GetNextOp(op2, a2.goingFwd);
      end;
      op1b := op1;
      op1 := GetNextOp(op1, a1.goingFwd);
    end else
    begin
      Y := TopY(op1b.Pt, op1.Pt, op2.Pt.X) - op2.Pt.Y;
      result := Y <= 0;
      if Y <> 0 then Exit;
      op2b := op2;
      op2 := GetNextOp(op2, a2.goingFwd);
    end;
    if (op1.Pt.X < op1b.Pt.X) then
    begin
      if (op1b = op2b) then Exit; //true
      op := GetPrevOp(op1b, a1.goingFwd);
      result := TurnsLeftEqual(op.Pt, op1b.Pt, op1.Pt);
      Exit;
    end else if (op2.Pt.X < op2b.Pt.X) then
    begin
      op := GetPrevOp(op2b, a2.goingFwd);
      result := TurnsLeftEqual(op2.Pt, op2b.Pt, op.Pt);
      Exit;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidOrder(a1, a2: PActive2): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if a1.currY = a2.currY then
    result := InternalValidate(a1, a2) else
    result := a1.currY < a2.currY;
end;

//------------------------------------------------------------------------------
// TClipperEx methods ...
//------------------------------------------------------------------------------

constructor TClipperEx.Create;
begin
  inherited;
  FLocMinList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipperEx.Destroy;
begin
  inherited;
  FLocMinList.Free;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.CleanUpPath;
var
  i: integer;
  Y: Int64;
begin
  //in case of abnormal termination ...
  while assigned(FScanLine) do PopScanLine(Y);
  while assigned(FActives2) do DisposeActive2(FActives2);
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMin(FLocMinList[i]));
  FLocMinList.Clear;
  FLocMinIdx := 0;
end;
//------------------------------------------------------------------------------

function TClipperEx.AddLocMinOp(op: TOutPt; outRec: TOutRec): Boolean;
var
  lm: PLocalMin;
begin
  new(lm);
  lm.X := op.Pt.X;
  lm.op := op;
  lm.outRec := outRec;
  FLocMinList.Add(lm);
  result := true;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.GetLocMins;
var
  op : TOutPt;
  goingRight: Boolean;
begin
  op := FOutRec.pts;
  //skip to the front of verticals ...
  while (op.Next <> FOutRec.pts) and (op.Next.Pt.X = op.Pt.X) do op := op.next;
  if (op.Next = FOutRec.pts) then Exit; //a flat path

  goingRight := op.next.pt.X > op.Pt.X;
  //if we start at a LocMin then add it now ...
  if (goingRight) and (FOutRec.pts.prev.pt.X > FOutRec.pts.pt.X) then
    AddLocMinOp(op, FOutRec);

  op := op.next;
  while (op <> FOutRec.pts) do
  begin
    if (goingRight) then
    begin
      while (op <> FOutRec.pts) and (op.next.pt.X >= op.pt.X) do op := op.next;
      if (op = FOutRec.pts) then break;
      goingRight := false;
    end else
    begin
      while (op <> FOutRec.pts) and (op.next.pt.X <= op.pt.X) do op := op.next;
      if (op = FOutRec.pts) then break;
      AddLocMinOp(op, FOutRec);
      goingRight := true;
    end;
    op := op.next;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.PopLocalMin(X: Int64;
  out localMin: PLocalMin): Boolean;
begin
  Result := FLocMinIdx < FLocMinList.Count;
  if not result then exit;
  localMin := PLocalMin(FLocMinList[FLocMinIdx]);
  if (localMin.X = X) then
    inc(FLocMinIdx) else
    Result := false;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.InsertScanLine(const X: Int64);
var
  newSl, sl: PScanLine;
begin
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.X := X;
    FScanLine := newSl;
    newSl.Next := nil;
  end else if X < FScanLine.X then
  begin
    new(newSl);
    newSl.X := X;
    newSl.Next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.Next) and (X >= sl.Next.X) do sl := sl.Next;
    if X = sl.X then Exit;
    new(newSl);
    newSl.X := X;
    newSl.Next := sl.Next;
    sl.Next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.PrepareSweep(idx: Integer): Boolean;
var
  i: integer;
  op: TOutPt;
begin
  result := false;
  FOutRec := OutRecList[idx];
  if (FOutRec.State = orOpen) or not assigned(FOutRec.pts) then Exit;
  op := FOutRec.pts;
  FActives2 := nil;

  //strip duplicates and vertical colinears ...
  repeat
    CheckFixDupsColinear(op, FOutRec);
    op := op.Next;
  until op = FOutRec.pts;
  CheckFixDupsColinear(op, FOutRec);

  if not IsPolygon(op) then
  begin
    DisposePolyPts(FOutRec.pts);
    FOutRec.pts := nil;
    Exit;
  end;

  GetLocMins;
  FVert := nil;
  FLocMinList.Sort(LocMinListSort);
  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMin(FLocMinList[i]).X);
  result := true;
end;
//------------------------------------------------------------------------------

function TClipperEx.PopScanLine(out X: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  X := FScanLine.X;
  sl := FScanLine;
  FScanLine := FScanLine.Next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipperEx.ProcessSweep: Boolean;
var
  X: Int64;
  a: PActive2;
begin
  Result := true;
  if not PopScanLine(X) then Exit;
  while Result and not FRedo do
  begin
    InsertLocalMinIntoAEL(X);
    while Result and not FRedo and PopVert(a) do
      Result := ProcessVertical(a, X);
    if not result or FRedo or not PopScanLine(X) then break;
    if assigned(FActives2) then
      Result := ProcessHorizontal(X);
  end;
end;
//------------------------------------------------------------------------------

procedure PerpendicularAdjust(a: PActive2; op: TOutPt);
begin
  if a.top.Y = a.op.Pt.Y then
  begin
    if TurnsLeft(a.op.Pt, a.top, op.Prev.Pt) then dec(op.Pt.Y)
    else inc(op.Pt.Y);
  end else
  begin
    if (a.op.Pt.Y > a.top.Y) = TurnsLeft(a.op.Pt, a.top, op.Prev.Pt) then
      dec(op.Pt.X) else
      inc(op.Pt.X);
  end;
end;
//------------------------------------------------------------------------------

function InsertOp(const pt: TPoint64; insertAfter: TOutPt): TOutPt;
begin
  Result := TOutPt.Create;
  Result.Pt := pt;
  Result.Next := insertAfter.Next;
  insertAfter.Next.Prev := Result;
  insertAfter.Next := Result;
  Result.Prev := insertAfter;
end;
//------------------------------------------------------------------------------

function TClipperEx.DoFix(a1, a2: PActive2): Boolean;
var
  op, op1,op2,op3,op4: TOutPt;
  d, d1, d2, d3, d4: double;
  pt: TPoint64;
begin
  FRedo := true;
  result := true;
  if not assigned(a2) then
  begin
    //a spiking minima or maxima
    if (a1.op = a1.next.op) and (a1.op = a1.lm.op) then
      CheckDisposeOutPt(a1.op, FOutRec) else             //minima
      CheckDisposeOutPt(GetNextOp(a1), FOutRec);         //maxima
  end else
  begin
    //a self-intersect ...
    op1 := a1.op; op2 := GetNextOp(a1);
    op3 := a2.op; op4 := GetNextOp(a2);
    pt := GetIntersectPoint(a1, a2);
    d1 := DistanceSqrd(op1.Pt, pt);
    d2 := DistanceSqrd(op2.Pt, pt);
    d3 := DistanceSqrd(op3.Pt, pt);
    d4 := DistanceSqrd(op4.Pt, pt);
    if min(d1,d2) > DIST_SQ_TOL then
    begin
      if a1.goingFwd then
        op1 := InsertOp(pt, op1) else
        op1 := InsertOp(pt, op2);
    end
    else if d2 < d1 then
      op1 := op2;
    if min(d3,d4) > DIST_SQ_TOL then
    begin
      if a2.goingFwd then
        op3 := InsertOp(pt, op3) else
        op3 := InsertOp(pt, op4);
    end
    else if d4 < d3 then
      op3 := op4;
    SplitPath(op1, op3);
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.UpdateActive(a: PActive2; X: Int64): Boolean;
var
  op: TOutPt;
begin
  if a.goingFwd then
  begin
    a.op := a.op.Next;
    op := a.op.Next;
  end else
  begin
    a.op := a.op.Prev;
    op := a.op.Prev;
  end;
  result := (op.Prev <> op.Next) and (op.Pt.X >= a.op.Pt.X);
  if not result then exit;

  a.top := op.Pt;
  //Maxima verticals aren't allowed when going forward. So at verticals
  //going forward, make sure that these are intermediate verticals ...
  if a.goingFwd and (a.top.X = X) then
  repeat
    op := op.Next;
  until (op = a.op) or (op.Pt.X <> X);
  result := (op <> a.op) and (op.Pt.X >= X);
  if not result then Exit;

  SetDy(a, X);
  a.currY := a.op.Pt.Y;

  if IsVertical(a) then
  begin
    PushVert(a)
  end else
  begin
    if a.top.X > X then InsertScanLine(a.top.X);
    if assigned(a.prev) and (a.prev.currY = a.currY) and
      PointsEqual(a.op.Pt, a.prev.op.Pt) then SplitPath(a.op, a.prev.op)
    else if assigned(a.next) and (a.next.currY = a.currY) and
      PointsEqual(a.op.Pt, a.next.op.Pt) then SplitPath(a.op, a.next.op);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.SwapPositionsInAEL(a1, a2: PActive2);
var
  prev, next: PActive2;
begin
  next := a2.next;
  if Assigned(next) then next.prev := a1;
  prev := a1.prev;
  if Assigned(prev) then prev.next := a2;
  a2.prev := prev;
  a2.next := a1;
  a1.prev := a2;
  a1.next := next;
  if not assigned(a2.prev) then FActives2 := a2;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.InsertIntoAel1(a: PActive2);
var
  a2: PActive2;
begin
  if not Assigned(FActives2) then
  begin
    a.next := nil;
    a.prev := nil;
    FActives2 := a;
  end
  else if IsValidOrder(a, FActives2) then
  begin
    a.prev := nil;
    a.next := FActives2;
    FActives2.prev := a;
    FActives2 := a;
  end else
  begin
    a2 := FActives2;
    while Assigned(a2.next) and IsValidOrder(a2.next, a) do
      a2 := a2.next;
    a.next := a2.next;
    if Assigned(a.next) then a.next.prev := a;
    a.prev := a2;
    a2.next := a;
  end;
end;
//----------------------------------------------------------------------

procedure TClipperEx.InsertIntoAel2(a, a2: PActive2);
begin
  a2.next := a.next;
  if Assigned(a2.next) then a2.next.prev := a2;
  a2.prev := a;
  a.next := a2;
end;
//----------------------------------------------------------------------

procedure TClipperEx.SplitPath(op1, op2: TOutPt);
var
  op3, op4: TOutPt;
  outrec: TOutRec;
begin
  op3 := op1.Next;
  op4 := op2.Next;
  op1.Next := op4; op4.Prev := op1;
  op2.Next := op3; op3.Prev := op2;

  //find out which path contains FOutRec.Pts so we can
  //add the other (new) path to OutRecList ...
  op3 := op2;
  repeat
    if op3 = FOutRec.Pts then
    begin
      op2 := op1;
      break;
    end;
    op3 := op3.Next;
  until op3 = op2;

  //now add op2 to OutRecList
  outrec := TOutRec.Create;
  outRec.Idx := OutRecList.Add(outRec);
  outRec.Pts := op2;
  outRec.PolyPath := nil;
  FRedo := true;
end;
//----------------------------------------------------------------------

procedure TClipperEx.InsertLocalMinIntoAEL(const X: Int64);
var
  lm: PLocalMin;
  aTop, aBot: PActive2;
begin
  aTop := nil; aBot := nil; //stops compiler warning

  while PopLocalMin(X, lm) do
  begin
    //nb: horizontals will always be op.prev
    case Orientation(lm) of
      oCW:
        begin
          aTop := CreateActive(lm, true);
          aBot := CreateActive(lm, false);
        end;
      oCCW:
        begin
          aTop := CreateActive(lm, false);
          aBot := CreateActive(lm, true);
        end;
      else
      begin
        //no orientation when no significant area, so dispose ...
        DisposePolyPts(lm.outRec.pts);
        lm.outRec.pts := nil;
        Continue;
      end;
    end;

    //nb: aTop CAN be vertical if it's an inner path (with CCW orientation)
    InsertIntoAel1(aTop);
    InsertIntoAel2(aTop, aBot);

    if IsVertical(aTop) then
    begin
      PushVert(aTop);
    end else
    begin
      InsertScanLine(aTop.top.X);
      //very occasionally two vertices share a local minima, and
      //whenever they do, split the polygon there ...
      if assigned(aTop.prev) and (aTop.currY = aTop.prev.currY) and
        PointsEqual(aTop.op.Pt, aTop.prev.op.Pt) then
      begin
        SplitPath(ATop.op, aTop.prev.op);
        break;
      end;
    end;
    if IsVertical(aBot) then
    begin
      PushVert(aBot);
    end else
    begin
      InsertScanLine(aBot.top.X);
      //very occasionally two vertices share a local minima, and
      //whenever they do, split the polygon there ...
      if assigned(aBot.next) and (aBot.currY = aBot.next.currY) and
        PointsEqual(aBot.op.Pt, aBot.next.op.Pt) then
      begin
        SplitPath(ABot.op, ABot.next.op); //#181
        break;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.ProcessVertical(vert: PActive2; X : Int64): Boolean;
var
  prev, next: PActive2;
begin
  Result := true;
  if (vert.top.Y < vert.currY) then
  begin
    ///////////////////////////////////////////////////////////////
    // heading up ...
    ///////////////////////////////////////////////////////////////
    while Result and assigned(vert.prev) do
    begin
      prev := vert.prev;
      if (prev.currY <= vert.top.Y) then break;

      if IsVertical(prev) or IsSoftMaxima(vert) or IsSoftMinima(prev, X) then
      begin
        //overlapping verticals (todo - join edges later)
        //or vertical edge passing over or under a minima or maxima
        SwapPositionsInAEL(prev, vert);
      end else
      begin
        Result := DoFix(prev, vert);
        Exit;
      end;
    end;
  end else
  begin
    ///////////////////////////////////////////////////////////////
    // heading down ...
    ///////////////////////////////////////////////////////////////
    while result and assigned(vert.next) do
    begin
      next := vert.next;
      if (next.currY >= vert.top.Y) then break;

      if IsVertical(next) or IsSoftMaxima(vert) or IsSoftMinima(next, X) then
      begin
        SwapPositionsInAEL(vert, next);
      end else
      begin
        Result := DoFix(vert, next);
        Exit;
      end;
    end;
    ///////////////////////////////////////////////////////////////
  end;

  if not UpdateActive(vert, X) then DisposeActive2(vert);
end;
//------------------------------------------------------------------------------

function TClipperEx.ProcessHorizontal(X : Int64): Boolean;
var
  a, next: PActive2;
begin
  Result := true;
  a := FActives2;
  a.currY := TopY(a, X);
  while assigned(a) do
  begin
    next := a.next;
    if assigned(next) then
    begin
      next.currY := TopY(next, X);

      if (a.top.X <= X) or (next.top.X <= X) then
      begin
        if (a.currY = next.currY) then
        begin
          if (a.op = next.op) and (a.op = a.lm.op) then
          begin
            Result := DoFix(a, nil);             //minima spike #131 #133 #136
            Exit;
          end
          else if (a.top.X > X) and
            IsHardMaxima(a) and (GetNextOp(a) = GetNextOp(next)) then
          begin
            Result := DoFix(a, nil);             //maxima spike
            Exit;
          end;
        end
        else if (a.currY > next.currY) then      //rounding issue ...
        begin
          Result := DoFix(a, next);
          Exit;
        end;
      end;
    end;

    if not result then Exit;
    if (a.top.X <= X) then
    begin
      if IsVertical(a) and (a.top.X = X) then PushVert(a)
      else if not UpdateActive(a, X) then DisposeActive2(a);
    end;
    a := next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.PushVert(a: PActive2);
begin
  if assigned(FVert) then
    a.nextVert := FVert else
    a.nextVert := nil;
  FVert := a;
end;
//------------------------------------------------------------------------------

function TClipperEx.PopVert(out a: PActive2): Boolean;
begin
  Result := assigned(FVert);
  if not Result then Exit;
  a := FVert;
  FVert := FVert.nextVert;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.DisposeActive2(a: PActive2);
begin
  if (a = FActives2) and (a.next = nil) then
  begin
    Dispose(a);
    FActives2 := nil;
    Exit;
  end;
  if (a = FActives2) then FActives2 := a.next;
  if assigned(a.prev) then a.prev.next := a.next;
  if assigned(a.next) then a.next.prev := a.prev;
  if a = a.lm.aTop then a.lm.aTop := nil
  else a.lm.aBot := nil;
  Dispose(a);
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType; out closedPaths: TPaths;
  fillRule: TFillRule): Boolean;
var
  i: integer;
begin
  try try
    Result := ExecuteInternal(clipType, fillRule);
    i := 0;
    while Result and (i < OutRecList.Count) do
    begin
      FRedo := false;
      if PrepareSweep(i) then
      begin
        result := ProcessSweep;
        if not FRedo then inc(i);
      end
      else inc(i);
      CleanUpPath;
    end;
    closedPaths := ReBuildPaths(OutRecList);
  except
    Result := false;
    //DebugString := IntToStr(i);
  end;
  finally
    CleanUpPath;
    CleanUp; //inherited;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType; out closedPaths, openPaths: TPaths;
  fillRule: TFillRule = frEvenOdd): Boolean;
//var
//  i: integer;
begin
  Result := false;
//  closedPaths := nil;
//  openPaths := nil;
//  try try
//    Result := inherited Execute(clipType, closedPaths, fillRule);
//    if not Result then Exit;
//    BuildOutPaths(closedPaths, FOutList);
//    for i := 0 to FOutList.Count -1 do
//    begin
//      if not PrepareSweep(i) then Continue;
//      ProcessSweep;
//      CleanUpPath;
//    end;
//    closedPaths := ReBuildPaths(FOutList);
//  except
//    Result := false;
//  end;
//  finally
//    FinalCleanUp;
//  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType; var polytree: TPolyTree;
  out openPaths: TPaths; fillRule: TFillRule): Boolean;
//var
//  i: integer;
begin
  Result := false;
//  if not assigned(polytree) then
//    raise EClipperLibException.Create(rsClipper_PolyTreeErr);
//  polytree.Clear;
//  openPaths := nil;
//  try try
//    Result := false;
////    Result := inherited Execute(clipType, polytree, openPaths, fillRule);
////    if not Result then Exit;
////    for i := 0 to FOpList.Count -1 do
////    begin
////      if not PrepareSweep(i) then Continue;
////      ProcessSweep;
////      CleanUpLocMins;
////    end;
//  except
//    Result := false;
//  end;
//  finally
//    FinalCleanUp;
//  end;
end;
//------------------------------------------------------------------------------

initialization
finalization
end.
