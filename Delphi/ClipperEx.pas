unit ClipperEx;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (pre-alpha)                                                *
* Date      :  19 Febuary 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Removes micro self-intersections and rechecks path orientation. *
*              This module is very much a work in progress and will undergo    *
*              major revision. It also lacks functionality that will join      *
*              polygons in clipping solutions that share touching collinear    *
*              edges.                                                          *
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
  Classes, SysUtils, Math, ClipperCore, Clipper;

type

  PLocalMinEx = ^TLocalMinEx;
  TLocalMinEx = record
    Y          : Int64;
    op         : TOutPt;
    outRec     : TOutRec;
  end;

  TClipperEx = class(TClipper)
  private
    FLocMinList: TList;
    FLocMinIdx : integer;
    FScanLine  : PScanLine;
    FActives2  : PActive;
    FHorz      : PActive;
    FOutRec    : TOutRec;
    FRedo      : Boolean;
    function UpdateActive(a: PActive; Y: Int64): Boolean;
    procedure InsertScanLine(const Y: Int64);
    function PopScanLine(out Y: Int64): Boolean;
    function PopLocalMin(Y: Int64;
      out localMin: PLocalMinEx): Boolean;
    procedure InsertIntoAel1(a: PActive);
    procedure InsertIntoAel2(a, a2: PActive);
    procedure SplitPath(op1, op2: TOutPt);
    procedure InsertLocalMinIntoAEL(const Y: Int64);
    function PrepareSweep(idx: Integer): Boolean;
    function DoSweep: Boolean;
    procedure SwapPositionsInAEL(a1, a2: PActive);
    function DoHorizontal(horz: PActive; Y : Int64): Boolean;
    function DoScanbeam(Y : Int64): Boolean;
    function AddLocMinOp(op: TOutPt; outRec: TOutRec): Boolean;
    procedure GetLocMins;
    procedure PushHorz(a: PActive); {$IFDEF INLINING} inline; {$ENDIF}
    function PopHorz(out a: PActive): Boolean;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure DisposeActive2(a: PActive);
    function DoFix(a1, a2: PActive): Boolean;
    procedure CleanUpPath;
  protected
    function ExecuteInternal(clipType: TClipType;
      fillRule: TFillRule): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths: TPaths): Boolean; override;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths, openPaths: TPaths): Boolean; override;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      var polytree: TPolyTree; out openPaths: TPaths): Boolean; override;
  end;

  //TClipperExD = class(TClipperEx)
  //There's currently no floating point wrapper for TClipperEx because I'll
  //probably be merging TClipperEx back into TClipper once TClipperEx is fully
  //functional. In the meantime you'll need to do integer conversions manually.

  EClipperExLibException = class(Exception);

  function Union(const paths: TPaths; fr: TFillRule): TPaths; overload;
  function Union(const paths: TPathsD; fr: TFillRule): TPathsD; overload;

implementation

resourcestring
  rsClipperEx_error = 'ClipperEx error';

const
  VERTICAL   = 0;
  DIST_SQ_TOL = 2.0;

type
  TOrientation = (oNone, oCW, oCCW);

{$WARN SYMBOL_PLATFORM OFF}

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function IsOpen(outrec: TOutRec): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOpen;
end;
//------------------------------------------------------------------------------

function IsOuter(outrec: TOutRec): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOuter;
end;
//------------------------------------------------------------------------------

function IsHorizontal(a: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (a.dx = NegInfinity) or (a.dx = Infinity);
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

function SegmentsIntersect(const seg1a, seg1b, seg2a, seg2b: TPoint64): boolean;
var
  dx1, dy1, dx2, dy2: double;
begin
  //4 cross-products !
  dx1 := seg1a.X - seg1b.X; dy1 := seg1a.Y - seg1b.Y;
  dx2 := seg2a.X - seg2b.X; dy2 := seg2a.Y - seg2b.Y;
  result :=
    ((dy1 * (seg2a.X - seg1a.X) - dx1 * (seg2a.Y - seg1a.Y)) *
     (dy1 * (seg2b.X - seg1a.X) - dx1 * (seg2b.Y - seg1a.Y)) < 0) and
    ((dy2 * (seg1a.X - seg2a.X) - dx2 * (seg1a.Y - seg2a.Y)) *
     (dy2 * (seg1b.X - seg2a.X) - dx2 * (seg1b.Y - seg2a.Y)) < 0);
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

function GetNextOp(a: PActive): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := GetNextOp(a.op, a.WindDx > 0);
end;
//------------------------------------------------------------------------------

function GetNextNextOp(a: PActive): TOutPt; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if a.Dx > 0 then result := a.op.Next.Next
  else result := a.op.Prev.Prev;
end;
//------------------------------------------------------------------------------

function IsSoftMinima(a: PActive; Y: Int64): Boolean;
var
  op: TOutPt;
begin
  //accommodates horizontal minima ...
  result := (a.bot.Y = Y);
  if not result then Exit;
  if a.WindDx > 0 then
  begin
    op := a.op.Prev;
    while op.Pt.Y = Y do op := op.Prev;
  end else
  begin
    op := a.op.Next;
    while op.Pt.Y = Y do op := op.Next;
  end;
  result := op.Pt.Y < Y;
end;
//------------------------------------------------------------------------------

function IsSoftMaxima(op: TOutPt; goingFwd: Boolean): Boolean; overload;
var
  X: Int64;
begin
  X := op.Pt.X;
  //accommodates horizontal maxima ...
  if goingFwd then
  begin
    op := op.Next.Next;
    while (op.Pt.X = X) do op := op.Next;
  end else
  begin
    op := op.Prev.Prev;
    while (op.Pt.X = X) do op := op.Prev;
  end;
  result := (op.Pt.X < X);
end;
//------------------------------------------------------------------------------

function IsSoftMaxima(a: PActive): Boolean; overload;
var
  op: TOutPt;
  X: Int64;
begin
  X := a.top.X;
  //accommodates horizontal maxima ...
  if a.WindDx > 0 then
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

function GetDx(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dy: Int64;
begin
  dy := (pt2.Y - pt1.Y);
  if dy <> 0 then result := (pt2.X - pt1.X)/dy
  else if (pt2.X > pt1.X) then Result := NegInfinity
  else Result := Infinity;
end;
//------------------------------------------------------------------------------

function TopX(a: PActive; const Y: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (Y <= a.Top.Y) or IsHorizontal(a) then Result := a.Top.X
  else if (Y >= a.bot.Y) then Result := a.bot.X
  else Result := a.bot.X + Round(a.Dx * (Y - a.bot.Y));
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint64; const Y: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (Y = pt1.Y) then
    Result := pt1.X
  else if (Y = pt2.Y) or (pt1.X = pt2.X) or (pt1.Y = pt2.Y) then
    Result := pt2.X
  else
    Result := pt1.X + Round((pt2.X - pt1.X)/(pt2.Y - pt1.Y) * (Y - pt1.Y));
end;
//------------------------------------------------------------------------------

procedure SetDx(a: PActive); overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  a.dx := GetDx(a.bot, a.top);
end;
//------------------------------------------------------------------------------

procedure SetDx(a: PActive; Y: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  a.dx := GetDx(a.bot, a.top);
  a.currX := TopX(a, Y);
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
begin
  //sorts list into descending Y order
  //note1: a negative result preserves the item order
  //note2: using (item2.Y - item1.Y) could potentially cause integer overflows
  if PLocalMinEx(item2).Y = PLocalMinEx(item1).Y then result := 0
  else if PLocalMinEx(item2).Y > PLocalMinEx(item1).Y then result := 1
  else result := -1;
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: TOutPt): TOutPt; {$IFDEF INLINING} inline; {$ENDIF}
begin
  //nb: this function is safe to use before any LocMins are created
  result := op.Prev;
  op.Prev.Next := op.Next;
  op.Next.Prev := op.Prev;
  op.Free;
end;
//------------------------------------------------------------------------------

function CheckDisposeOutPt(op: TOutPt): TOutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (op = op.outRec.pts) then
  begin
    op.Prev.OutRec := op.OutRec;
    op.outRec.pts := op.Prev;
  end;
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

function DistanceFromLineSqrd(const pt, line1, line2: TPoint64): double;
var
  A, B, C: double;
begin
  //see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
  A := line1.Y - line2.Y;
  B := line2.X - line1.X;
  if (A = 0) and (B = 0) then
  begin
    A := pt.X - line1.X;
    B := pt.Y - line1.Y;
    result := (A * A + B * B);
  end else
  begin
    C := A * line1.X  + B * line1.Y;
    C := A * pt.X + B * pt.Y - C;
    Result := (C * C) / (A * A + B * B);
  end;
end;
//---------------------------------------------------------------------------

function IsTriangleOrLess(op: TOutPt): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (op.Next.Next.Next = op) or (op.Next.Next = op); //3, 2,or 1 vertex
end;
//------------------------------------------------------------------------------

function IsSmallTriangle(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := IsTriangleOrLess(op) and
    ((op.Next.Next = op) or
    (DistanceFromLineSqrd(op.Prev.Pt, op.Pt, op.Next.Pt) <= 2) or
    (DistanceFromLineSqrd(op.Pt, op.Next.Pt, op.Prev.Pt) <= 2) or
    (DistanceFromLineSqrd(op.Next.Pt, op.Prev.Pt, op.Pt) <= 2));
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPoint64): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  result := (x1 * y2 - y1 * x2);
end;
//---------------------------------------------------------------------------

function TurnsRight(const pt1, pt2, pt3: TPoint64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := CrossProduct(pt1, pt2, pt3) > 0;
end;
//---------------------------------------------------------------------------

function IsClockwise(op: TOutPt): boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := CrossProduct(op.Prev.Pt, op.Pt, op.Next.Pt) > 0;
end;
//----------------------------------------------------------------------

function GetIntersectPoint(a1, a2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  if a1.dx = a2.dx then
  begin
    //Very rarely we'll get parallel edges here. The quick and dirty solution
    //is simply to return an arbitrary vertex (as below) since these problem
    //polygons are typically very small. However, ideally we should search for
    //and fix the real edges that are intersecting.
    //eg: (535,482 535,481 537,482 536,482 534,481)
    result := a1.bot;
    Exit;
  end;

  if a1.dx = VERTICAL then
  begin
    Result.X := a1.bot.X;
    if IsHorizontal(a2) then
      Result.Y := a2.bot.Y
    else
    begin
      with a2^ do b2 := bot.Y - bot.X / dx;
      Result.Y := round(Result.X / a2.dx + b2);
    end;
  end
  else if a2.dx = VERTICAL then
  begin
    Result.X := a2.bot.X;
    if IsHorizontal(a1) then
      Result.Y := a1.bot.Y
    else
    begin
      with a1^ do b1 := bot.Y - bot.X / dx;
      Result.Y := round(Result.X / a1.dx + b1);
    end;
  end
  else if IsHorizontal(a1) then
  begin
    Result.Y := a1.top.Y;
    b2 := a2.bot.X - a2.bot.Y * a2.dx;
    Result.X := round(a2.dx * Result.Y + b2);
  end
  else if IsHorizontal(a2) then
  begin
    Result.Y := a2.top.Y;
    b1 := a1.bot.X - a1.bot.Y * a1.dx;
    Result.X := round(a1.dx * Result.Y + b1);
  end
  else
  begin
    b1 := a1.bot.X - a1.bot.Y * a1.dx;
    b2 := a2.bot.X - a2.bot.Y * a2.dx;
    m := (b2 - b1)/(a1.dx - a2.dx);
    Result.Y := round(m);
    if Abs(a1.dx) < Abs(a2.dx) then
      Result.X := round(a1.dx * m + b1) else
      Result.X := round(a2.dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

function InsertOp(const pt: TPoint64; insertAfter: TOutPt): TOutPt; overload;
begin
  Result := TOutPt.Create;
  Result.Pt := pt;
  Result.Next := insertAfter.Next;
  insertAfter.Next.Prev := Result;
  insertAfter.Next := Result;
  Result.Prev := insertAfter;
end;
//------------------------------------------------------------------------------

procedure InsertOp(const pt: TPoint64; a: PActive; asBottom: Boolean); overload;
var
  newOp, insertAfter: TOutPt;
begin
  if (pt.Y >= a.bot.Y) or (pt.Y <= a.top.Y) then Exit;
  if a.WindDx > 0 then insertAfter := a.op
  else insertAfter := a.op.Prev;
  newOp := TOutPt.Create;
  newOp.Pt := pt;
  newOp.Next := insertAfter.Next;
  insertAfter.Next.Prev := newOp;
  insertAfter.Next := newOp;
  newOp.Prev := insertAfter;
  if asBottom then
  begin
    a.op := newOp;
    a.Bot := pt;
    a.top := GetNextOp(a).Pt;
  end else
    a.top := pt;
  a.dx := GetDx(a.bot, a.top);
end;
//------------------------------------------------------------------------------

function CreateActive(lm: PLocalMinEx; goingForward: integer) : PActive;
begin
  new(result);
  result.op := lm.op;
  result.WindDx := goingForward;
  result.outRec := lm.outRec;
  Result.Bot := lm.op.Pt;
  if goingForward > 0 then
    Result.top := lm.op.Next.Pt else
    Result.top := lm.op.Prev.Pt;
  result.currX := lm.op.Pt.X;
  SetDx(result);
end;
//------------------------------------------------------------------------------

procedure CheckFixDupsColinear(var op: TOutPt);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  while op.Prev <> op.Next do //loop while removing OutPts
  begin
    if PointsEqual(op.Pt, op.Next.Pt) or //duplicate or colinear
    ((op.Pt.X = op.Next.Pt.X) and (op.Pt.X = op.Prev.Pt.X)) or   //vert colinear
    ((op.Pt.Y = op.Next.Pt.Y) and (op.Pt.Y = op.Prev.Pt.Y)) then //horz colinear
    begin
      if op.Next = op then Exit;
      op := CheckDisposeOutPt(op);
    end
    else if PointsEqual(op.Prev.Pt, op.Next.Pt) then            //colinear spike
    begin
      op := CheckDisposeOutPt(op);
    end else
      break;
  end;
end;
//------------------------------------------------------------------------------

function IsValidOrder(a1, a2: PActive): Boolean;
var
  op1, op2, op3, op4, op1b, op2b: TOutPt;
  X: Int64;
begin
  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  op1b := a1.op; op2b := a2.op;
  while true do
  begin
    op1 := GetNextOp(op1b, a1.WindDx > 0);
    op2 := GetNextOp(op2b, a2.WindDx > 0);
    if (op1 = op2) then
    begin
      result := GetDx(op1b.Pt, op1.Pt) <= GetDx(op2b.Pt, op1.Pt);
      break;
    end
    else if op1.Pt.Y >= op2.Pt.Y then
    begin
      X := TopX(op2b.Pt, op2.Pt, op1.Pt.Y) - op1.Pt.X;
      result := X >= 0;
      if X <> 0 then Exit;
      if op2.Pt.X = op1.Pt.X then
      begin
        op2b := op2;
        op2 := GetNextOp(op2, a2.WindDx > 0);
      end;
      op1b := op1;
      op1 := GetNextOp(op1, a1.WindDx > 0);
    end else
    begin
      X := op2.Pt.X - TopX(op1b.Pt, op1.Pt, op2.Pt.Y);
      result := X >= 0;
      if X <> 0 then Exit;
      op2b := op2;
      op2 := GetNextOp(op2, a2.WindDx > 0);
    end;

    //if one or both edges have passed their maxima ...
    if (op1.Pt.Y > op1b.Pt.Y) then
    begin
      if (op2.Pt.Y > op2b.Pt.Y) then
      begin
        //both edges have passed their maxima, so this is a bit tricky
        op3 := op1b;
        op4 := GetPrevOp(op3, a1.WindDx > 0);
        if TurnsRight(op4.Pt, op3.Pt, op1.Pt) <>
          TurnsRight(op4.Pt, op3.Pt, op2.Pt) then
          result := TurnsRight(op1.Pt, op3.Pt, op2.Pt) else
          result := TurnsRight(op2.Pt, op3.Pt, op1.Pt);
      end else
        result := (a1.WindDx > 0) <> IsClockwise(op1b);
      Exit;
    end
    else if (op2.Pt.Y > op2b.Pt.Y) then
    begin
      result := (a2.WindDx > 0) = IsClockwise(op2b);
      Exit;
    end;
  end;
end;
//------------------------------------------------------------------------------

type
  TPipResult = (pipIn, pipOut, pipOn);

function PointInPolygon(const pt: TPoint64; op: TOutPt): TPipResult;
var
  val: Integer;
  op2: TOutPt;
  d, d2, d3: Double; //using doubles to avoid possible integer overflow
  ip, ipNext: TPoint64;
begin
  Result := pipOn;
  val := 0;
  op2 := op;
  repeat
    ip := op2.Pt;
    ipNext := op2.Next.Pt;
    if (ipNext.Y = pt.Y) then
    begin
      if (ipNext.X = pt.X) or ((ip.Y = pt.Y) and
        ((ipNext.X > pt.X) = (ip.X < pt.X))) then Exit;
    end;

    if ((ip.Y < pt.Y) <> (ipNext.Y < pt.Y)) then
    begin
      if (ip.X >= pt.X) then
      begin
        if (ipNext.X > pt.X) then val := 1 - val
        else
        begin
          d2 := (ip.X - pt.X);
          d3 := (ipNext.X - pt.X);
          d := d2 * (ipNext.Y - pt.Y) - d3 * (ip.Y - pt.Y);
          if (d = 0) then Exit;
          if ((d > 0) = (ipNext.Y > ip.Y)) then val := 1 - val;
        end;
      end else
      begin
        if (ipNext.X > pt.X) then
        begin
          d2 := (ip.X - pt.X);
          d3 := (ipNext.X - pt.X);
          d := d2 * (ipNext.Y - pt.Y) - d3 * (ip.Y - pt.Y);
          if (d = 0) then Exit;
          if ((d > 0) = (ipNext.Y > ip.Y)) then val := 1 - val;
        end;
      end;
    end;
    op2 := op2.Next;
  until op2 = op;

  case val of
    -1: result := pipOn;
     1: result := pipIn;
     else result := pipOut;
  end;
end;
//---------------------------------------------------------------------------

function Path1ContainsPath2(const op, op2: TOutPt): Boolean;
var
  pt: TPoint64;
  op3: TOutPt;
begin
  //precondition: Path2 may touch but not intersect Path1.
  Result := false;
  op3 := op2;
  repeat
    pt := op3.Pt;
    //nb: PointInPolygon returns 0 if false, +1 if true, -1 if pt on polygon
    case PointInPolygon(pt, op) of
      pipOut: Exit;
      pipIn: begin Result := true; Exit; end;
      //else continue
    end;
    op3 := op3.Next;
  until op3 = op2;
  Result := true; //ie no vertex in Path2 is outside Path1.
end;

//---------------------------------------------------------------------------
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
    Dispose(PLocalMinEx(FLocMinList[i]));
  FLocMinList.Clear;
  FLocMinIdx := 0;
end;
//------------------------------------------------------------------------------

function TClipperEx.AddLocMinOp(op: TOutPt; outRec: TOutRec): Boolean;
var
  lm: PLocalMinEx;
begin
  new(lm);
  lm.Y := op.Pt.Y;
  lm.op := op;
  lm.outRec := outRec;
  FLocMinList.Add(lm);
  result := true;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.GetLocMins;
var
  op : TOutPt;
  startsGoingUp, goingUp: Boolean;
begin
  op := FOutRec.pts;
  //move FOutRec.pts to the front of verticals ...
  while (op.Next <> FOutRec.pts) and (op.Next.Pt.Y = op.Pt.Y) do op := op.next;
  if (op.Next = FOutRec.pts) then Exit; //a flat path
  FOutRec.pts := op;

  goingUp := op.next.pt.Y < op.Pt.Y;
  startsGoingUp := goingUp;
  op := op.next;
  while (op <> FOutRec.pts) do
  begin
    if (goingUp) then
    begin
      while (op <> FOutRec.pts) and (op.next.pt.Y <= op.pt.Y) do op := op.next;
      if (op = FOutRec.pts) then break;
      goingUp := false;
    end else
    begin
      while (op <> FOutRec.pts) and (op.next.pt.Y >= op.pt.Y) do op := op.next;
      if (op = FOutRec.pts) then break;
      AddLocMinOp(op, FOutRec);
      goingUp := true;
    end;
    op := op.next;
  end;
  if not goingUp and startsGoingUp then
    AddLocMinOp(op, FOutRec);
end;
//------------------------------------------------------------------------------

function TClipperEx.PopLocalMin(Y: Int64;
  out localMin: PLocalMinEx): Boolean;
begin
  Result := FLocMinIdx < FLocMinList.Count;
  if not result then exit;
  localMin := PLocalMinEx(FLocMinList[FLocMinIdx]);
  if (localMin.Y = Y) then
    inc(FLocMinIdx) else
    Result := false;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.Y := Y;
    FScanLine := newSl;
    newSl.Next := nil;
  end else if Y > FScanLine.Y then
  begin
    new(newSl);
    newSl.Y := Y;
    newSl.Next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.Next) and (Y <= sl.Next.Y) do sl := sl.Next;
    if Y = sl.Y then Exit;
    new(newSl);
    newSl.Y := Y;
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
  if IsOpen(FOutRec) or not assigned(FOutRec.pts) then Exit;
  op := FOutRec.pts;

  if IsOuter(FOutRec) then FOutRec.Owner := nil
  else while Assigned(FOutRec.Owner) and not Assigned(FOutRec.Owner.Pts) do
    FOutRec.Owner := FOutRec.Owner.Owner;

  FActives2 := nil;

  //strip duplicates and vertical colinears ...
  repeat
    op.OutRec := FOutRec;
    CheckFixDupsColinear(op);
    op := op.Next;
  until op = FOutRec.pts;
  CheckFixDupsColinear(op);

  if not IsPolygon(op) then
  begin
    DisposePolyPts(FOutRec.pts);
    FOutRec.pts := nil;
    Exit;
  end;

  GetLocMins;
  FHorz := nil;
  FLocMinList.Sort(LocMinListSort);
  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMinEx(FLocMinList[i]).Y);
  result := true;
end;
//------------------------------------------------------------------------------

function TClipperEx.PopScanLine(out Y: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  Y := FScanLine.Y;
  sl := FScanLine;
  FScanLine := FScanLine.Next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipperEx.DoSweep: Boolean;
var
  Y: Int64;
  a: PActive;
begin
  Result := true;
  if not PopScanLine(Y) then Exit;
  while Result and not FRedo do
  begin
    InsertLocalMinIntoAEL(Y);
    while Result and not FRedo and PopHorz(a) do
      Result := DoHorizontal(a, Y);
    if not result or FRedo or not PopScanLine(Y) then break;
    if assigned(FActives2) then
      Result := DoScanbeam(Y);
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.DoFix(a1, a2: PActive): Boolean;
var
  op1,op2,op3,op4: TOutPt;
  d1, d2, d3, d4: double;
  pt: TPoint64;
begin
  FRedo := true;

  {
    disposing minima: is safe but if either side is also a maxima
    then dispose of both edges too (and let adjacent edges do their jobs)
    disposing maxima: is safe too but ...
  }


  result := true;
  if not assigned(a2) then
  begin
    //a spiking minima or maxima
    if (a1.op = a1.NextInAEL.op) then
      CheckDisposeOutPt(a1.op) else             //minima
      CheckDisposeOutPt(GetNextOp(a1));         //maxima
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
      if a1.WindDx > 0 then
        op1 := InsertOp(pt, op1) else
        op1 := InsertOp(pt, op2);
    end
    else if d2 < d1 then
      op1 := op2;
    if min(d3,d4) > DIST_SQ_TOL then
    begin
      if a2.WindDx > 0 then
        op3 := InsertOp(pt, op3) else
        op3 := InsertOp(pt, op4);
    end
    else if d4 < d3 then
      op3 := op4;

    SplitPath(op1, op3);
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.UpdateActive(a: PActive; Y: Int64): Boolean;
var
  op: TOutPt;
begin
  if a.WindDx > 0 then
  begin
    a.op := a.op.Next;  //update a.op   (bot OutPt)
    op := a.op.Next;    //op == new top (top Point64)
  end else
  begin
    a.op := a.op.Prev;  //
    op := a.op.Prev;    //
  end;
  result := (op.Prev <> op.Next) and (op.Pt.Y <= a.op.Pt.Y);
  if not result then exit;  //a definite maxima

  a.Bot := a.Top;
  a.top := op.Pt;
  //Maxima horizontals aren't allowed when going forward. So at horizontals
  //going forward, make sure these are intermediate verticals ...
  if (a.WindDx  > 0) and (a.top.Y = Y) then
  begin
    repeat
      op := op.Next;
    until (op = a.op) or (op.Pt.Y <> Y);
    result := (op <> a.op) and (op.Pt.Y < Y);
    if not result then Exit;
  end;

  SetDx(a, Y);
  a.currX := a.Bot.X;

  if IsHorizontal(a) then PushHorz(a)
  else if a.top.Y < Y then InsertScanLine(a.top.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperEx.SwapPositionsInAEL(a1, a2: PActive);
var
  prev, next: PActive;
begin
  next := a2.NextInAEL;
  if Assigned(next) then next.PrevInAEL := a1;
  prev := a1.PrevInAEL;
  if Assigned(prev) then prev.NextInAEL := a2;
  a2.PrevInAEL := prev;
  a2.NextInAEL := a1;
  a1.PrevInAEL := a2;
  a1.NextInAEL := next;
  if not assigned(a2.PrevInAEL) then FActives2 := a2;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.InsertIntoAel1(a: PActive);
var
  prev, next: PActive;
begin
  if not Assigned(FActives2) then
  begin
    a.NextInAEL := nil;
    a.PrevInAEL := nil;
    FActives2 := a;
    Exit;
  end;

  if IsValidOrder(a, FActives2) then
  begin
    a.PrevInAEL := nil;
    a.NextInAEL := FActives2;
    FActives2.PrevInAEL := a;
    FActives2 := a;
    prev := nil;
  end else
  begin
    prev := FActives2;
    while Assigned(prev.NextInAEL) and IsValidOrder(prev.NextInAEL, a) do
      prev := prev.NextInAEL;
    a.NextInAEL := prev.NextInAEL;
    if Assigned(a.NextInAEL) then a.NextInAEL.PrevInAEL := a;
    a.PrevInAEL := prev;
    prev.NextInAEL := a;
  end;

  //and one final order check ...
  next := a.NextInAEL;
  if Assigned(prev) and (a.currX - prev.currX < 5) and (prev.dx < a.dx) and
    TurnsRight(a.bot, a.top, prev.top) and
    SegmentsIntersect(a.bot, a.top, prev.bot, prev.top) then
      SwapPositionsInAEL(prev, a)
  else if Assigned(next) and (next.currX - a.currX < 5) and
    (next.dx > a.dx) and TurnsRight(next.top, a.top, a.bot) and
      SegmentsIntersect(a.bot, a.top, next.bot, next.top) then
        SwapPositionsInAEL(a, next);
end;
//----------------------------------------------------------------------

procedure TClipperEx.InsertIntoAel2(a, a2: PActive);
begin
  a2.NextInAEL := a.NextInAEL;
  if Assigned(a2.NextInAEL) then a2.NextInAEL.PrevInAEL := a2;
  a2.PrevInAEL := a;
  a.NextInAEL := a2;
end;
//----------------------------------------------------------------------

function AltState(outrec: TOutRec): TOutRecState;
begin
  if IsOuter(outrec)then result := osInner else Result := osOuter;
end;
//----------------------------------------------------------------------

procedure TClipperEx.SplitPath(op1, op2: TOutPt);
var
  op, op1n, op2n: TOutPt;
  outrec: TOutRec;
begin
  if op1 = op2 then RaiseError;

  op1n := op1.Next;
  op2n := op2.Next;
  op1.Next := op2n; op2n.Prev := op1;
  op2.Next := op1n; op1n.Prev := op2;

  if IsSmallTriangle(op2) then
  begin
    DisposePolyPts(op2);
    FOutRec.Pts := op1;
  end
  else if IsSmallTriangle(op1) then
  begin
    DisposePolyPts(op1);
    FOutRec.Pts := op2;
  end else
  begin
    //find out which path contains FOutRec.Pts and
    //add the other newly separated path to OutRecList ...
    op := op2;
    repeat
      if op = FOutRec.Pts then
      begin
        op2 := op1; //swap op1 and op2
        op1 := op;
        break;
      end;
      op := op.Next;
    until op = op2;

    //add op2 to OutRecList
    outrec := TOutRec.Create;
    outRec.Idx := OutRecList.Add(outRec);
    outRec.Pts := op2;
    outRec.PolyPath := nil;

    if Path1ContainsPath2(op1, op2) then
    begin
      outRec.State := AltState(FOutRec);
      outRec.Owner := FOutRec;
    end
    else if Path1ContainsPath2(op2, op1) then
    begin
      outRec.State := FOutRec.State;
      FOutRec.State := AltState(FOutRec);
      outRec.Owner := FOutRec.Owner;
      FOutRec.Owner := outRec;
    end else
    begin
      outRec.State := FOutRec.State;
      outRec.Owner := FOutRec.Owner;
    end;
  end;
end;
//----------------------------------------------------------------------

procedure TClipperEx.InsertLocalMinIntoAEL(const Y: Int64);
var
  lm: PLocalMinEx;
  left, right, left2, right2: PActive;
  op: TOutPt;
  winding: double;
begin
  while PopLocalMin(Y, lm) do
  begin
    winding := CrossProduct(lm.op.Prev.Pt, lm.op.Pt, lm.op.Next.Pt);
    if winding = 0 then
    begin
      CheckDisposeOutPt(lm.op);
      FRedo := true;
      Exit;
    end;

    if winding > 0 then
    begin
      left := CreateActive(lm, 1);
      right := CreateActive(lm, -1);
    end else
    begin
      left := CreateActive(lm, -1);
      right := CreateActive(lm, 1);
    end;

    //nb: 'left' will be horizontal when it's an inner CCW path
    InsertIntoAel1(left);

    left2 := left.PrevInAEL;
    if assigned(left2) and
      (left.currX = left2.currX) then
    begin
      op := GetNextOp(left);
      if GetNextOp(left2) = op then
      begin
        CheckDisposeOutPt(op); //minima-maxima spike
        FRedo := true;
      end
      else if (left2.bot.Y > Y) then  //avoids micro self-intersections
        InsertOp(left.bot, left2, true);
    end;

    InsertIntoAel2(left, right);
    right2 := right.NextInAEL;
    if assigned(right2) and (right.currX = right2.currX) then
    begin
      op := GetNextOp(right);
      if GetNextOp(right2) = op then
      begin
        CheckDisposeOutPt(op); //minima-maxima spike
        FRedo := true;
      end
      else if (right2.top.Y < Y) and (right2.bot.Y > Y) then
        InsertOp(right.bot, right2, true);
    end;

    if FRedo then Exit;

    if IsHorizontal(left) then PushHorz(left)
    else InsertScanLine(left.top.Y);
    if IsHorizontal(right) then PushHorz(right)
    else InsertScanLine(right.top.Y);
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.DoHorizontal(horz: PActive; Y : Int64): Boolean;
var
  prev, next: PActive;
begin
  Result := true;
  if (horz.top.X < horz.currX) then
  begin
    ///////////////////////////////////////////////////////////////
    // heading left ...
    ///////////////////////////////////////////////////////////////
    while Result and assigned(horz.PrevInAEL) do
    begin
      prev := horz.PrevInAEL;
      if (prev.currX <= horz.top.X) then break;

      if IsHorizontal(prev) or IsSoftMaxima(horz) or IsSoftMinima(prev, Y) then
      begin
        //overlapping verticals (todo - join edges later)
        //or vertical edge passing over or under a minima or maxima
        SwapPositionsInAEL(prev, horz);
      end else
      begin
        Result := DoFix(prev, horz);
        Exit;
      end;
    end;
  end else
  begin
    ///////////////////////////////////////////////////////////////
    // heading right ...
    ///////////////////////////////////////////////////////////////
    while result and assigned(horz.NextInAEL) do
    begin
      next := horz.NextInAEL;
      if (next.currX >= horz.top.X) then break;

      if IsHorizontal(next) or IsSoftMaxima(horz) or IsSoftMinima(next, Y) then
      begin
        SwapPositionsInAEL(horz, next);
      end else
      begin
        Result := DoFix(horz, next);
        Exit;
      end;
    end;
    ///////////////////////////////////////////////////////////////
  end;

  if not UpdateActive(horz, Y) then DisposeActive2(horz);
end;
//------------------------------------------------------------------------------

function TClipperEx.DoScanbeam(Y : Int64): Boolean;
var
  a, prev, next: PActive;
begin
  Result := true;
  a := FActives2;
  a.currX := TopX(a, Y);
  while assigned(a) do
  begin
    next := a.NextInAEL;
    if assigned(next) then
    begin
      next.currX := TopX(next, Y);

      if (a.currX = next.currX) then
      begin
        if (a.op = next.op) and ((a.top.Y = Y) or (next.top.Y = Y)) then
        begin
          Result := DoFix(a, nil);     //minima spike #23 #138 #145 #151
          Exit;
        end
        else if ((GetNextOp(a) = GetNextOp(next)) and (a.dx >= next.dx)) then
        begin
          //remove maxima spike ...
          CheckDisposeOutPt(GetNextOp(a));
          //there can still be more to do here (ie residual spike) #179
          FRedo := true;
          Exit;
        end;
      end
      else if (a.currX > next.currX) and
        ((a.top.Y >= Y) or (next.top.Y >= Y)) then //rounding issue ...
      begin
        Result := DoFix(a, next);
        Exit;
      end;
    end;

    if not result then Exit;
    if (a.top.Y >= Y) then
    begin
      prev := a.PrevInAEL;
      if assigned(prev) and (prev.CurrX = a.CurrX) then
        InsertOp(a.Top, prev, true)
      else if assigned(next) and (next.CurrX = a.CurrX) then
        InsertOp(a.Top, next, false);

      if IsHorizontal(a) and (a.top.Y = Y) then PushHorz(a)
      else if not UpdateActive(a, Y) then DisposeActive2(a);
    end;
    a := next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.PushHorz(a: PActive);
begin
  if assigned(FHorz) then
    a.NextInSel := FHorz else
    a.NextInSel := nil;
  FHorz := a;
end;
//------------------------------------------------------------------------------

function TClipperEx.PopHorz(out a: PActive): Boolean;
begin
  Result := assigned(FHorz);
  if not Result then Exit;
  a := FHorz;
  FHorz := FHorz.NextInSel;
end;
//------------------------------------------------------------------------------

procedure TClipperEx.DisposeActive2(a: PActive);
begin
  if (a = FActives2) and (a.NextInAEL = nil) then
  begin
    Dispose(a);
    FActives2 := nil;
    Exit;
  end;
  if (a = FActives2) then FActives2 := a.NextInAEL;
  if assigned(a.PrevInAEL) then a.PrevInAEL.NextInAEL := a.NextInAEL;
  if assigned(a.NextInAEL) then a.NextInAEL.PrevInAEL := a.PrevInAEL;
  Dispose(a);
end;
//------------------------------------------------------------------------------

function TClipperEx.ExecuteInternal(clipType: TClipType;
  fillRule: TFillRule): Boolean;
var
  i: integer;
begin
  try
    Result := inherited ExecuteInternal(clipType, fillRule);
    i := 0;
    while Result and (i < OutRecList.Count) do
    begin
      FRedo := false;
      if PrepareSweep(i) then
      begin
        result := DoSweep;
        if not FRedo then inc(i);
      end
      else inc(i);
      CleanUpPath;
    end;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedPaths: TPaths): Boolean;
var
  dummy: TPaths;
begin
  closedPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if not Result then Exit;
    BuildResult(closedPaths, dummy);
  finally
    CleanUpPath;
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths, openPaths: TPaths): Boolean;
begin
  closedPaths := nil;
  openPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if not Result then Exit;
    BuildResult(closedPaths, openPaths);
  finally
    CleanUpPath;
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipperEx.Execute(clipType: TClipType; fillRule: TFillRule;
  var polytree: TPolyTree; out openPaths: TPaths): Boolean;
begin
  openPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if result then BuildResultTree(polytree, openPaths);
  finally
    CleanUpPath;
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function Union(const paths: TPaths; fr: TFillRule): TPaths;
begin
  with TClipperEx.Create do
  try
    AddPaths(paths, ptSubject);
    Execute(ctUnion, fr, result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Union(const paths: TPathsD; fr: TFillRule): TPathsD;
var
  pp, sol: TPaths;
begin
  pp := ScalePaths(paths, 1000, 1000);
  with TClipperEx.Create do
  try
    AddPaths(pp, ptSubject);
    Execute(ctUnion, fr, sol);
  finally
    Free;
  end;
  Result := ScalePathsD(sol, 0.001, 0.001);
end;
//------------------------------------------------------------------------------

end.
