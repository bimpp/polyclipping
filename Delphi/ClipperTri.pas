unit ClipperTri;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  12 January 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module to triangulate paths (note pre-condition below)          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

(*
Code outline:

See also https://www.cise.ufl.edu/class/cot5520sp18/CG_PolygonTriangulation.pdf

Precondition: All polygon intersections must be removed before starting.

A 'sweep' operation is performed over the polygon set to "keyhole" inner paths
and to "Y-monotone". Keyholing joins inner paths with their outer owners so
they become one path (though with 'touching' edges where joins are performed).
Y-monotone polygons are such that by following their edges up from the very
bottom, these edges never turn back toward the bottom before reaching the very
top. So Y-monotoning is the process of splitting polygons so their left and
right bounds never 'turn back'.

During this bottom up sweep, it's essential to avoid accidentally creating
self-intersections while splitting or joining polygons. And splitting or
joining vertices with their closest neighbours isn't always safe. The simplest
way to avoid self-intersections is to perform splits and joins with the vertex
encountered immediately prior to the current one (within that particular
polygon region). Hence during the sweep, each polygon region needs to keep
track of its most recently encountered vertex, its 'helper'. Each right bound
contains a link to the 'helper' vertex for that region. This is updated every
time a vertex is passed, whether on the left or right of that region.

Keyholing - when the lowest vertex of an inner path is inserted during the
sweep, this vertex is keyholed with the 'helper' vertex in the outer polygon.

Y-Monotone - removes all 'internal' local minima and maxima by splitting
these with 'helper' vertices. (Vertices at 'internal' minima and maxima point
toward the inside of their polygons.) An internal minima will split the polygon
by joining with the helper vertex immediately below. An internal maxima (which
will always be a helper) splits the polygon by joining with the next vertex
adjacent or above that's within the same polygon region.

Finally, triangulating Y-monotoned polygons -

The triangulating logic is best explained with pseudo-code:
Vertices are arranged in a closed loop and oriented in a clockwise direction.

vLow = GetLowerMostVertex
vLeft = vLow.next; vRight = vLow.prior;
while vLeft <> vRight
{
  v1 = GetLowerOf(vLeft, vRight);
  lowerIsLeft = (v1 = vLeft);
  if lowerIsLeft then vOpp = vRight else vOpp = vLeft
  //get v2, the vertex below v1 ...
  if lowerIsLeft then v2 = v1.prior
  else v2 = v1.next
  while v2 <> vOpp
  {
    //get v3, the vertex below v2 ...
    if lowerIsLeft then v3 = v2.prior
    else v3 = v2.next

    cp = CrossProduct(v1, v2, v3) //angling clockwise when cp < 0
    if cp <> 0 //ie ignore empty triangles
      if lowerIsLeft = (cp < 0) then break (ie exit inner while)
      else AddTriangle(v1, v2, v3)
    delete v2
    v2 = v3
   }
  //get next vertex above ...
  if lowerIsLeft vLeft = vLeft.next
  else vRight = vRight.prior
}

*)

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
  Windows, SysUtils, Classes, Math, ClipperCore, Clipper, ClipperEx;

function Triangulate(paths: TPaths): TPaths; overload;
function Triangulate(paths: TPathsD): TPathsD; overload;

implementation

type

  TOutRec = class;

  TOutPt = class
    Pt       : TPoint64;
    Next     : TOutPt;
    Prev     : TOutPt;
    OutRec   : TOutRec;
  end;

  TOutRecState = (osUndefined, osOpen, osOuter,
    osOuterCheck, osInner, osInnerCheck);

  TOutRec = class
    Idx      : Integer;
    Owner    : TOutRec;
    Pts      : TOutPt;
    State    : TOutRecState;
  end;

  PScanLine = ^TScanLine;
  TScanLine = record
    Y          : Int64;
    Next       : PScanLine;
  end;

  PLocalMin = ^TLocalMinEx;

  PActive2 = ^TActive2;
  TActive2 = record
    bot        : TPoint64;
    top        : TPoint64;
    opTop      : TOutPt;
    hlpIsMax   : Boolean;
    hlpOutPt   : TOutPt;
    currX      : Int64;
    dx         : double;
    goingFwd   : Boolean;
    next       : PActive2;
    prev       : PActive2;
    nextHorz   : PActive2;
  end;

  TLocalMinEx = record
    Y          : Int64;
    op         : TOutPt;
  end;

  TClipperTri = class
  private
    FLocMinList  : TList;
    FOrecList    : TList;
    FLocMinIdx   : integer;
    FScanLine    : PScanLine;
    FActives2    : PActive2;
    FHorz        : PActive2;
    FTriangles   : TPaths;
    function UpdateActive(a: PActive2; Y: Int64): Boolean;
    procedure InsertScanLine(const Y: Int64);
    function PopScanLine(out Y: Int64): Boolean;
    function PeekLocalMin(Y: Int64): Boolean;
    procedure PopLocalMin(Y: Int64; out localMin: PLocalMin);
      {$IFDEF INLINING} inline; {$ENDIF}
    function InsertIntoAel1(a: PActive2): Boolean;
    procedure InsertIntoAel2(a, a2: PActive2);
    procedure SwapPositionsInAEL(e1, e2: PActive2);
    procedure InsertLocMinIntoScanline(const Y: Int64);
    procedure PushHorz(horz: PActive2);
    function PopHorz: PActive2;
    procedure DoHorizontal(horz: PActive2);
    function SweepYMonotone: Boolean;
    function SweepTriangulate: Boolean;
    function DoYMonotoneScanline(Y : Int64): Boolean;
    procedure DoInternalMaxima(a: PActive2);
    function AdjustHorzHelper(var opHlp: TOutPt; const top: TPoint64): TOutPt;
    procedure Triangulate(op, altOp: TOutPt; isGoingFwd: Boolean);
    procedure AddTriangle(const pt1, pt2, pt3: TPoint64);
    function AddLocMinOp(op: TOutPt; ort: TOutRec): Boolean;
    procedure GetLocMins(const path: TPath);
    procedure DisposeActive2(a: PActive2);
    procedure CleanUp;
  public
    constructor Create(scalingFraction: double = 0);
    destructor Destroy; override;
    function Execute(const srcPaths: TPaths;
      out triangles: TPaths): Boolean; overload;
  end;

  EClipperExLibException = class(Exception);

resourcestring
  rsClipperTri_error = 'ClipperTri error';

{$WARN SYMBOL_PLATFORM OFF}

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

procedure RaiseError;
begin
  raise EClipperExLibException.Create(rsClipperTri_error);
end;
//------------------------------------------------------------------------------

procedure UpdateOutPts(startOp, endOp: TOutPt; ort: TOutRec);
begin
  repeat
    startOp.outRec := ort;
    startOp := startOp.Next;
  until startOp = endOp;
end;
//------------------------------------------------------------------------------

function IsInner(outRec: TOutRec): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (outRec.State = osInner);
end;
//------------------------------------------------------------------------------

function IsHorizontal(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.top.Y = a.bot.Y;
end;
//------------------------------------------------------------------------------

function IsLeftBound(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.goingFwd;
end;
//------------------------------------------------------------------------------

function IsRightBound(a: PActive2): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := not a.goingFwd;
end;
//------------------------------------------------------------------------------

function GetPrevOp(op: TOutPt; goingForward: Boolean): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if goingForward then result := op.Prev else result := op.Next;
end;
//------------------------------------------------------------------------------

function GetPrevOp(a: PActive2): TOutPt; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if a.goingFwd then result := a.opTop.Prev else result := a.opTop.Next;
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
  if a.goingFwd then result := a.opTop.Next else result := a.opTop.Prev;
end;
//------------------------------------------------------------------------------

function GetRight(a: PActive2): PActive2;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.next;
  while assigned(result) and ((result.opTop.outRec <> a.opTop.outRec) or
    not IsRightBound(result)) do result := result.next;
end;
//------------------------------------------------------------------------------

function GetOuterRight(a: PActive2): PActive2;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := a.next;
  while assigned(result) and not IsRightBound(result) do
    result := result.next;
end;
//------------------------------------------------------------------------------

procedure SetHelper(a: PActive2; op: TOutPt; isInternalMax: Boolean = false);
var
  right: PActive2;
begin
  if IsRightBound(a) then right := a
  else right := GetRight(a);
  if not assigned(right)  or (right.opTop.OutRec <> op.OutRec) then
    RaiseError;

  right.hlpIsMax := isInternalMax;
  right.hlpOutPt := op;
end;
//------------------------------------------------------------------------------

function InsertOp(const pt: TPoint64; insertAfter: TOutPt): TOutPt;
begin
  Result := TOutPt.Create;
  Result.Pt := pt;
  Result.outRec := insertAfter.outRec;
  Result.Next := insertAfter.Next;
  insertAfter.Next.Prev := Result;
  insertAfter.Next := Result;
  Result.Prev := insertAfter;
end;
//------------------------------------------------------------------------------

function DuplicateOp(op: TOutPt): TOutPt; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := InsertOp(op.Pt, op);
end;
//------------------------------------------------------------------------------

function DisposeOutPt(op: TOutPt): TOutPt;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := op.Prev;
  op.Prev.Next := op.Next;
  op.Next.Prev := op.Prev;
  op.Free;
end;
//------------------------------------------------------------------------------

function DisposePolyPts(op: TOutPt): boolean; //todo make this a procedure
  {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: TOutPt;
begin
  try
    op.Prev.Next := nil;
    while Assigned(op) do
    begin
      tmpPp := op;
      op := op.Next;
      tmpPp.Free;
    end;
    result := true;
  except
    result := false;
  end;
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

function TurnsLeft(const pt1, pt2, pt3: TPoint64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := CrossProduct(pt1, pt2, pt3) < 0;
end;
//---------------------------------------------------------------------------

function TurnsRight(const pt1, pt2, pt3: TPoint64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := CrossProduct(pt1, pt2, pt3) > 0;
end;
//---------------------------------------------------------------------------

function PointsEqual(const p1, p2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function IsClockwise(op: TOutPt): boolean; {$IFDEF INLINING} inline; {$ENDIF}
var
  prev, next: TOutPt;
begin
  prev := op.Prev; next := op.Next;
  while PointsEqual(op.Pt, prev.Pt) do prev := prev.Prev;
  while PointsEqual(op.Pt, next.Pt) do next := next.Next;
  Result := TurnsRight(prev.Pt, op.Pt, next.Pt);
end;
//----------------------------------------------------------------------

function IsCounterClockwise(op: TOutPt): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  prev, next: TOutPt;
begin
  prev := op.Prev; next := op.Next;
  while PointsEqual(op.Pt, prev.Pt) do prev := prev.Prev;
  while PointsEqual(op.Pt, next.Pt) do next := next.Next;
  Result := TurnsLeft(prev.Pt, op.Pt, next.Pt);
end;
//----------------------------------------------------------------------

function GetDx(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dy: Int64;
begin
  dy := (pt2.Y - pt1.Y);
  if dy <> 0 then result := (pt2.X - pt1.X) / dy
  else if (pt2.X > pt1.X) then result := NegInfinity
  else result := Infinity;
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint64; const Y: Int64): Int64; overload;
var
  dx: double;
begin
  if (Y = pt1.Y) then
    Result := pt1.X
  else if (Y = pt2.Y) or (pt1.Y = pt2.Y) or (pt1.X = pt2.X) then
    Result := pt2.X
  else
  begin
    dx := GetDx(pt1, pt2);
    Result := pt1.X + Round(dx * (Y - pt1.Y));
  end;
end;
//------------------------------------------------------------------------------

function TopX(e: PActive2; const currentY: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (currentY = e.Top.Y) or (e.Top.X = e.Bot.X) then Result := e.Top.X
  else Result := e.Bot.X + Round(e.Dx * (currentY - e.Bot.Y));
end;
//------------------------------------------------------------------------------

function IsValidOrder(a1, a2: PActive2): Boolean;
var
  pt1, pt2: TPoint64;
  op1, op2, op3, op4: TOutPt;
  X: Int64;
begin
  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  pt1 := a1.Bot; pt2 := a2.Bot;
  op1 := a1.opTop; op2 := a2.opTop;
  while true do
  begin
    if op1.Pt.Y >= op2.Pt.Y then
    begin
      X := TopX(pt2, op2.Pt, op1.Pt.Y) - op1.Pt.X;
      result := X > 0;
      if X <> 0 then Exit;
      if op2.Pt.Y = op1.Pt.Y then
      begin
        pt2 := op2.Pt;
        op2 := GetNextOp(op2, a2.goingFwd);
      end;
      pt1 := op1.Pt;
      op1 := GetNextOp(op1, a1.goingFwd);
    end else
    begin
      X := op2.Pt.X - TopX(pt1, op1.Pt, op2.Pt.Y);
      result := X > 0;
      if X <> 0 then Exit;
      pt2 := op2.Pt;
      op2 := GetNextOp(op2, a2.goingFwd);
    end;
    //so far edges are inseparable (ie at lower of their two maxima)

    if (op1.Pt.Y > pt1.Y) then
    begin
      //OK at least a1 has passed it's maxima (and pt1 == pt2 here)
      if (op2.Pt.Y > pt2.Y) then
      begin
        //both edges have passed their maxima, so this is a bit tricky
        op3 := GetPrevOp(op1, a1.goingFwd);
        op4 := GetPrevOp(op3, a1.goingFwd);
        if TurnsRight(op4.Pt, op3.Pt, op1.Pt) <>
          TurnsRight(op4.Pt, op3.Pt, op2.Pt) then
          result := TurnsRight(op2.Pt, op3.Pt, op1.Pt) else
          result := TurnsRight(op1.Pt, op3.Pt, op2.Pt);
      end
      else
        result := a1.goingFwd <> IsClockwise(GetPrevOp(op1, a1.goingFwd));
    end
    else if (op2.Pt.Y > pt2.Y) then
      result := a2.goingFwd = IsClockwise(GetPrevOp(op2, a2.goingFwd))
    else
      Continue;

    Exit;
  end;
end;
//------------------------------------------------------------------------------

function IsValidNext(left, right, next: PActive2): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  Y, x1, x2, x3: Int64;
  pt1, pt2: TPoint64;
begin
  if IsHorizontal(left) then
    pt1 := GetNextOp(left.opTop, left.goingFwd).Pt else
    pt1 := left.top;
  if IsHorizontal(right) then
    pt2 := GetNextOp(right.opTop, right.goingFwd).Pt else
    pt2 := right.top;
  Y := Max(Max(pt1.Y, pt2.Y), next.top.Y);
  x1 := TopX(left.bot, pt1, Y);
  x2 := TopX(left.bot, pt2, Y);
  x3 := TopX(next, Y);

  if (x3 >= x2) and (x3 >= x1) then result := true
  else if (x3 <= x2) and (x3 <= x1) then result := false
  else result := x3 > (x1 + x2) div 2;
end;
//------------------------------------------------------------------------------

function IsValidPrev(left, right, prev: PActive2): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  Y, x1, x2, x3: Int64;
  pt1, pt2: TPoint64;
begin
  if IsHorizontal(left) then
    pt1 := GetNextOp(left.opTop, left.goingFwd).Pt else
    pt1 := left.top;
  if IsHorizontal(right) then
    pt2 := GetNextOp(right.opTop, right.goingFwd).Pt else
    pt2 := right.top;
  Y := Max(Max(pt1.Y, pt2.Y), prev.top.Y);
  x1 := TopX(left.bot, pt1, Y);
  x2 := TopX(left.bot, pt2, Y);
  x3 := TopX(prev, Y);

  if (x3 <= x2) and (x3 <= x1) then result := true
  else if (x3 >= x2) and (x3 >= x1) then result := false
  else result := x3 < (x1 + x2) div 2;
end;
//------------------------------------------------------------------------------

function IsMaxima(a: PActive2): Boolean;
var
  op, op2: TOutPt;
begin
  op := a.opTop;
  op2 := GetNextOp(a);
  result := (op2.Pt.Y > op.Pt.Y);
  if (op2.Pt.Y <> op.Pt.Y) or not a.goingFwd then Exit;
  //Maxima horizontals aren't allowed when going forward. So at horizontals
  //going forward, make sure that these are only intermediate horizontals ...
  repeat
    op2 := op2.Next;
  until (op2 = op) or (op2.Pt.Y <> op.Pt.Y);
  result := (op2.Pt.Y > op.Pt.Y); //false == an intermediate horz.
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  pt1, pt2: TPoint64;
  dx,dy: Int64;
begin
  //compare Y (descending), then X (ascending) then finally angles at minima
  //and returning negative values indicates correct order (item1 before item2)
  if item1 = item2 then begin result := 0; Exit; end;
  dy := PLocalMin(item2).Y - PLocalMin(item1).Y;
  if dy = 0 then
  begin
    dx := PLocalMin(item1).op.Pt.X - PLocalMin(item2).op.Pt.X;
    if dx = 0 then
    begin
      //minima share the same point so the wider angle preceeds the narrower
      with PLocalMin(item1)^ do
        if IsClockwise(op) then pt1 := op.Next.Pt else pt1 := op.Prev.Pt;
      with PLocalMin(item2)^ do
        if IsClockwise(op) then pt2 := op.Next.Pt else pt2 := op.Prev.Pt;
      if TurnsLeft(pt1, PLocalMin(item1).op.Pt, pt2) then Result := -1  //#106, #210
      else Result := 1;
    end
    else if dx > 0 then Result := 1
    else Result := -1;
  end
  else if dy > 0 then Result := 1
  else Result := -1;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: TOutPt);
var
  op2,op3: TOutPt;
begin
  if not Assigned(op) or (op.Next = op.Prev) then Exit;
  op2 := op;
  repeat
    op3:= op2.Next;
    op2.Next := op2.Prev;
    op2.Prev := op3;
    op2 := op3;
  until op2 = op;
end;
//------------------------------------------------------------------------------

function CreateActive(lm: PLocalMin; goingForward: Boolean) : PActive2;
begin
  new(result);
  result.opTop := GetNextOp(lm.op, goingForward);
  result.goingFwd := goingForward;
  Result.bot := lm.op.Pt;
  Result.top := Result.opTop.Pt;
  Result.dx := GetDx(Result.bot, Result.top);
  Result.currX := Result.bot.X;
  Result.hlpOutPt := lm.op;
  Result.hlpIsMax := false;
end;

//------------------------------------------------------------------------------
// TClipperTri methods ...
//------------------------------------------------------------------------------

constructor TClipperTri.Create(scalingFraction: double);
begin
  FLocMinList  := TList.Create;
  FOrecList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipperTri.Destroy;
begin
  FLocMinList.Free;
  FOrecList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.CleanUp;
var
  i: integer;
  dummy: Int64;
  ort: TOutRec;
begin
  //in case of abnormal termination ...
  while assigned(FScanLine) do PopScanLine(dummy);
  while assigned(FActives2) do DisposeActive2(FActives2);
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMin(FLocMinList[i]));
  FLocMinList.Clear;
  FLocMinIdx := 0;

  for i := 0 to FOrecList.Count -1 do
  begin
    ort := FOrecList[i];
    if assigned(ort.Pts) then
      if not DisposePolyPts(ort.Pts) then break;
    ort.Free;
  end;
  FOrecList.Clear;
end;
//------------------------------------------------------------------------------

function TClipperTri.AddLocMinOp(op: TOutPt; ort: TOutRec): Boolean;
var
  lm: PLocalMin;
begin
  //With horizontal minima make sure we add the left most vertex.
  //This is important when there's another minima that 'touches' this horizontal
  //so that sorting will ensure the outer (horizontal) minima is added first.
  //See also LocMinListSort().
  while (op.Prev.Pt.Y = op.Pt.Y) and (op.Prev.Pt.X <= op.Pt.X) do op := op.Prev;
  new(lm);
  lm.Y := op.Pt.Y;
  lm.op := op;
  FLocMinList.Add(lm);
  result := true;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.GetLocMins(const path: TPath);
var
  i, highI: integer;
  op, op2, op3: TOutPt;
  goingUp, goingUpAtStart: Boolean;
  ort: TOutRec;
begin
  highI := high(path);
  if highI < 0 then Exit;
  op := TOutPt.Create;
  op.Pt := path[0];
  ort := TOutRec.Create;
  ort.Idx := FOrecList.Add(ort);
  ort.State := osUndefined;
  ort.Pts := op;
  op.OutRec := ort;
  op2 := op;
  for i := 1 to highI do
  begin
    op3 := TOutPt.Create;
    op3.Pt := path[i];
    op3.OutRec := ort;
    op3.Prev := op2;
    op2.Next := op3;
    op2 := op3;
  end;
  op.Prev := op2;
  op2.Next := op;

  //skip to the front of horizontals ...
  while (op.Next <> ort.pts) and (op.Next.Pt.Y = op.Pt.Y) do op := op.next;
  if (op.Next = ort.pts) then Exit; //a flat path

  goingUp := op.next.pt.Y < op.Pt.Y;
  goingUpAtStart := goingUp;

  //with horizontal minima, make sure we start at the left most vertex ...
  while (op.Prev.Pt.Y = op.Pt.Y) and (op.Prev.Pt.X <= op.Pt.X) do op := op.Prev;
  ort.pts := op;
  op := op.next;
  while (op <> ort.pts) do
  begin
    if (goingUp) then
    begin
      while (op <> ort.pts) and (op.next.pt.Y <= op.pt.Y) do
      begin
        if (op.next.pt.Y = op.pt.Y) and
          ((op.next.pt.X = op.pt.X) or (op.next.next.pt.Y = op.pt.Y)) then
            DisposeOutPt(op.Next) else
            op := op.next;
      end;
      if (op = ort.pts) then break;
      goingUp := false;
    end else
    begin
      while (op <> ort.pts) and (op.next.pt.Y >= op.pt.Y) do
      begin
        if (op.next.pt.Y = op.pt.Y) and
          ((op.next.pt.X = op.pt.X) or (op.next.next.pt.Y = op.pt.Y)) then
            DisposeOutPt(op.Next) else
            op := op.next;
      end;
      if (op = ort.pts) then break;
      AddLocMinOp(op, ort);
      goingUp := true;
    end;
    op := op.next;
  end;
  if not goingUp and goingUpAtStart then AddLocMinOp(op, ort);
end;
//------------------------------------------------------------------------------

function TClipperTri.PeekLocalMin(Y: Int64): Boolean;
begin
  Result := (FLocMinIdx < FLocMinList.Count)
    and (PLocalMin(FLocMinList[FLocMinIdx]).Y = Y);
end;
//------------------------------------------------------------------------------

procedure TClipperTri.PopLocalMin(Y: Int64;
  out localMin: PLocalMin);
begin
  localMin := FLocMinList[FLocMinIdx];
  inc(FLocMinIdx);
end;
//------------------------------------------------------------------------------

procedure TClipperTri.InsertScanLine(const Y: Int64);
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

function TClipperTri.PopScanLine(out Y: Int64): Boolean;
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

procedure TClipperTri.PushHorz(horz: PActive2);
begin
  horz.nextHorz := FHorz;
  FHorz := horz;
end;
//------------------------------------------------------------------------------

function TClipperTri.PopHorz: PActive2;
begin
  result := FHorz;
  FHorz := result.nextHorz;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.DoHorizontal(horz: PActive2);
var
  right: PActive2;
begin
  if horz.top.X > horz.bot.X then
  begin
    while assigned(horz.next) and (horz.top.X > horz.next.currX) do
    begin
      SwapPositionsInAEL(horz, horz.next);
    end;
    if IsRightBound(horz) then right := horz
    else right := GetRight(horz);
    if assigned(right) and right.hlpIsMax then
      DoInternalMaxima(horz);
  end else
  begin
    if IsRightBound(horz) then right := horz
    else right := GetRight(horz);
    if assigned(right) and right.hlpIsMax then
      DoInternalMaxima(horz);
    while assigned(horz.prev) and (horz.top.X < horz.prev.currX) do
      SwapPositionsInAEL(horz.prev, horz);
  end;
  if not UpdateActive(horz, horz.bot.Y) then DisposeActive2(horz)
  else if IsHorizontal(horz) then PushHorz(horz);
end;
//------------------------------------------------------------------------------

function TClipperTri.SweepYMonotone: Boolean;
var
  Y: Int64;
begin
  Result := true;
  FHorz := nil;
  if not PopScanLine(Y) then Exit; //nothing to do, so all finished :)
  while Result do
  begin
    while PeekLocalMin(Y) do //todo - this peek is only for debugging
      InsertLocMinIntoScanline(Y);
    if not PopScanLine(Y) then break;
    if assigned(FActives2) then
      Result := DoYMonotoneScanline(Y);
    while assigned(FHorz) do DoHorizontal(PopHorz);
  end;
end;
//------------------------------------------------------------------------------

function TClipperTri.SweepTriangulate: Boolean;
var
  i: integer;
  op, op2, lowOp: TOutPt;
  ort: TOutRec;
begin
  result := true;
  for i := 0 to FOrecList.Count -1 do
  begin
    ort := FOrecList[i];
    if not assigned(ort.Pts) then continue;

    //get the lowermost vertex ...
    lowOp := ort.Pts;
    op := ort.Pts.Next;
    while op <> ort.Pts do
    begin
      if op.Pt.Y > lowOp.Pt.Y then lowOp := op;
      op := op.Next;
    end;

    op := lowOp.Next;
    op2 := lowOp.Prev;
    while op <> op2 do
    begin
      if op.Pt.Y > op2.Pt.Y then
      begin
        Triangulate(op, op2, true);
        op := op.Next;
      end else
      begin
        Triangulate(op2, op, false);
        op2 := op2.Prev;
      end;
    end;
    Triangulate(op, op, true);
    ort.Pts := op;
  end;
end;
//------------------------------------------------------------------------------

function TClipperTri.UpdateActive(a: PActive2; Y: Int64): Boolean;
var
  op: TOutPt;
begin
  result := not IsMaxima(a);
  op := a.opTop;
  if result then
  begin
    SetHelper(a, op);
    a.opTop := GetNextOp(a);
    a.bot := a.top;
    a.top := a.opTop.Pt;
    a.dx := GetDx(a.bot, a.top);
    a.currX := a.bot.X;
    if (a.top.Y < Y) then InsertScanLine(a.top.Y);
  end
  else if IsLeftBound(a) and IsCounterClockwise(op) then
    SetHelper(a, op, true); //internal maxima
end;
//------------------------------------------------------------------------------

function TClipperTri.InsertIntoAel1(a: PActive2): Boolean;
var
  prev: PActive2;
begin
  result := true;
  if not Assigned(FActives2) then
  begin
    a.next := nil;
    a.prev := nil;
    FActives2 := a;
    Exit;
  end;

  if IsValidOrder(a, FActives2) then
  begin
    a.prev := nil;
    a.next := FActives2;
    FActives2.prev := a;
    FActives2 := a;
  end else
  begin
    prev := FActives2;
    while Assigned(prev.next) and IsValidOrder(prev.next, a) do
      prev := prev.next;
    a.next := prev.next;
    if Assigned(a.next) then a.next.prev := a;
    a.prev := prev;
    prev.next := a;
  end;
end;
//----------------------------------------------------------------------

procedure TClipperTri.InsertIntoAel2(a, a2: PActive2);
begin
  a2.next := a.next;
  if Assigned(a2.next) then a2.next.prev := a2;
  a2.prev := a;
  a.next := a2;
end;
//----------------------------------------------------------------------

procedure TClipperTri.SwapPositionsInAEL(e1, e2: PActive2);
var
  prev, next: PActive2;
begin
  //preconditon: e1 must be immediately to the left of e2
  next := e2.Next;
  if Assigned(next) then next.Prev := e1;
  prev := e1.Prev;
  if Assigned(prev) then prev.Next := e2;
  e2.Prev := prev;
  e2.Next := e1;
  e1.Prev := e2;
  e1.Next := next;
  if not Assigned(e2.Prev) then FActives2 := e2;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.InsertLocMinIntoScanline(const Y: Int64);
var
  lm: PLocalMin;
  ort1, ort2: TOutRec;
  left, right, right2: PActive2;
  op1, op2, op3, op4: TOutPt;
  IsCW: Boolean;
begin
  PopLocalMin(Y, lm);
  ort1 := lm.op.outRec;

  IsCW := IsClockwise(lm.op);
  left := CreateActive(lm, IsCW);
  right := CreateActive(lm, not IsCW);

  if ort1.State < osOuter then
  begin
    if IsCW then
      ort1.State := osOuter else
      ort1.State := osInner;
  end;

  InsertIntoAel1(left);

  //Occasionally due to rounding a minima can be placed incorrectly such
  //that it's positioned outside its outer. So this needs checking ...
  while assigned(left.next) and (left.next.currX - left.currX < 5) and
    (right.dx <= left.next.dx) and not IsValidNext(left, right, left.next) do
  begin
    lm.op.Pt.X := left.next.currX;
    SwapPositionsInAEL(left, left.next);
  end;
  while assigned(left.prev) and (left.currX - left.prev.currX < 5) and
    (right.dx >= left.prev.dx) and not IsValidPrev(left, right, left.prev) do
  begin
    lm.op.Pt.X := left.prev.currX;
    SwapPositionsInAEL(left.prev, left);
  end;

  InsertIntoAel2(left, right);

  if not IsCW then
  begin
    right2 := GetOuterRight(right);

    if assigned(right2) then
      ort2 := right2.hlpOutPt.outRec else
      ort2 := nil;

    if IsInner(ort1) then
    begin
      //KEYHOLE JOINING ...
      if not assigned(right2) then RaiseError;
      ort1.Pts := nil;
      UpdateOutPts(lm.op, lm.op, ort2);
      op1 := lm.op;
      op2 := DuplicateOp(op1);
      ort2.Pts := op2;
      op3 := right2.hlpOutPt;
      op4 := DuplicateOp(op3);
      if not assigned(op3) then RaiseError;
      op3.Next := op2; op2.Prev := op3;
      op1.Next := op4; op4.Prev := op1;
    end
    else if assigned(right2) and right2.hlpIsMax then
    begin
      //Y-MONOTONE INTERNAL MAXIMA (MINIMA-MAXIMA SPLIT)
      if not assigned(right2) then RaiseError;
      right2.hlpIsMax := false;
      op1 := lm.op;
      ort1.Pts := op1;
      op2 := DuplicateOp(op1); //op1 is left of op2
      op3 := right2.hlpOutPt;
      op4 := DuplicateOp(op3); //op4 is left of op3
      //now split the path ...
      op1.Next := op4; op4.Prev := op1;
      op3.Next := op2; op2.Prev := op3;

      ort2 := TOutRec.Create;
      ort2.Pts := op2;
      ort2.Idx := ort1.Idx;
      ort2.State := osOuter;
      FOrecList.Add(ort2);
      UpdateOutPts(op4, op1, ort1);
      UpdateOutPts(op2, op3, ort2);
    end else
    begin
      //Y-MONOTONE INTERNAL MINIMA (MINIMA-HELPER SPLIT) ...
      op1 := lm.op;
      ort1.Pts := op1;
      op2 := DuplicateOp(op1); //op1 is left of op2
      op3 := right2.hlpOutPt;
      op4 := DuplicateOp(op3); //op4 is left of op3
      //now split the path ...
      op1.Next := op4; op4.Prev := op1;
      op3.Next := op2; op2.Prev := op3;

      ort2 := op3.outRec;
      op3.outRec := nil; //temp. flag
      UpdateOutPts(op4, op1, ort1);
      if op3.outRec <> ort1 then
      begin
        ort2 := TOutRec.Create;
        ort2.Pts := op3;
        ort2.Idx := ort1.Idx;
        ort2.State := osOuter;
        FOrecList.Add(ort2);
        UpdateOutPts(op2, op2, ort2);
      end else
        ort2.Pts := nil; //this turns out to be an inner join
    end;
  end; //if not IsCW

  if not IsHorizontal(left) then
  begin
    SetHelper(left, GetPrevOp(left));
    InsertScanLine(left.top.Y);
  end else
    DoHorizontal(left);

  if not IsHorizontal(right) then
  begin
    SetHelper(right, GetPrevOp(right));
    InsertScanLine(right.top.Y);
  end else
    DoHorizontal(right);
end;
//------------------------------------------------------------------------------

function TClipperTri.AdjustHorzHelper(var opHlp: TOutPt; const top: TPoint64): TOutPt;
begin
  if (opHlp.Next.Pt.Y = opHlp.Pt.Y) and
    ((opHlp.Next.Pt.X > opHlp.Pt.X) = (opHlp.Next.Pt.X < top.X)) then
      result := opHlp.Next
  else if (opHlp.Prev.Pt.Y = opHlp.Pt.Y) and
    ((opHlp.Prev.Pt.X > opHlp.Pt.X) = (opHlp.Prev.Pt.X < top.X)) then
      result := opHlp.Prev
  else
    result := opHlp;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.DoInternalMaxima(a: PActive2);
var
  right: PActive2;
  op1,op2,op3,op4: TOutPt;
  ort1, ort2: TOutRec;
  IsMax: Boolean;
begin
  if IsRightBound(a) then
    right := a else
    right := GetRight(a);

  right.hlpIsMax := false; //stops repeating

  if a.opTop = right.hlpOutPt then Exit;
  IsMax := IsMaxima(a);

  ort1 := a.opTop.outRec;
  ort1.Pts := a.opTop;
  op1 := a.opTop;                       //op1 : op2;
  op2 := DuplicateOp(op1);
  op3 := AdjustHorzHelper(right.hlpOutPt, op1.Pt);
  op4 := DuplicateOp(op3);              //op4 : op3;
  op1.Next := op4; op4.Prev := op1;
  op3.Next := op2; op2.Prev := op3;

  ort2 := TOutRec.Create;
  ort2.Pts := op2;
  ort2.Idx := ort1.Idx;
  ort2.State := osOuter;
  FOrecList.Add(ort2);

  UpdateOutPts(op4, op1, ort1);
  UpdateOutPts(op2, op3, ort2);

  if IsLeftBound(a) then a.opTop := op2
  else a.opTop := op1;

  if IsMax then
  begin
    if not IsMaxima(a) then //may not be maxima following split
      if a.opTop = op1 then a.opTop := op2
      else a.opTop := op1;

    if IsLeftBound(a) and not assigned(GetRight(a)) then
    begin
      if a.opTop = op1 then a.opTop := op2
      else a.opTop := op1;
    end;
  end else
    SetHelper(a, a.opTop);
end;
//------------------------------------------------------------------------------

function TClipperTri.DoYMonotoneScanline(Y : Int64): Boolean;
var
  a, next, right: PActive2;
begin
  Result := true;
  a := FActives2;
  a.currX := TopX(a, Y);
  while assigned(a) do
  begin
    next := a.next;
    if assigned(next) then next.currX := TopX(next, Y);

    if (a.top.Y = Y) then
    begin
      //GET PREVIOUS LINK (HELPER) INFO ...
      if IsLeftBound(a) then
      begin
        right := GetRight(a);
        if not assigned(right) then RaiseError;
      end else
        right := a;

      //Y-MONOTONE INTERNAL MAXIMA ...
      if right.hlpIsMax then
        DoInternalMaxima(a);

      //finally update the edge, skipping horizontals ...
      if not UpdateActive(a, Y) then DisposeActive2(a)
      else if IsHorizontal(a) then PushHorz(a);
    end;
    a := next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.Triangulate(op, altOp: TOutPt; isGoingFwd: Boolean);
var
  topY: Int64;
  op2, op3: TOutPt;
  crossProd: double;
begin
  topY := op.Pt.Y;
  op2 := GetPrevOp(op, isGoingFwd);
  while (op2 <> altOp) do
  begin
    op3 := GetPrevOp(op2, isGoingFwd);
    if (op3.Pt.Y < topY) then break;
    crossProd := CrossProduct(op3.Pt, op2.Pt, op.Pt);
    if (crossProd <> 0) then
    begin
      if isGoingFwd = (crossProd < 0) then break;
      if isGoingFwd then
        AddTriangle(op3.Pt, op2.Pt, op.Pt) else
        AddTriangle(op.Pt, op2.Pt, op3.Pt);
    end;
    DisposeOutPt(op2);
    op2 := op3;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.AddTriangle(const pt1, pt2, pt3: TPoint64);
var
  len: integer;
  p: TPath;
begin
  SetLength(p, 3);
  p[0] := pt1; p[1] := pt2; p[2] := pt3;
  len := Length(FTriangles);
  SetLength(FTriangles, len +1);
  FTriangles[len] := p;
end;
//------------------------------------------------------------------------------

procedure TClipperTri.DisposeActive2(a: PActive2);
begin
  if (a = FActives2) and (a.next = nil) then
  begin
    Dispose(a);
    FActives2 := nil;
  end else
  begin
    if (a = FActives2) then FActives2 := a.next;
    if assigned(a.prev) then a.prev.next := a.next;
    if assigned(a.next) then a.next.prev := a.prev;
    Dispose(a);
  end;
end;
//------------------------------------------------------------------------------

function TClipperTri.Execute(const srcPaths: TPaths;
  out triangles: TPaths): Boolean;
var
  i: integer;
  lm: PLocalMin;
begin
  try try
    FTriangles := nil;
    result := true;
    for i := 0 to high(srcPaths) do GetLocMins(srcPaths[i]);
    FLocMinList.Sort(LocMinListSort);

    if FLocMinList.Count > 0 then
    begin
      //use 'for ... downto' as inserting is more efficient in reverse order ...
      for i := FLocMinList.Count -1 downto 0 do
        InsertScanLine(PLocalMin(FLocMinList[i]).Y);

      //reverse paths if outer paths aren't clockwise ...
      if not IsClockwise(PLocalMin(FLocMinList[0]).op) then
        for i := 0 to FLocMinList.Count -1 do
        begin
          lm := PLocalMin(FLocMinList[i]);
          if lm.op.OutRec.State = osOpen then Continue;
          ReverseOutPts(lm.op);
          lm.op.OutRec.State := osOpen; //flag as reversed
        end;

      FActives2 := nil;
      //keyholing & y-monotone and then finally triangulate ...
      result := SweepYMonotone and SweepTriangulate;
      triangles := FTriangles;
    end;
  finally
    CleanUp;
  end;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function Triangulate(paths: TPaths): TPaths;
begin
  with TClipperTri.Create do
  try
    Execute(paths, result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Triangulate(paths: TPathsD): TPathsD;
var
  pp, sol: TPaths;
begin
  pp := ScalePaths(paths, 1000, 1000);
  pp := Union(pp, frEvenOdd); //belt and braces (remove later)
  with TClipperTri.Create do
  try
    Execute(pp, sol);
  finally
    Free;
  end;
  Result := ScalePathsD(sol, 0.001, 0.001);
end;
//------------------------------------------------------------------------------

end.
