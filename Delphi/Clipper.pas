unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  20 November 2018                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2017                                         *
* Purpose   :  Base clipping module                                            *
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
  SysUtils, Classes, Math;

type
  TPoint64 = record X, Y: Int64; end;
  TPointD = record X, Y: double; end;

  TPath = array of TPoint64;
  TPaths = array of TPath;

  TPathD = array of TPointD;
  TPathsD = array of TPathD;

  TRect64 = record Left, Top, Right, Bottom: Int64; end;

  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  TPathType = (ptSubject, ptClip);
  //By far the most widely used winding rules for polygon filling are EvenOdd
  //and NonZero (see GDI, GDI+, XLib, OpenGL, Cairo, AGG, Quartz, SVG, Gr32).
  //https://www.w3.org/TR/SVG/painting.html
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  PVertex = ^TVertex;
  TVertex = record
    Pt    : TPoint64;
    next  : PVertex;
    prev  : PVertex;
    flags : TVertexFlags;
  end;

  PVertexArray = ^TVertexArray;
  TVertexArray = array[0..MaxInt div sizeof(TVertex) -1] of TVertex;

  //Every closed path (or polygon) is made up of a series of vertices forming
  //edges that alternate between going up (relative to the Y-axis) and then
  //going down. Edges that consecutively go up or consecutively go down can be
  //grouped together into 'bounds' (or sides if they're simple convex polygons).
  //Local Minima are pointers to those vertices where descending bounds become
  //ascending bounds.

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    vertex    : PVertex;
    PolyType  : TPathType;
    IsOpen    : Boolean;
  end;

  TOutRec = class;

  PActive = ^TActive;
  TActive = record
    Bot      : TPoint64;
    Curr     : TPoint64;
    Top      : TPoint64;
    Dx       : Double;        //inverse of edge slope (zero = vertical)
    WindDx   : Integer;       //wind direction (ascending: +1; descending: -1)
    WindCnt  : Integer;       //current wind count
    WindCnt2 : Integer;       //current wind count of the opposite TPolyType
    OutRec   : TOutRec;
    //AEL - 'active edge' linked list (Vatti's AET - active edge table)
    PrevInAEL: PActive;
    NextInAEL: PActive;
    //SEL - 'sorted edge' linked list (Vatti's ST - sorted table)
    //    -  this is also (re)used in processing horizontals.
    PrevInSEL: PActive;
    NextInSEL: PActive;
    Jump     : PActive;       //for merge sorting (see BuildIntersectList())
    VertTop  : PVertex;
    LocMin   : PLocalMinima;  //a bottom of an edge 'bound' (Vatti)
  end;

  PScanLine = ^TScanLine;
  TScanLine = record
    Y        : Int64;
    Next     : PScanLine;
  end;

  TPolyTree = class;
  TPolyPath = class;

  TOutPt = class
    Pt       : TPoint64;
    Next     : TOutPt;
    Prev     : TOutPt;
  end;

  TOutRecState = (orOuter, orInner, orOpen);

  //OutRec: contains a path in the clipping solution. Edges in the AEL will
  //have the OutRec pointer assigned if they are part of the clipping solution.
  TOutRec = class
    Idx      : Integer;
    Owner    : TOutRec;
    frontE   : PActive;
    backE    : PActive;
    Pts      : TOutPt;
    PolyPath : TPolyPath;
    State    : TOutRecState;
  end;

  TClipper = class
  private
    FScalingFrac        : double;
    FScanLine           : PScanLine;
    FLocMinListSorted   : Boolean;
    FHasOpenPaths       : Boolean;
    FCurrentLocMinIdx   : Integer;
    FClipType           : TClipType;
    FFillRule           : TFillRule;
    FIntersectList      : TList;
    FOutRecList         : TList;
    FLocMinList         : TList;
    FActives            : PActive; //see AEL above
    FSel                : PActive; //see SEL above
    FVertexList         : TList;
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function PopScanLine(out Y: Int64): Boolean;
    function PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRec(index: Integer);
    procedure DisposeAllOutRecs;
    procedure DisposeVerticesAndLocalMinima;
    procedure AddPathToVertexList(const p: TPath;
      polyType: TPathType; isOpen: Boolean);
    function IsContributingClosed(e: PActive): Boolean;
    function IsContributingOpen(e: PActive): Boolean;
    procedure SetWindingLeftEdgeClosed(e: PActive);
    procedure SetWindingLeftEdgeOpen(e: PActive);
    procedure InsertLocalMinimaIntoAEL(const botY: Int64);
    procedure InsertLeftEdge(e: PActive);
    procedure PushHorz(e: PActive); {$IFDEF INLINING} inline; {$ENDIF}
    function PopHorz(out e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    procedure StartOpenPath(e: PActive; const pt: TPoint64);
    procedure UpdateEdgeIntoAEL(var e: PActive);
    procedure IntersectEdges(e1, e2: PActive; const pt: TPoint64);
    procedure DeleteFromAEL(e: PActive);
    procedure CopyActivesToSEL; {$IFDEF INLINING} inline; {$ENDIF}
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure ProcessIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure InsertNewIntersectNode(e1, e2: PActive; topY: Int64);
    procedure BuildIntersectList(const topY: Int64);
    procedure ProcessIntersectList;
    procedure FixupIntersectionOrder;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    procedure SwapPositionsInSEL(e1, e2: PActive);
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure ProcessHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    function DoMaxima(e: PActive): PActive;
    procedure SetScalingFrac(value: double);
    function AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
    procedure AddLocalMinPoly(e1, e2: PActive;
      const pt: TPoint64; IsNew: Boolean = false);
    procedure AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
    procedure JoinOutrecPaths(e1, e2: PActive);
  protected
    procedure CleanUp; //unlike Clear, CleanUp preserves added paths
    function ExecuteInternal(clipType: TClipType; fillRule: TFillRule): Boolean;
    procedure BuildResult(out closedPaths, openPaths: TPaths);
    procedure BuildResultTree(polyTree: TPolyTree; out openPaths: TPaths);
    property OutRecList: TList read FOutRecList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRect64;

    //ADDPATH & ADDPATHS METHODS ...
    //Integer paths (TPath) ...
    procedure AddPath(const path64: TPath; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;
    procedure AddPaths(const paths64: TPaths; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;
    //Floating point paths (TPathD) ...
    procedure AddPath(const pathD: TPathD; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;
    procedure AddPaths(const pathsD: TPathsD; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;

    //EXECUTE METHODS ...
    //Integer paths (TPath) ...
    function Execute(clipType: TClipType; out closedPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; overload; virtual;
    function Execute(clipType: TClipType; out closedPaths, openPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; overload; virtual;
    //Floating point paths (TPathD) ...
    function Execute(clipType: TClipType; out closedPaths: TPathsD;
      fillRule: TFillRule = frEvenOdd): Boolean; overload; virtual;
    function Execute(clipType: TClipType; out closedPaths, openPaths: TPathsD;
      fillRule: TFillRule = frEvenOdd): Boolean; overload; virtual;
    //Alternative TPolyTree structure to contain the solution's closed path ...
    function Execute(clipType: TClipType;
      var polytree: TPolyTree; out openPaths: TPaths;
      fillRule: TFillRule = frEvenOdd): Boolean; overload; virtual;

    property Scaling: double read FScalingFrac write SetScalingFrac;
  end;

  TPolyPath = class
  private
    FParent      : TPolyPath;
    FPath        : TPath;
    FChildList   : TList;
    FScalingFrac : double; //nb: set by TClipper object
    function     GetChildCnt: Integer;
    function     GetChild(index: Integer): TPolyPath;
    function     GetIsHole: Boolean;
    function     GetPathD: TPathD;
  protected
    function     AddChild(const path: TPath): TPolyPath;
  public
    constructor  Create;  virtual;
    destructor   Destroy; override;
    procedure    Clear;
    property     Parent: TPolyPath read FParent;
    property     IsHole: Boolean read GetIsHole;
    property     ChildCount: Integer read GetChildCnt;
    property     Child[index: Integer]: TPolyPath read GetChild;

    property     Path: TPath read FPath;            //Int64 coordinate paths
    property     PathD: TPathD read GetPathD;       //double coordinate paths
    property     Scaling: double read FScalingFrac; //read only!
  end;

  //TPolyTree: Intended as a read-only data structure for closed paths
  //returned by a clipping operation. This structure is much more complex
  //than the alternative TPaths structure, but it does preserve path ownership
  //(ie which paths contain (own) other paths).
  TPolyTree = class(TPolyPath);

  EClipperLibException = class(Exception);

function PointsEqual(const p1, p2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Int64): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const left, top, right, bottom: Int64): TRect64;

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths;
function PolyTreeToPathsD(PolyTree: TPolyTree): TPathsD;

const
  nullRect: TRect64 = (left:0; top: 0; right:0; Bottom: 0);

resourcestring
  rsClipper_OpenPathErr = 'Only subject paths can be open.';
  rsClipper_PolyTreeErr = 'The TPolyTree parameter must be assigned.';
  rsClipper_ClippingErr = 'Undefined clipping error';
  rsClipper_ScalingErr  =
    'The scaling fraction must be assigned before any paths are added.';
  rsClipper_ScalingErr2 =
    'The scaling fraction must be greater than zero.';

implementation

//OVERFLOWCHECKS OFF is a necessary workaround for a compiler bug that very
//occasionally reports incorrect overflow errors in Delphi versions before 10.2.
//see https://forums.embarcadero.com/message.jspa?messageID=871444
{$OVERFLOWCHECKS OFF}

type
  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    Edge1  : PActive;
    Edge2  : PActive;
    Pt     : TPoint64;
  end;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function IsHotEdge(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := assigned(e.OutRec);
end;
//------------------------------------------------------------------------------

function IsOpen(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.IsOpen;
end;
//------------------------------------------------------------------------------

function IsOpen(outrec: TOutRec): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = orOpen;
end;
//------------------------------------------------------------------------------

function IsFront(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  //the front edge will be the LEFT edge when it's an OUTER polygon
  //so that outer polygons will be orientated clockwise
  Result := (e = e.OutRec.frontE);
end;
//------------------------------------------------------------------------------

function IsInvalidPath(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := not assigned(op) or (op.Next = op);
end;
//------------------------------------------------------------------------------

procedure SwapSides(outRec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  e2 := outRec.frontE;
  outRec.frontE := outRec.backE;
  outRec.backE := e2;
  outRec.Pts := outRec.Pts.Next;
end;
//------------------------------------------------------------------------------

function FixSides(e: PActive): Boolean;
var
  e2: PActive;
begin
  result := true;
  e2 := e;
  while assigned(e2.PrevInAEL) do
  begin
    e2 := e2.PrevInAEL;
    if assigned(e2.OutRec) and not IsOpen(e2) then result := not result;
  end;
  if result <> IsFront(e) then
  begin
    if result  then e.OutRec.State := orOuter
    else e.OutRec.State := orInner;
    SwapSides(e.OutRec);
    Result := true;     //now fixed
  end
  else result := false; //seems OK
end;
//------------------------------------------------------------------------------

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

function TopX(e: PActive; const currentY: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (currentY = e.Top.Y) or (e.Top.X = e.Bot.X) then Result := e.Top.X
  else Result := e.Bot.X + Round(e.Dx*(currentY - e.Bot.Y));
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint64; const Y: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dx: double;
begin
  if (Y = pt1.Y) then Result := pt1.X
  else if (Y = pt2.Y) then Result := pt2.X
  else if (pt1.Y = pt2.Y) or (pt1.X = pt2.X) then Result := pt2.X
  else
  begin
    dx := GetDx(pt1, pt2);
    Result := pt1.X + Round(dx * (Y - pt1.Y));
  end;
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Top.Y = e.Bot.Y);
end;
//------------------------------------------------------------------------------

function IsHeadingRightHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = NegInfinity);
end;
//------------------------------------------------------------------------------

function IsHeadingLeftHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = Infinity);
end;
//------------------------------------------------------------------------------

function PointsEqual(const p1, p2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Int64): TPoint64;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Double): TPoint64;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Rect64(const left, top, right, bottom: Int64): TRect64;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Right := right;
  Result.Bottom := bottom;
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

function GetTopDeltaX(e1, e2: PActive): Int64; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if e1.Top.Y > e2.Top.Y then
    Result := TopX(e2, e1.Top.Y) - e1.Top.X else
    Result := e2.Top.X - TopX(e1, e2.Top.Y);
end;
//------------------------------------------------------------------------------

procedure SwapActives(var e1, e2: PActive); {$IFDEF INLINING} inline; {$ENDIF}
var
  e: PActive;
begin
  e := e1; e1 := e2; e2 := e;
end;
//------------------------------------------------------------------------------

function GetPolyType(const e: PActive): TPathType;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function IsSamePolyType(const e1, e2: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e1.LocMin.PolyType = e2.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(e1, e2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  //if parallel then return the current pt of e1 ...
  if (e1.Dx = e2.Dx) then
  begin
    Result.Y := e1.Curr.Y;
    Result.X := TopX(e1, Result.Y);
    Exit;
  end
  else if e1.Dx = 0 then
  begin
    Result.X := e1.Bot.X;
    if IsHorizontal(e2) then
      Result.Y := e2.Bot.Y
    else
    begin
      with e2^ do b2 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e2.Dx + b2);
    end;
  end
  else if e2.Dx = 0 then
  begin
    Result.X := e2.Bot.X;
    if IsHorizontal(e1) then
      Result.Y := e1.Bot.Y
    else
    begin
      with e1^ do b1 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e1.Dx + b1);
    end;
  end else
  begin
    with e1^ do b1 := Bot.X - Bot.Y * Dx;
    with e2^ do b2 := Bot.X - Bot.Y * Dx;
    m := (b2-b1)/(e1.Dx - e2.Dx);
    Result.Y := round(m);
    if Abs(e1.Dx) < Abs(e2.Dx) then
      Result.X := round(e1.Dx * m + b1) else
      Result.X := round(e2.Dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

(*******************************************************************************
*  Dx:                             0(90deg)                                    *
*                                  |                                           *
*               +inf (180deg) <--- o ---> -inf (0deg)                          *
*******************************************************************************)
procedure SetDx(e: PActive);  {$IFDEF INLINING} inline; {$ENDIF}
begin
  e.Dx := GetDx(e.Bot, e.Top);
end;
//------------------------------------------------------------------------------

function IsClockwise(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.WindDx > 0;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if IsClockwise(e) then
    Result := e.vertTop.next else
    Result := e.vertTop.prev;
end;
//------------------------------------------------------------------------------

function NextVertex(op: PVertex; isClockwise: Boolean): PVertex; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if isClockwise then Result := op.next
  else Result := op.prev;
end;
//------------------------------------------------------------------------------

function NextNextVertex(op: PVertex; isClockwise: Boolean): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if isClockwise then Result := op.next.next
  else Result := op.prev.prev;
end;
//------------------------------------------------------------------------------

function PrevPrevVertex(op: PVertex; isClockwise: Boolean): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if isClockwise then Result := op.prev.prev
  else Result := op.next.next;
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.flags;
end;
//------------------------------------------------------------------------------

procedure TerminateHotOpen(e: PActive);
begin
  if e.OutRec.frontE = e then
    e.OutRec.frontE := nil else
    e.OutRec.backE := nil;
  e.OutRec := nil;
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  if IsHorizontal(e) then
  begin
    //we can't be sure whether the MaximaPair is on the left or right, so ...
    Result := e.PrevInAEL;
    while assigned(Result) and (Result.Curr.X >= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.PrevInAEL;
    end;
    Result := e.NextInAEL;
    while assigned(Result) and (TopX(Result, e.Top.Y) <= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
  end else
  begin
    Result := e.NextInAEL;
    while assigned(Result) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function PointCount(pts: TOutPt): Integer; {$IFDEF INLINING} inline; {$ENDIF}
var
  p: TOutPt;
begin
  Result := 0;
  if not Assigned(pts) then Exit;
  p := pts;
  repeat
    Inc(Result);
    p := p.Next;
  until p = pts;
end;
//------------------------------------------------------------------------------

function BuildPath(op: TOutPt; scaling: double): TPath;
var
  i, j, opCnt: integer;
begin
  result := nil;
  opCnt := PointCount(op);
  if (opCnt < 2) then Exit;
  setLength(result, opCnt);
  if scaling = 1.0 then
  begin
    for i := 0 to opCnt -1 do
    begin
      result[i] := op.Pt;
      op := op.Next;
    end;
  end else
  begin
    result[0] := Point64(op.Pt.X / scaling, op.Pt.Y / scaling);
    j := 1;
    for i := 2 to opCnt do
    begin
      op := op.Next;
      result[j] := Point64(op.Pt.X / scaling, op.Pt.Y / scaling);
      if not PointsEqual(result[j-1], result[j]) then inc(j);
    end;
    if j < opCnt then setLength(result, j);
  end;
end;
//------------------------------------------------------------------------------

procedure DisposeOutPt(pp: TOutPt); {$IFDEF INLINING} inline; {$ENDIF}
begin
  pp.Prev.Next := pp.Next;
  pp.Next.Prev := pp.Prev;
  pp.Free;
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(pp: TOutPt);  {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: TOutPt;
begin
  pp.Prev.Next := nil;
  while Assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.Next;
    tmpPp.Free;
  end;
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  dy: Int64;
begin
  dy := PLocalMinima(item2).vertex.Pt.Y - PLocalMinima(item1).vertex.Pt.Y;
  if dy < 0 then Result := -1
  else if dy > 0 then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

procedure SetSides(outRec: TOutRec; startEdge, endEdge: PActive);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  outRec.frontE := startEdge;
  outRec.backE := endEdge;
end;
//------------------------------------------------------------------------------

procedure SwapOutRecs(e1, e2: PActive);
var
  or1, or2: TOutRec;
  e: PActive;
begin
  or1 := e1.OutRec;
  or2 := e2.OutRec;
  if (or1 = or2) then
  begin
    e := or1.frontE;
    or1.frontE := or1.backE;
    or1.backE := e;
    Exit;
  end;
  if assigned(or1) then
  begin
    if e1 = or1.frontE then
      or1.frontE := e2 else
      or1.backE := e2;
  end;
  if assigned(or2) then
  begin
    if e2 = or2.frontE then
      or2.frontE := e1 else
      or2.backE := e1;
  end;
  e1.OutRec := or2;
  e2.OutRec := or1;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: TOutPt);
var
  op1, op2: TOutPt;
begin
  if not Assigned(op) then Exit;
  op1 := op;
  repeat
    op2:= op1.Next;
    op1.Next := op1.Prev;
    op1.Prev := op2;
    op1 := op2;
  until op1 = op;
end;
//------------------------------------------------------------------------------

function ValidateIsOuter(e2: PActive): Boolean;
begin
  result := e2.OutRec.State = orOuter;
  e2 := e2.NextInAEL;
  while assigned(e2) do
  begin
   if IsHotEdge(e2) and not IsOpen(e2) then result := not result;
   e2 := e2.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function Area(op: TOutPt): Double;
var
  op2: TOutPt;
  d: Double;
begin
  Result := 0;
  op2 := op;
  if Assigned(op2) then
  repeat
    d := op2.Prev.Pt.X + op2.Pt.X;
    Result := Result + d * (op2.Prev.Pt.Y - op2.Pt.Y);
    op2 := op2.Next;
  until op2 = op;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

procedure TryFixOrientation(e: PActive);
var
  outrec: TOutRec;
begin
  outrec := e.OutRec;
  if (outrec.Pts = outrec.Pts.Next) or (outrec.Pts.Prev = outrec.Pts.Next) or
    ((Area(outrec.Pts) <= 0) = (outrec.State = orOuter)) or
    ValidateIsOuter(e) then Exit;

  if outrec.State = orInner then
    outrec.State := orOuter else
    outrec.State := orInner;
  SwapSides(outrec);
  ReverseOutPts(outrec.Pts);
end;
//------------------------------------------------------------------------------

procedure SetOwnerAndInnerOuterState(e: PActive);
var
  e2: PActive;
  outRec: TOutRec;
begin
  outRec := e.OutRec;
  if IsOpen(e) then
  begin
    outRec.Owner := nil;
    outRec.State := orOpen;
    Exit;
  end;
  //set owner ...
  if IsHeadingLeftHorz(e) then
  begin
    e2 := e.NextInAEL; //ie assess state from opposite direction
    while assigned(e2) and (not IsHotEdge(e2) or IsOpen(e2)) do
      e2 := e2.NextInAEL;
    if not assigned(e2) then outRec.Owner := nil
    else if (e2.OutRec.State = orOuter) = (e2.OutRec.frontE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end else
  begin
    e2 := e.PrevInAEL;
    while assigned(e2) and (not IsHotEdge(e2) or IsOpen(e2)) do
      e2 := e2.PrevInAEL;
    if not assigned(e2) then
      outRec.Owner := nil
    else if (e2.OutRec.State = orOuter) = (e2.OutRec.backE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end;

  //set inner/outer ...
  if not assigned(outRec.Owner) or (outRec.Owner.State = orInner) then
    outRec.State := orOuter else
    outRec.State := orInner;

end;
//------------------------------------------------------------------------------

function EdgesAdjacentInSel(node: PIntersectNode): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  with node^ do
    Result := (Edge1.NextInSEL = Edge2) or (Edge1.PrevInSEL = Edge2);
end;
//------------------------------------------------------------------------------

function DxToInt(dx: double): Integer; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if dx > 0 then result := 1
  else if dx < 0 then result := -1
  else result := 0;
end;
//------------------------------------------------------------------------------

function IntersectListSort(node1, node2: Pointer): Integer;
begin
  result := PIntersectNode(node2).Pt.Y - PIntersectNode(node1).Pt.Y;
  if (result = 0) and (node1 <> node2) then
    result := PIntersectNode(node1).Pt.X - PIntersectNode(node2).Pt.X;
end;

//------------------------------------------------------------------------------
// TClipper methods ...
//------------------------------------------------------------------------------

constructor TClipper.Create;
begin
  FLocMinList := TList.Create;
  FOutRecList := TList.Create;
  FIntersectList := TList.Create;
  FVertexList := TList.Create;
  FScalingFrac := 1.0;
end;
//------------------------------------------------------------------------------

destructor TClipper.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FIntersectList.Free;
  FVertexList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipper.CleanUp;
var
  dummy: Int64;
begin
  try
    //in case of exceptions ...
    while assigned(FActives) do DeleteFromAEL(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;

    DisposeScanLineList;
    DisposeAllOutRecs;
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Clear;
begin
  CleanUp;
  DisposeVerticesAndLocalMinima;
  FCurrentLocMinIdx := 0;
  FLocMinListSorted := false;
  FHasOpenPaths := False;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  i: Integer;
begin
  if not FLocMinListSorted then
  begin
    FLocMinList.Sort(LocMinListSort);
    FLocMinListSorted := true;
  end;
  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMinima(FLocMinList[i]).vertex.Pt.Y);
  FCurrentLocMinIdx := 0;
  FActives := nil;
  FSel := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetScalingFrac(value: double);
begin
  if FLocMinList.Count > 0 then
    raise EClipperLibException.Create(rsClipper_ScalingErr);
  if value <= 0 then
    raise EClipperLibException.Create(rsClipper_ScalingErr2);
  FScalingFrac := value;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  //The scanline list is a single-linked list of all the Y coordinates of
  //subject and clip vertices in the clipping operation (sorted descending).
  //However, only scanline Y's at Local Minima are inserted before clipping
  //starts. While scanlines are removed sequentially during the sweep operation,
  //new scanlines are only inserted whenever edge bounds are updated. This keeps
  //the scanline list relatively short, optimising performance.
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
    while Assigned(sl.Next) and (Y <= sl.Next.Y) do
      sl := sl.Next;
    if Y = sl.Y then Exit; //skip duplicates
    new(newSl);
    newSl.Y := Y;
    newSl.Next := sl.Next;
    sl.Next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.PopScanLine(out Y: Int64): Boolean;
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

function TClipper.PopLocalMinima(Y: Int64;
  out localMinima: PLocalMinima): Boolean;
begin
  Result := false;
  if FCurrentLocMinIdx = FLocMinList.Count then Exit;
  localMinima := PLocalMinima(FLocMinList[FCurrentLocMinIdx]);
  if (localMinima.vertex.Pt.Y = Y) then
  begin
    inc(FCurrentLocMinIdx);
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeScanLineList;
var
  sl: PScanLine;
begin
  while Assigned(FScanLine) do
  begin
    sl := FScanLine.Next;
    Dispose(FScanLine);
    FScanLine := sl;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(index: Integer);
var
  outRec: TOutRec;
begin
  outRec := FOutRecList[index];
  if Assigned(outRec.Pts) then DisposePolyPts(outRec.Pts);
  outRec.Free;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeAllOutRecs;
var
  i: Integer;
begin
  for i := 0 to FOutRecList.Count -1 do DisposeOutRec(i);
  FOutRecList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMinima(FLocMinList[i]));
  FLocMinList.Clear;
  for i := 0 to FVertexList.Count -1 do FreeMem(FVertexList[i]);
  FVertexList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPathToVertexList(const p: TPath;
  polyType: TPathType; isOpen: Boolean);
var
  i, j, pathLen: Integer;
  x,y: Int64;
  isFlat, goingUp, p0IsMinima, p0IsMaxima: Boolean;
  v: PVertex;
  va: PVertexArray;

  procedure AddLocMin(vert: PVertex);
  var
    lm: PLocalMinima;
  begin
    if vfLocMin in vert.flags then Exit; //ensures vertex is added only once
    Include(vert.flags, vfLocMin);
    new(lm);
    lm.vertex := vert;
    lm.PolyType := polyType;
    lm.IsOpen := isOpen;
    FLocMinList.Add(lm);                 //nb: sorted in Reset()
  end;
  //----------------------------------------------------------------------------

begin
  pathLen := length(p);
  while (pathLen > 1) and PointsEqual(p[pathLen -1], p[0]) do dec(pathLen);
  if (pathLen < 2) then Exit;

  p0IsMinima := false;
  p0IsMaxima := false;
  i := 1;
  //find the first non-horizontal segment in the path ...
  while (i < pathLen) and (p[i].Y = p[0].Y) do inc(i);
  isFlat := i = pathLen;
  if isFlat then
  begin
    if not isOpen then Exit;    //Ignore closed paths that have ZERO area.
    goingUp := false;           //And this just stops a compiler warning.
  end else
  begin
    goingUp := p[i].Y < p[0].Y; //because I'm using an inverted Y-axis display
    if goingUp then
    begin
      i := pathLen -1;
      while p[i].Y = p[0].Y do dec(i);
      p0IsMinima := p[i].Y < p[0].Y; //p[0].Y == a minima
    end else
    begin
      i := pathLen -1;
      while p[i].Y = p[0].Y do dec(i);
      p0IsMaxima := p[i].Y > p[0].Y; //p[0].Y == a maxima
    end;
  end;

  GetMem(va, sizeof(TVertex) * pathLen);
  FVertexList.Add(va);

  if FScalingFrac <> 1.0 then
    va[0].Pt := Point64(p[0].X * FScalingFrac, p[0].Y * FScalingFrac) else
    va[0].Pt := p[0];

  va[0].flags := [];
  if isOpen then
  begin
    include(va[0].flags, vfOpenStart);
    if goingUp then
      AddLocMin(@va[0]) else
      include(va[0].flags, vfLocMax);
  end;

  //nb: polygon orientation is determined later (see InsertLocalMinimaIntoAEL).
  i := 0;
  for j := 1 to pathLen -1 do
  begin
    if FScalingFrac <> 1.0 then
    begin
      x := Round(p[j].X * FScalingFrac);
      y := Round(p[j].Y * FScalingFrac);
      if (x = va[i].Pt.X) and (y = va[i].Pt.Y) then Continue; //duplicate
      va[j].Pt := Point64(x, y);
    end else
    begin
      if PointsEqual(p[j], va[i].Pt) then Continue;           //duplicate
      va[j].Pt := p[j];
    end;

    va[j].flags := [];
    va[i].next := @va[j];
    va[j].prev := @va[i];
    if (p[j].Y > p[i].Y) and goingUp then
    begin
      include(va[i].flags, vfLocMax);
      goingUp := false;
    end
    else if (p[j].Y < p[i].Y) and not goingUp then
    begin
      goingUp := true;
      AddLocMin(@va[i]);
    end;
    i := j;
  end;
  //i: index of the last vertex in the path.
  va[i].next := @va[0];
  va[0].prev := @va[i];

  if isOpen then
  begin
    include(va[i].flags, vfOpenEnd);
    if goingUp then
      include(va[i].flags, vfLocMax) else
      AddLocMin(@va[i]);
  end
  else if goingUp then
  begin
    //going up so find local maxima ...
    v := @va[i];
    while (v.Next.Pt.Y <= v.Pt.Y) do v := v.next;
    include(v.flags, vfLocMax);
    if p0IsMinima then AddLocMin(@va[0]); //ie just turned to going up
  end else
  begin
    //going down so find local minima ...
    v := @va[i];
    while (v.Next.Pt.Y >= v.Pt.Y) do v := v.next;
    AddLocMin(v);
    if p0IsMaxima then include(va[0].flags, vfLocMax);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPath(const path64: TPath; PolyType: TPathType;
  isOpen: Boolean);
begin
  if isOpen then
  begin
    if (PolyType = ptClip) then
      raise EClipperLibException.Create(rsClipper_OpenPathErr);
    FHasOpenPaths := true;
  end;
  FLocMinListSorted := false;
  AddPathToVertexList(path64, polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPath(const pathD: TPathD; PolyType: TPathType;
  isOpen: Boolean);
var
  i, len: integer;
  p: TPath;
begin
  len := length(pathD);
  setLength(p, len);
  for i := 0 to len -1 do
  begin
    p[i].X := Round(pathD[i].X * fScalingFrac);
    p[i].Y := Round(pathD[i].Y * fScalingFrac);
  end;
  AddPathToVertexList(p, polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths64: TPaths; polyType: TPathType;
  isOpen: Boolean);
var
  i: Integer;
begin
  for i := 0 to high(paths64) do AddPath(paths64[i], polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const pathsD: TPathsD; polyType: TPathType;
  isOpen: Boolean);
var
  i: Integer;
begin
  for i := 0 to high(pathsD) do AddPath(pathsD[i], polyType, isOpen);
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingClosed(e: PActive): Boolean;
begin
  Result := false;
  case FFillRule of
    frNonZero: if abs(e.WindCnt) <> 1 then Exit;
    frPositive: if (e.WindCnt <> 1) then Exit;
    frNegative: if (e.WindCnt <> -1) then Exit;
  end;

  case FClipType of
    ctIntersection:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
        frPositive: Result := (e.WindCnt2 > 0);
        frNegative: Result := (e.WindCnt2 < 0);
      end;
    ctUnion:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
        frPositive: Result := (e.WindCnt2 <= 0);
        frNegative: Result := (e.WindCnt2 >= 0);
      end;
    ctDifference:
      if GetPolyType(e) = ptSubject then
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
          frPositive: Result := (e.WindCnt2 <= 0);
          frNegative: Result := (e.WindCnt2 >= 0);
        end
      else
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
          frPositive: Result := (e.WindCnt2 > 0);
          frNegative: Result := (e.WindCnt2 < 0);
        end;
    ctXor:
        Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingOpen(e: PActive): Boolean;
begin
    case FClipType of
      ctIntersection:
        Result := (e.WindCnt2 <> 0);
      ctXor:
        Result := (e.WindCnt <> 0) <> (e.WindCnt2 <> 0);
      ctDifference:
        Result := (e.WindCnt2 = 0);
      else //ctUnion:
        Result := (e.WindCnt = 0) and (e.WindCnt2 = 0);
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindingLeftEdgeClosed(e: PActive);
var
  e2: PActive;
begin
  //Wind counts generally refer to polygon regions not edges, so here an edge's
  //WindCnt indicates the higher of the two wind counts of the regions touching
  //the edge. (Note also that adjacent region wind counts only ever differ
  //by one, and open paths have no meaningful wind directions or counts.)

  e2 := e.PrevInAEL;
  //find the nearest closed path edge of the same PolyType in AEL (heading left)
  while Assigned(e2) and (not IsSamePolyType(e2, e) or IsOpen(e2)) do
    e2 := e2.PrevInAEL;

  //todo: check open path with negative Winding
  if not Assigned(e2) then
  begin
    e.WindCnt := e.WindDx;
    e2 := FActives;
  end
  else if (FFillRule = frEvenOdd) then
  begin
    e.WindCnt := e.WindDx;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end else
  begin
    //NonZero, positive, or negative filling here ...
    //if e's WindCnt is in the SAME direction as its WindDx, then e is either
    //an outer left or a hole right boundary, so leftE must be inside 'e'.
    //(neither e.WindCnt nor e.WindDx should ever be 0)
    if (e2.WindCnt * e2.WindDx < 0) then
    begin
      //opposite directions so leftE is outside 'e' ...
      if (Abs(e2.WindCnt) > 1) then
      begin
        //outside prev poly but still inside another.
        if (e2.WindDx * e.WindDx < 0) then
          //reversing direction so use the same WC
          e.WindCnt := e2.WindCnt else
          //otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
          e.WindCnt := e2.WindCnt + e.WindDx;
      end
      //now outside all polys of same polytype so set own WC ...
      else e.WindCnt := e.WindDx;
    end else
    begin
      //leftE must be inside 'e'
      if (e2.WindDx * e.WindDx < 0) then
        //reversing direction so use the same WC
        e.WindCnt := e2.WindCnt
      else
        //otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
        e.WindCnt := e2.WindCnt + e.WindDx;
    end;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end;

  //update WindCnt2 ...
  if FFillRule = frEvenOdd then
    while (e2 <> e) do
    begin
      if IsSamePolyType(e2, e) or IsOpen(e2) then //do nothing
      else if e.WindCnt2 = 0 then e.WindCnt2 := 1
      else e.WindCnt2 := 0;
      e2 := e2.NextInAEL;
    end
  else
    while (e2 <> e) do
    begin
      if not IsSamePolyType(e2, e) and not IsOpen(e2) then
        Inc(e.WindCnt2, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindingLeftEdgeOpen(e: PActive);
var
  e2: PActive;
  cnt1, cnt2: Integer;
begin
  e2 := FActives;
  if FFillRule = frEvenOdd then
  begin
    cnt1 := 0;
    cnt2 := 0;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(cnt2)
      else if not IsOpen(e2) then inc(cnt1);
      e2 := e2.NextInAEL;
    end;
    if Odd(cnt1) then e.WindCnt := 1 else e.WindCnt := 0;
    if Odd(cnt2) then e.WindCnt2 := 1 else e.WindCnt2 := 0;
  end else
  begin
    //if FClipType in [ctUnion, ctDifference] then e.WindCnt := e.WindDx;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(e.WindCnt2, e2.WindDx)
      else if not IsOpen(e2) then inc(e.WindCnt, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidAelOrder(a1, a2: PActive): Boolean;
var
  pt1, pt2: TPoint64;
  op1, op2: PVertex;
  X: Int64;
begin
  if a2.Curr.X <> a1.Curr.X then
  begin
    Result := a2.Curr.X > a1.Curr.X;
    Exit;
  end;

  pt1 := a1.Bot; pt2 := a2.Bot;
  op1 := a1.VertTop; op2 := a2.VertTop;
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
        op2 := NextVertex(op2, IsClockwise(a2));
      end;
      pt1 := op1.Pt;
      op1 := NextVertex(op1, IsClockwise(a1));
    end else
    begin
      X := op2.Pt.X - TopX(pt1, op1.Pt, op2.Pt.Y);
      result := X > 0;
      if X <> 0 then Exit;
      pt2 := op2.Pt;
      op2 := NextVertex(op2, IsClockwise(a2));
    end;
    if (op1.Pt.Y > pt1.Y) then
    begin
      result := TurnsLeft(PrevPrevVertex(op1, IsClockwise(a1)).Pt, pt1, op1.Pt);
      Exit;
    end else if (op2.Pt.Y > pt2.Y) then
    begin
      result := TurnsLeft(op2.Pt, pt2, PrevPrevVertex(op2, IsClockwise(a2)).Pt);
      Exit;
    end;
  end;
  result := true;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLeftEdge(e: PActive);
var
  e2: PActive;
begin
  if not Assigned(FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := nil;
    FActives := e;
  end
  else if IsValidAelOrder(e, FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := FActives;
    FActives.PrevInAEL := e;
    FActives := e;
  end else
  begin
    e2 := FActives;
    while Assigned(e2.NextInAEL) and IsValidAelOrder(e2.NextInAEL, e) do
      e2 := e2.NextInAEL;
    e.NextInAEL := e2.NextInAEL;
    if Assigned(e2.NextInAEL) then e2.NextInAEL.PrevInAEL := e;
    e.PrevInAEL := e2;
    e2.NextInAEL := e;
  end;
end;
//----------------------------------------------------------------------

procedure InsertRightEdge(e, e2: PActive);
begin
  e2.NextInAEL := e.NextInAEL;
  if Assigned(e.NextInAEL) then e.NextInAEL.PrevInAEL := e2;
  e2.PrevInAEL := e;
  e.NextInAEL := e2;
end;
//----------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const botY: Int64);
var
  leftB, rightB: PActive;
  locMin: PLocalMinima;
  contributing: Boolean;
begin
  //Add local minima (if any) at BotY ...
  //nb: horizontal local minima edges should contain locMin.vertex.prev

  while PopLocalMinima(botY, locMin) do
  begin
    if (vfOpenStart in locMin.vertex.flags) then
    begin
      leftB := nil;
    end else
    begin
      new(leftB);
      FillChar(leftB^, sizeof(TActive), 0);
      leftB.LocMin := locMin;
      leftB.OutRec := nil;
      leftB.Bot := locMin.vertex.Pt;
      leftB.vertTop := locMin.vertex.prev; //ie descending
      leftB.Top := leftB.vertTop.Pt;
      leftB.Curr := leftB.Bot;
      leftB.WindCnt2 := 0;
      leftB.WindDx := -1;
      SetDx(leftB);
    end;

    if (vfOpenEnd in locMin.vertex.flags) then
    begin
      rightB := nil;
    end else
    begin
      new(rightB);
      FillChar(rightB^, sizeof(TActive), 0);
      rightB.LocMin := locMin;
      rightB.OutRec := nil;
      rightB.Bot := locMin.vertex.Pt;
      rightB.vertTop := locMin.vertex.next; //ie ascending
      rightB.Top := rightB.vertTop.Pt;
      rightB.Curr := rightB.Bot;
      rightB.WindCnt2 := 0;
      rightB.WindDx := 1;
      SetDx(rightB);
    end;
    //Currently LeftB is just the descending bound and RightB is the ascending.
    //Now if the LeftB isn't on the left of RightB then we need swap them.
    if assigned(leftB) and assigned(rightB) then
    begin
      if IsHorizontal(leftB) then
      begin
        if IsHeadingRightHorz(leftB) then SwapActives(leftB, rightB);
      end
      else if IsHorizontal(rightB) then
      begin
        if IsHeadingLeftHorz(rightB) then SwapActives(leftB, rightB);
      end
      else if (leftB.Dx < rightB.Dx) then SwapActives(leftB, rightB);
    end
    else if not assigned(leftB) then
    begin
      leftB := rightB;
      rightB := nil;
    end;

    InsertLeftEdge(leftB);                   ///////
    if IsOpen(leftB) then
    begin
      SetWindingLeftEdgeOpen(leftB);
      contributing := IsContributingOpen(leftB);
    end else
    begin
      SetWindingLeftEdgeClosed(leftB);
      contributing := IsContributingClosed(leftB);
    end;

    if assigned(rightB) then
    begin
      rightB.WindCnt := leftB.WindCnt;
      rightB.WindCnt2 := leftB.WindCnt2;
      InsertRightEdge(leftB, rightB);        ///////
      if contributing then
        AddLocalMinPoly(leftB, rightB, leftB.Bot, true);

      if IsHorizontal(rightB) then
        PushHorz(rightB) else
        InsertScanLine(rightB.Top.Y);
    end
    else if contributing then
      StartOpenPath(leftB, leftB.Bot);

    if IsHorizontal(leftB) then
      PushHorz(leftB) else
      InsertScanLine(leftB.Top.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.PushHorz(e: PActive);
begin
  if assigned(FSel) then
    e.NextInSEL := FSel else
    e.NextInSEL := nil;
  FSel := e;
end;
//------------------------------------------------------------------------------

function TClipper.PopHorz(out e: PActive): Boolean;
begin
  Result := assigned(FSel);
  if not Result then Exit;
  e := FSel;
  FSel := FSel.NextInSEL;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(e1, e2: PActive;
  const pt: TPoint64; IsNew: Boolean);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Pts := nil;
  outRec.PolyPath := nil;

  e1.OutRec := outRec;
  SetOwnerAndInnerOuterState(e1);
  e2.OutRec := outRec;

  if not IsOpen(e1) then
  begin
    //Setting the owner and inner/outer states (above) is an essential
    //precursor to setting edge 'sides' (ie left and right sides of output
    //polygons) and hence the orientation of output paths ...
    if (outRec.State = orOuter) = IsNew then
      SetSides(outRec, e1, e2) else
      SetSides(outRec, e2, e1);
  end;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
begin
  if not IsOpen(e1) and IsFront(e1) <> (e1.OutRec.State = orOuter) then
    TryFixOrientation(e1);

  AddOutPt(e1, pt);
  AddOutPt(e2, pt); //very occasionally this is necessary #177

  if  (e1.OutRec = e2.OutRec) then
  begin
    e1.outRec.frontE := nil;
    e1.outRec.backE := nil;
    e1.OutRec := nil;
    e2.OutRec := nil;
  end
  //and to preserve the winding orientation of Outrec ...
  else if e1.OutRec.Idx < e2.OutRec.Idx then
    JoinOutrecPaths(e1, e2) else
    JoinOutrecPaths(e2, e1);

end;
//------------------------------------------------------------------------------

procedure TClipper.JoinOutrecPaths(e1, e2: PActive);
var
  p1_start, p1_end, p2_start, p2_end: TOutPt;
begin
  if (IsFront(e1) = IsFront(e2)) then
  begin
    //one or other edge 'side' is wrong...
    if IsOpen(e1) then SwapSides(e2.OutRec)
    else if not FixSides(e1) and not FixSides(e2) then
      raise EClipperLibException.Create(rsClipper_ClippingErr);
    if e1.OutRec.Owner = e2.OutRec then
      e1.OutRec.Owner := e2.OutRec.Owner;
  end;

  //join e2 outrec path onto e1 outrec path and then delete e2 outrec path
  //pointers. (see joining_outpt.svg)
  p1_start :=  e1.OutRec.Pts;
  p2_start :=  e2.OutRec.Pts;
  p1_end := p1_start.Next;
  p2_end := p2_start.Next;

  if IsFront(e1) then
  begin
    p2_end.Prev := p1_start;
    p1_start.Next := p2_end;
    p2_start.Next := p1_end;
    p1_end.Prev := p2_start;
    e1.OutRec.Pts := p2_start;
    e1.OutRec.frontE := e2.OutRec.frontE;
    if not IsOpen(e1) then e1.OutRec.frontE.OutRec := e1.OutRec;
    //strip duplicates ...
    if (p2_end <> p2_start) and PointsEqual(p2_end.Pt, p2_end.Prev.Pt) then
      DisposeOutPt(p2_end);
  end else
  begin
    p1_end.Prev := p2_start;
    p2_start.Next := p1_end;
    p1_start.Next := p2_end;
    p2_end.Prev := p1_start;
    e1.OutRec.backE := e2.OutRec.backE;
    if not IsOpen(e1) then e1.OutRec.backE.OutRec := e1.OutRec;
    //strip duplicates ...
    if (p1_end <> p1_start) and PointsEqual(p1_end.Pt, p1_end.Prev.Pt) then
      DisposeOutPt(p1_end);
  end;

  if PointsEqual(e1.OutRec.Pts.Pt, e1.OutRec.Pts.Prev.Pt) and
    not IsInvalidPath(e1.OutRec.Pts) then
      DisposeOutPt(e1.OutRec.Pts.Prev);

  //after joining, the e2.OutRec must contains no vertices ...
  e2.OutRec.frontE := nil;
  e2.OutRec.backE := nil;
  e2.OutRec.Pts := nil;
  e2.OutRec.Owner := e1.OutRec; //this may be redundant

  //and e1 and e2 are maxima and are about to be dropped from the Actives list.
  e1.OutRec := nil;
  e2.OutRec := nil;
end;
//------------------------------------------------------------------------------

function TClipper.AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
var
  opFront, opBack: TOutPt;
  toFront: Boolean;
  outrec: TOutRec;
begin
  //Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
  //opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
  outrec := e.OutRec;
  toFront := IsFront(e);
  opFront := outrec.Pts;
  opBack := opFront.Next;
  if toFront and PointsEqual(pt, opFront.Pt) then
    result := opFront
  else if not toFront and PointsEqual(pt, opBack.Pt) then
    result := opBack
  else
  begin
    Result := TOutPt.Create;
    Result.Pt := pt;
    opBack.Prev := Result;
    Result.Prev := opFront;
    Result.Next := opBack;
    opFront.Next := Result;
    if toFront then outrec.Pts := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.StartOpenPath(e: PActive; const pt: TPoint64);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Owner := nil;
  outRec.State := orOpen;
  outRec.Pts := nil;
  outRec.PolyPath := nil;
  outRec.frontE := nil;
  outRec.backE := nil;
  e.OutRec := outRec;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var e: PActive);
begin
  e.Bot := e.Top;
  e.vertTop := NextVertex(e);
  e.Top := e.vertTop.Pt;
  e.Curr := e.Bot;
  SetDx(e);
  if not IsHorizontal(e) then InsertScanLine(e.Top.Y);
end;
//------------------------------------------------------------------------------

procedure TClipper.IntersectEdges(e1, e2: PActive; const pt: TPoint64);
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
begin
  //intersections with OPEN paths are managed differently to closed paths
  if FHasOpenPaths and (IsOpen(e1) or IsOpen(e2)) then
  begin
    if (IsOpen(e1) and IsOpen(e2) ) then Exit;
    //the following line avoids duplicating a whole lot of code ...
    if IsOpen(e2) then SwapActives(e1, e2);
    case FClipType of
      ctIntersection, ctDifference:
        if IsSamePolyType(e1, e2) or (abs(e2.WindCnt) <> 1) then Exit;
      ctUnion:
        if IsHotEdge(e1) <> ((abs(e2.WindCnt) <> 1) or
          (IsHotEdge(e1) <> (e2.WindCnt2 <> 0))) then Exit; //just works!
      ctXor:
        if (abs(e2.WindCnt) <> 1) then Exit;
    end;
    //toggle contribution ...
    if IsHotEdge(e1) then
    begin
      AddOutPt(e1, pt);
      TerminateHotOpen(e1);
    end
    else StartOpenPath(e1, pt);
    Exit;
  end;

  //update winding counts...
  //assumes that e1 will be to the right of e2 ABOVE the intersection
  if IsSamePolyType(e1, e2) then
  begin
    if FFillRule = frEvenOdd then
    begin
      e1WindCnt := e1.WindCnt;
      e1.WindCnt := e2.WindCnt;
      e2.WindCnt := e1WindCnt;
    end else
    begin
      if e1.WindCnt + e2.WindDx = 0 then
        e1.WindCnt := -e1.WindCnt else
        Inc(e1.WindCnt, e2.WindDx);
      if e2.WindCnt - e1.WindDx = 0 then
        e2.WindCnt := -e2.WindCnt else
        Dec(e2.WindCnt, e1.WindDx);
    end;
  end else
  begin
    if FFillRule <> frEvenOdd then Inc(e1.WindCnt2, e2.WindDx)
    else if e1.WindCnt2 = 0 then e1.WindCnt2 := 1
    else e1.WindCnt2 := 0;

    if FFillRule <> frEvenOdd then Dec(e2.WindCnt2, e1.WindDx)
    else if e2.WindCnt2 = 0 then e2.WindCnt2 := 1
    else e2.WindCnt2 := 0;
  end;

  case FFillRule of
    frPositive:
      begin
        e1WindCnt := e1.WindCnt;
        e2WindCnt := e2.WindCnt;
      end;
    frNegative:
      begin
        e1WindCnt := -e1.WindCnt;
        e2WindCnt := -e2.WindCnt;
      end;
    else
      begin
        e1WindCnt := abs(e1.WindCnt);
        e2WindCnt := abs(e2.WindCnt);
      end;
  end;

  if IsHotEdge(e1) and IsHotEdge(e2) then
  begin
    if not (e1WindCnt in [0,1]) or not (e2WindCnt in [0,1]) or
      (not IsSamePolyType(e1, e2) and (fClipType <> ctXor)) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
    end else if IsFront(e1) or (e1.OutRec = e2.OutRec) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
      AddLocalMinPoly(e1, e2, pt);
    end else
    begin
      //right & left bounds touching, NOT maxima & minima ...
      AddOutPt(e1, pt);
      AddOutPt(e2, pt);
      SwapOutRecs(e1, e2);
    end;
  end else if IsHotEdge(e1) then
  begin
    if (e2WindCnt = 0) or (e2WindCnt = 1) then
    begin
      AddOutPt(e1, pt);
      SwapOutRecs(e1, e2);
    end;
  end
  else if IsHotEdge(e2) then
  begin
    if (e1WindCnt = 0) or (e1WindCnt = 1) then
    begin
      AddOutPt(e2, pt);
      SwapOutRecs(e1, e2);
    end;
  end
  else if  ((e1WindCnt = 0) or (e1WindCnt = 1)) and
    ((e2WindCnt = 0) or (e2WindCnt = 1)) then
  begin
    //neither Edge is currently contributing ...
    case FFillRule of
      frPositive:
      begin
        e1WindCnt2 := e1.WindCnt2;
        e2WindCnt2 := e2.WindCnt2;
      end;
      frNegative:
      begin
        e1WindCnt2 := -e1.WindCnt2;
        e2WindCnt2 := -e2.WindCnt2;
      end
      else
      begin
        e1WindCnt2 := abs(e1.WindCnt2);
        e2WindCnt2 := abs(e2.WindCnt2);
      end;
    end;

    if not IsSamePolyType(e1, e2) then
    begin
      AddLocalMinPoly(e1, e2, pt);
    end
    else if (e1WindCnt = 1) and (e2WindCnt = 1) then
      case FClipType of
        ctIntersection:
          if (e1WindCnt2 > 0) and (e2WindCnt2 > 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctUnion:
          if (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctDifference:
          if ((GetPolyType(e1) = ptClip) and (e1WindCnt2 > 0) and
            (e2WindCnt2 > 0)) or ((GetPolyType(e1) = ptSubject) and
            (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0)) then
              AddLocalMinPoly(e1, e2, pt);
        ctXor:
          AddLocalMinPoly(e1, e2, pt);
      end
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromAEL(e: PActive);
var
  aelPrev, aelNext: PActive;
begin
  aelPrev := e.PrevInAEL;
  aelNext := e.NextInAEL;
  if not Assigned(aelPrev) and not Assigned(aelNext) and
    (e <> FActives) then Exit; //already deleted
  if Assigned(aelPrev) then aelPrev.NextInAEL := aelNext
  else FActives := aelNext;
  if Assigned(aelNext) then aelNext.PrevInAEL := aelPrev;
  Dispose(e);
end;
//------------------------------------------------------------------------------

procedure TClipper.CopyActivesToSEL;
var
  e: PActive;
begin
  FSel := FActives;
  e := FActives;
  while Assigned(e) do
  begin
    e.PrevInSEL := e.PrevInAEL;
    e.NextInSEL := e.NextInAEL;
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.AdjustCurrXAndCopyToSEL(topY: Int64);
var
  e: PActive;
begin
  FSel := FActives;
  e := FActives;
  while Assigned(e) do
  begin
    e.PrevInSEL := e.PrevInAEL;
    e.NextInSEL := e.NextInAEL;
    e.Curr.X := TopX(e, topY);
    //nb: don't update e.Curr.Y yet (see InsertNewIntersectNode)
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.ExecuteInternal(clipType: TClipType; fillRule: TFillRule): Boolean;
var
  Y: Int64;
  e: PActive;
begin
  Result := clipType = ctNone;
  if Result then Exit;
  try
    FFillRule := fillRule;
    FClipType := clipType;
    Reset;
    if not PopScanLine(Y) then Exit;
    while true do
    begin
      InsertLocalMinimaIntoAEL(Y);
      while PopHorz(e) do ProcessHorizontal(e);
      if not PopScanLine(Y) then Break; //Y now top of scanbeam
      ProcessIntersections(Y);
      DoTopOfScanbeam(Y);
    end;
    Result := True;
  except;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; out closedPaths: TPaths;
  fillRule: TFillRule): Boolean;
var
  dummy: TPaths;
begin
  closedPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if Result then BuildResult(closedPaths, dummy);
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; out closedPaths, openPaths: TPaths;
  fillRule: TFillRule = frEvenOdd): Boolean;
begin
  closedPaths := nil;
  openPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if Result then BuildResult(closedPaths, openPaths);
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  out closedPaths: TPathsD; fillRule: TFillRule): Boolean;
var
  i, j, len, len2: integer;
  pp, dummy: TPaths;
begin
  closedPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if not Result then Exit;
    BuildResult(pp, dummy);
    len := length(pp);
    setLength(closedPaths, len);
    for i := 0 to len -1 do
    begin
      len2 := length(pp[i]);
      setlength(closedPaths[i], len2);
      for j := 0 to len2 -1 do
      begin
        closedPaths[i][j].X := pp[i][j].X / FScalingFrac;
        closedPaths[i][j].Y := pp[i][j].Y / FScalingFrac;
      end;
    end;
  finally
    CleanUp;
  end;

end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  out closedPaths, openPaths: TPathsD; fillRule: TFillRule): Boolean;
var
  i, j, len, len2: integer;
  cp, op: TPaths;
begin
  closedPaths := nil;
  openPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if Result then BuildResult(cp, op);

    len := length(cp);
    setLength(closedPaths, len);
    for i := 0 to len -1 do
    begin
      len2 := length(cp[i]);
      setlength(closedPaths[i], len2);
      for j := 0 to len2 -1 do
      begin
        closedPaths[i][j].X := cp[i][j].X / FScalingFrac;
        closedPaths[i][j].Y := cp[i][j].Y / FScalingFrac;
      end;
    end;

    len := length(op);
    setLength(openPaths, len);
    for i := 0 to len -1 do
    begin
      len2 := length(op[i]);
      setlength(openPaths[i], len2);
      for j := 0 to len2 -1 do
      begin
        openPaths[i][j].X := op[i][j].X / FScalingFrac;
        openPaths[i][j].Y := op[i][j].Y / FScalingFrac;
      end;
    end;

  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; var polytree: TPolyTree;
  out openPaths: TPaths; fillRule: TFillRule): Boolean;
begin
  if not assigned(polytree) then
    raise EClipperLibException.Create(rsClipper_PolyTreeErr);
  polytree.Clear;
  openPaths := nil;
  try
    Result := ExecuteInternal(clipType, fillRule);
    if Result then BuildResultTree(polytree, openPaths);
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersections(const topY: Int64);
var
  cnt: integer;
begin
  BuildIntersectList(topY);
  cnt := FIntersectList.Count;
  if (cnt = 0) then Exit;
  try
    if (cnt > 1) then FixupIntersectionOrder;
    ProcessIntersectList;
  finally
    DisposeIntersectNodes; //clean up only needed if there's been an error
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeIntersectNodes;
var
  i: Integer;
begin
  for i := 0 to FIntersectList.Count - 1 do
    Dispose(PIntersectNode(FIntersectList[i]));
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertNewIntersectNode(e1, e2: PActive; topY: Int64);
var
  pt: TPoint64;
  node: PIntersectNode;
begin
  pt := GetIntersectPoint(e1, e2);
  //Rounding errors can occasionally place the calculated intersection
  //point either below or above the scanbeam, so check and correct ...
  if (pt.Y > e1.Curr.Y) then
  begin
    //E.Curr.Y is still at the bottom of scanbeam here
    pt.Y := e1.Curr.Y;
    //use the more vertical of the 2 edges to derive pt.X ...
    if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := TopX(e1, pt.Y) else
      pt.X := TopX(e2, pt.Y);
  end
  else if pt.Y < topY then
  begin
    //TopY = top of scanbeam
    pt.Y := topY;
    if e1.Top.Y = topY then
      pt.X := e1.Top.X
    else if e2.Top.Y = topY then
      pt.X := e2.Top.X
    else if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := e1.Curr.X
    else
      pt.X := e2.Curr.X;
  end;

  new(node);
  node.Edge1 := e1;
  node.Edge2 := e2;
  node.Pt := pt;
  FIntersectList.Add(node);
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildIntersectList(const topY: Int64);
var
  i, lCnt, rCnt, mul: integer;
  first, second, base, prevBase, p, n, tmp: PActive;
begin
  if not Assigned(FActives) or not Assigned(FActives.NextInAEL) then Exit;
  AdjustCurrXAndCopyToSEL(topY);

  //Merge sort FActives into their new positions at the top of scanbeam, and
  //create an intersection node every time an edge crosses over another ...
  //see https://stackoverflow.com/a/46319131/359538
	mul := 1;
	while (true) do
  begin
    first := FSel;
    prevBase := nil;
		//sort successive larger 'mul' count of nodes ...
		while assigned(first) do
    begin
			if (mul = 1) then
      begin
        second := first.NextInSEL;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.NextInSEL;
      end else
      begin
			  second := first.Jump;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.Jump;
      end;

      //now sort first and second groups ...
      base := first;
			lCnt := mul; rCnt := mul;
			while (lCnt > 0) and (rCnt > 0) do
			begin
				if (first.Curr.X > second.Curr.X) then
        begin
          // create one or more Intersect nodes ///////////
          tmp := second.PrevInSEL;
          for i := 1 to lCnt do
          begin
            //create a new intersect node...
            InsertNewIntersectNode(tmp, second, topY);
            tmp := tmp.PrevInSEL;
          end;

          if (first = base) then
          begin
            if assigned(prevBase) then prevBase.Jump := second;
            base := second;
            base.Jump := first.Jump;
            if (first.PrevInSEL = nil) then FSel := second;
          end;
          tmp := second.NextInSEL;

          //now move the out of place edge to it's new position in SEL ...
          //remove second from list ...
          p := second.PrevInSEL;
          n := second.NextInSEL;
          p.NextInSEL := n; //always a prev since we're moving from right to left
          if Assigned(n) then n.PrevInSEL := p;
          //insert back into list ...
          p := first.PrevInSEL;
          if assigned(p) then p.NextInSEL := second;
          first.PrevInSEL := second;
          second.PrevInSEL := p;
          second.NextInSEL := first;

          second := tmp;
          if not assigned(second) then break;
          dec(rCnt);
        end else
        begin
          first := first.NextInSEL;
          dec(lCnt);
        end;
      end;
      first := base.Jump;
      prevBase := base;
    end;
    if FSel.Jump = nil then Break
    else mul := mul shl 1;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersectList;
var
  i: Integer;
begin
  for i := 0 to FIntersectList.Count - 1 do
  begin
    with PIntersectNode(FIntersectList[i])^ do
    begin
      IntersectEdges(Edge1, Edge2, Pt);
      SwapPositionsInAEL(Edge1, Edge2);
    end;
    dispose(PIntersectNode(FIntersectList[i]));
  end;
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.FixupIntersectionOrder;
var
  i, j, cnt: Integer;
  node: PIntersectNode;
begin
  cnt := FIntersectList.Count;
  //It's important that edge intersections are processed from the bottom up,
  //but it's also crucial that intersections only occur between adjacent edges.
  //The first sort here (a quicksort), arranges intersections relative to their
  //vertical positions within the scanbeam ...
  FIntersectList.Sort(IntersectListSort);

  //Now we simulate processing these intersections, and as we do, we make sure
  //that the intersecting edges remain adjacent. If they aren't, this simulated
  //intersection is delayed until such time as these edges do become adjacent.
  CopyActivesToSEL;
  for i := 0 to cnt - 1 do
  begin
    if not EdgesAdjacentInSel(FIntersectList[i]) then
    begin
      j := i + 1;
      while (j < cnt) and not EdgesAdjacentInSel(FIntersectList[j]) do inc(j);
      //Swap IntersectNodes ...
      node := FIntersectList[i];
      FIntersectList[i] := FIntersectList[j];
      FIntersectList[j] := node;
    end;
    with PIntersectNode(FIntersectList[i])^ do
      SwapPositionsInSEL(Edge1, Edge2);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  //preconditon: e1 must be immediately to the left of e2
  next := e2.NextInAEL;
  if Assigned(next) then next.PrevInAEL := e1;
  prev := e1.PrevInAEL;
  if Assigned(prev) then prev.NextInAEL := e2;
  e2.PrevInAEL := prev;
  e2.NextInAEL := e1;
  e1.PrevInAEL := e2;
  e1.NextInAEL := next;
  if not Assigned(e2.PrevInAEL) then FActives := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInSEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  //preconditon: e1 must be immediately to the left of e2
  next := e2.NextInSEL;
  if Assigned(next) then next.PrevInSEL := e1;
  prev := e1.PrevInSEL;
  if Assigned(prev) then prev.NextInSEL := e2;
  e2.PrevInSEL := prev;
  e2.NextInSEL := e1;
  e1.PrevInSEL := e2;
  e1.NextInSEL := next;
  if not Assigned(e2.PrevInSEL) then FSel := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessHorizontal(horzEdge: PActive);
var
  e, maxPair: PActive;
  horzLeft, horzRight: Int64;
  isLeftToRight: Boolean;
  pt: TPoint64;
  isMax: Boolean;

  procedure ResetHorzDirection;
  var
    e: PActive;
  begin
    if (horzEdge.Bot.X = horzEdge.Top.X) then
    begin
      //the horizontal edge is going nowhere ...
      horzLeft := horzEdge.Curr.X;
      horzRight := horzEdge.Curr.X;
      e := horzEdge.NextInAEL;
      while assigned(e) and (e <> maxPair) do
        e := e.NextInAEL;
      isLeftToRight := assigned(e);
    end
    else if horzEdge.Curr.X < horzEdge.Top.X then
    begin
      horzLeft := horzEdge.Curr.X;
      horzRight := horzEdge.Top.X;
      isLeftToRight := true;
    end else
    begin
      horzLeft := horzEdge.Top.X;
      horzRight := horzEdge.Curr.X;
      isLeftToRight := false;
    end;
  end;
  //------------------------------------------------------------------------

begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with the bottom vertices of      *
* other HEs [#] and with non-horizontal edges [*]. Once these intersections    *
* are completed, intermediate HEs are 'promoted' to the next edge in their     *
* bounds, and they in turn may be intersected [%] by other HEs.                *
*                                                                              *
* eg: 3 horizontals at a scanline:  /   |                     /          /     *
*              |                   /    |    (HE3) o=========%==========o      *
*              o=======o (HE2)    /     |         /         /                  *
*         o============#=========*======*========#=========o (HE1)             *
*        /             |        /       |       /                              *
*******************************************************************************)

  //with closed paths, simplify consecutive horizontals into a 'single' edge ...
  if not IsOpen(horzEdge) then
  begin
    pt := horzEdge.Bot;
    while not IsMaxima(horzEdge) and
      (NextVertex(horzEdge).Pt.Y = pt.Y) do
        UpdateEdgeIntoAEL(horzEdge);
    horzEdge.Bot := pt;
    horzEdge.Curr := pt;
    //update Dx in case of direction change ...
    if horzEdge.Bot.X < horzEdge.Top.X then
      horzEdge.Dx := NegInfinity else
      horzEdge.Dx := Infinity;
  end;

  maxPair := nil;
  if IsMaxima(horzEdge) and (not IsOpen(horzEdge) or
      ([vfOpenStart, vfOpenEnd] * horzEdge.vertTop.flags = [])) then
        maxPair := GetMaximaPair(horzEdge);

  ResetHorzDirection;
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, horzEdge.Curr);

  while true do //loops through consec. horizontal edges (if open)
  begin
    isMax := IsMaxima(horzEdge);
    if isLeftToRight  then
      e := horzEdge.NextInAEL else
      e := horzEdge.PrevInAEL;

    while assigned(e) do
    begin
      //Break if we've gone past the end of the horizontal ...
      if (isLeftToRight and (e.Curr.X > horzRight)) or
        (not isLeftToRight and (e.Curr.X < horzLeft)) then Break;
      //or if we've got to the end of an intermediate horizontal edge ...
      if (E.Curr.X = horzEdge.Top.X) and not isMax and not IsHorizontal(e) then
      begin
        pt := NextVertex(horzEdge).Pt;
        if(isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
          (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
      end;

      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge)  then
        begin
          if isLeftToRight then
            AddLocalMaxPoly(horzEdge, e, horzEdge.Top) else
            AddLocalMaxPoly(e, horzEdge, horzEdge.Top);
        end;
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      pt := Point64(e.Curr.X, horzEdge.Curr.Y);
      if (isLeftToRight) then
      begin
        IntersectEdges(horzEdge, e, pt);
        SwapPositionsInAEL(horzEdge, e);
        e := horzEdge.NextInAEL;
      end else
      begin
        IntersectEdges(e, horzEdge, pt);
        SwapPositionsInAEL(e, horzEdge);
        e := horzEdge.PrevInAEL;
      end;
    end;

    //check if we've finished with (consecutive) horizontals ...
    if isMax or (NextVertex(horzEdge).Pt.Y <> horzEdge.Top.Y) then Break;

    //still more horizontals in bound to process ...
    UpdateEdgeIntoAEL(horzEdge);
    ResetHorzDirection;

    if IsOpen(horzEdge) then
    begin
      if IsMaxima(horzEdge) then maxPair := GetMaximaPair(horzEdge);
      if IsHotEdge(horzEdge) then AddOutPt(horzEdge, horzEdge.Bot);
    end;
  end;

  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, horzEdge.Top);

  if not IsOpen(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge) //this is the end of an intermediate horiz.
  else if not IsMaxima(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge)
  else if not assigned(maxPair) then //ie open at top
    DeleteFromAEL(horzEdge)
  else if IsHotEdge(horzEdge) then
      AddLocalMaxPoly(horzEdge, maxPair, horzEdge.Top)
  else
  begin
    DeleteFromAEL(maxPair); DeleteFromAEL(horzEdge);
  end;

end;
//------------------------------------------------------------------------------

procedure TClipper.DoTopOfScanbeam(Y: Int64);
var
  e: PActive;
begin
  FSel := nil; //FSel is reused to flag horizontals (see PushHorz below)
  e := FActives;
  while Assigned(e) do
  begin
    //nb: 'e' will never be horizontal here
    if (e.Top.Y = Y) then
    begin
      e.Curr := e.Top;     //needed for horizontal processing

      //this should avoid micro self-intersections
      //and has negligible impact on performance ...
      if assigned(e.PrevInAEL) and (e.PrevInAEL.Curr.X = e.Curr.X) and
        (e.PrevInAEL.Bot.Y <> Y) and  IsHotEdge(e.PrevInAEL) then
          AddOutPt(e.PrevInAEL, e.Curr);
      if assigned(e.NextInAEL) and (e.NextInAEL.Curr.X = e.Curr.X) and
        (e.NextInAEL.Top.Y <> Y) and IsHotEdge(e.NextInAEL) then
          AddOutPt(e.NextInAEL, e.Curr);

      if IsMaxima(e) then
      begin
        e := DoMaxima(e);  //TOP OF BOUND (MAXIMA)
        Continue;
      end else
      begin
        //INTERMEDIATE VERTEX ...
        UpdateEdgeIntoAEL(e);
        if IsHotEdge(e) then AddOutPt(e, e.Bot);
        if IsHorizontal(e) then
          PushHorz(e);     //horizontals are processed later
      end;
    end else
      e.Curr.Y := Y; //nb: e.Curr.X has already been updated

    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.DoMaxima(e: PActive): PActive;
var
  eNext, ePrev, eMaxPair: PActive;
begin
  ePrev := e.PrevInAEL;
  eNext := e.NextInAEL;
  Result := eNext;

  if IsOpen(e) and ([vfOpenStart, vfOpenEnd] * e.vertTop.flags <> []) then
  begin
    if IsHotEdge(e) then AddOutPt(e, e.Top);
    if not IsHorizontal(e) then
    begin
      if IsHotEdge(e) then TerminateHotOpen(e);
      DeleteFromAEL(e);
    end;
    Exit;
  end else
  begin
    eMaxPair := GetMaximaPair(e);
    if not assigned(eMaxPair) then Exit; //EMaxPair is a horizontal ...
  end;

  //only non-horizontal maxima here.
  //process any edges between maxima pair ...
  while (eNext <> eMaxPair) do
  begin
    IntersectEdges(e, eNext, e.Top);
    SwapPositionsInAEL(e, eNext);
    eNext := e.NextInAEL;
  end;

  if IsOpen(e) then
  begin
    if IsHotEdge(e) then
    begin
      if assigned(eMaxPair) then
        AddLocalMaxPoly(e, eMaxPair, e.Top) else
        AddOutPt(e, e.Top);
    end;
    if assigned(eMaxPair) then
      DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
      Result := FActives;
    Exit;
  end;

  //here E.NextInAEL == ENext == EMaxPair ...
  if IsHotEdge(e) then
    AddLocalMaxPoly(e, eMaxPair, e.Top);

  DeleteFromAEL(e);
  DeleteFromAEL(eMaxPair);
  if assigned(ePrev) then
    Result := ePrev.NextInAEL else
    Result := FActives;
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildResult(out closedPaths, openPaths: TPaths);
var
  i, j, cntClosed, cntOpen: Integer;
  outRec: TOutRec;
begin
  cntClosed := 0; cntOpen := 0;
  SetLength(closedPaths, FOutRecList.Count);
  SetLength(openPaths, FOutRecList.Count);
  for i := 0 to FOutRecList.Count -1 do
  begin
    outRec := FOutRecList[i];
    if not assigned(outRec.Pts) then Continue;

    if (outRec.State = orOpen) then
    begin
      openPaths[cntOpen] := BuildPath(outRec.Pts, FScalingFrac);
      if length(openPaths[cntOpen]) > 1 then inc(cntOpen);
    end else
    begin
      closedPaths[cntClosed] := BuildPath(outRec.Pts, FScalingFrac);
      j := length(closedPaths[cntClosed]);
      if (j > 2) and
        PointsEqual(closedPaths[cntClosed][0],closedPaths[cntClosed][j -1]) then
          setlength(closedPaths[cntClosed], j - 1);
      if j > 2 then inc(cntClosed);
    end;
  end;
  SetLength(closedPaths, cntClosed);
  SetLength(openPaths, cntOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.BuildResultTree(polyTree: TPolyTree; out openPaths: TPaths);
var
  i, j, cntOpen: Integer;
  outRec: TOutRec;
  path: TPath;
begin
  polyTree.FScalingFrac := self.FScalingFrac;
  setLength(openPaths, FOutRecList.Count);
  cntOpen := 0;
  for i := 0 to FOutRecList.Count -1 do
    if Assigned(FOutRecList[i]) then
    begin
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      if (outRec.State = orOpen) then
      begin
        openPaths[cntOpen] := BuildPath(outRec.Pts, FScalingFrac);
        if length(openPaths[cntOpen]) > 1 then inc(cntOpen);
      end else
      begin
        path := BuildPath(outRec.Pts, FScalingFrac);
        j := length(path);
        if (j > 2) and PointsEqual(path[0], path[j -1]) then
          setlength(path, j - 1);
        if j < 3 then continue;

        if assigned(outRec.Owner) and assigned(outRec.Owner.PolyPath) then
          outRec.PolyPath := outRec.Owner.PolyPath.AddChild(path) else
          outRec.PolyPath := polyTree.AddChild(path);
      end;
    end;
  setLength(openPaths, cntOpen);
end;
//------------------------------------------------------------------------------

function TClipper.GetBounds: TRect64;
var
  i: Integer;
  v, vStart: PVertex;
begin
  if FVertexList.Count = 0 then
    Result := Rect64(0, 0, 0, 0)
  else
    with PVertex(FVertexList[0]).Pt do
      Result := Rect64(X, Y, X, Y);
  for i := 0 to FVertexList.Count -1 do
  begin
    vStart := FVertexList[i];
    v := vStart;
    repeat
      if v.Pt.X < Result.Left then Result.Left := v.Pt.X
      else if v.Pt.X > Result.Right then Result.Right := v.Pt.X;
      if v.Pt.Y < Result.Top then Result.Top := v.Pt.Y
      else if v.Pt.Y > Result.Bottom then Result.Bottom := v.Pt.Y;
      v := v.next;
    until v = vStart;
  end;
end;

//------------------------------------------------------------------------------
//  TPolyPath class
//------------------------------------------------------------------------------

constructor TPolyPath.Create;
begin
  FChildList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TPolyPath.Destroy;
begin
  Clear;
  FChildList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TPolyPath.Clear;
var
  i: Integer;
begin
  for i := 0 to FChildList.Count -1 do
    TPolyPath(FChildList[i]).Free;
  FChildList.Clear;
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetChild(index: Integer): TPolyPath;
begin
  if (index < 0) or (index >= FChildList.Count) then
    Result := nil else
    Result := TPolyPath(FChildList[index]);
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetIsHole: Boolean;
begin
  Result := not assigned(FParent) or not FParent.GetIsHole;
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetChildCnt: Integer;
begin
  Result := FChildList.Count;
end;
//------------------------------------------------------------------------------

function TPolyPath.AddChild(const path: TPath): TPolyPath;
begin
  Result := TPolyPath.Create;
  Result.FPath := path;
  FChildList.Add(Result);
  Result.FParent := self;
end;
//------------------------------------------------------------------------------

function TPolyPath.GetPathD: TPathD;
var
  i, len: integer;
begin
  len := length(FPath);
  setLength(Result, len);
  if FScalingFrac = 1 then
  begin
    for i := 0 to len -1 do
    begin
      Result[i].X := FPath[i].X;
      Result[i].Y := FPath[i].Y;
    end;
  end else
  begin
    for i := 0 to len -1 do
    begin
      Result[i].X := FPath[i].X / FScalingFrac;
      Result[i].Y := FPath[i].Y / FScalingFrac;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPaths(Poly: TPolyPath; var Paths: TPaths);
var
  i: Integer;
begin
  if (Length(Poly.Path) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Path;
  end;

  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPaths(Poly.Child[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths;
begin
  Result := nil;
  AddPolyNodeToPaths(PolyTree, Result);
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPathsD(Poly: TPolyPath; var Paths: TPathsD);
var
  i: Integer;
begin
  if (Length(Poly.Path) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.GetPathD;
  end;

  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPathsD(Poly.Child[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPathsD(PolyTree: TPolyTree): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------

end.

