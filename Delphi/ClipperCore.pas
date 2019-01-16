unit ClipperCore;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  14 January 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Core Clipper Library module                                     *
*              Contains structures and functions used throughout the library   *
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
  Classes, SysUtils, Math;

type
  TPoint64 = record X, Y: Int64; end;
  TPointD = record X, Y: double; end;

  //TPath: a simple data structure to represent a series of vertices, whether
  //open (poly-line) or closed (polygon). A path may be simple or complex (self
  //intersecting). For simple polygons, path orientation (whether clockwise or
  //counter-clockwise) is generally used to differentiate outer paths from inner
  //paths (holes). For complex polygons (and also for overlapping polygons),
  //explicit 'filling rules' (see below) are used to indicate regions that are
  //inside (filled) and regions that are outside (unfilled) a specific polygon.
  TPath = array of TPoint64;
  TPaths = array of TPath;
  TArrayOfPaths = array of TPaths;

  TPathD = array of TPointD;
  TPathsD = array of TPathD;

  TRect64 = {$IFDEF UNICODE}record{$ELSE}object{$ENDIF}
  private
    function GetWidth: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : Int64;
    Top    : Int64;
    Right  : Int64;
    Bottom : Int64;
    property Width: Int64 read GetWidth;
    property Height: Int64 read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TRectD = {$ifdef UNICODE}record{$else}object{$endif}
  private
    function GetWidth: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : double;
    Top    : double;
    Right  : double;
    Bottom : double;
    property Width: double read GetWidth;
    property Height: double read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TClipType = (ctNone, ctIntersection, ctUnion, ctDifference, ctXor);
  TPathType = (ptSubject, ptClip);
  //By far the most widely used winding rules for polygon filling are EvenOdd
  //and NonZero (see GDI, GDI+, XLib, OpenGL, Cairo, AGG, Quartz, SVG, Gr32).
  //https://www.w3.org/TR/SVG/painting.html
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TPointInPolygonResult = (pipInside, pipOutside, pipOn);

  EClipperLibException = class(Exception);

function PointsEqual(const p1, p2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Int64): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double): TPoint64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double): TPointD;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const left, top, right, bottom: Int64): TRect64;
function RectD(const left, top, right, bottom: double): TRectD;
function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
function GetBounds(const paths: TPaths): TRect64; overload;
function GetBounds(const paths: TPathsD): TRectD; overload;

function InflateRect(const rec: TRect64; dx, dy: Int64): TRect64;
function UnionRects(const rec, rec2: TRect64): TRect64;

//Area: result is type double to avoid potential integer overflow
function Area(const path: TPath): Double;
function Orientation(const path: TPath): Boolean;
function PointInPolygon(const pt: TPoint64;
  const path: TPath): TPointInPolygonResult;
function CrossProduct(const pt1, pt2, pt3: TPoint64): double;

function ScalePaths(const paths: TPaths; sx, sy: double): TPaths;
function ScalePathsD(const paths: TPaths; sx, sy: double): TPathsD; overload;
function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD; overload;
function OffsetPaths(const paths: TPaths; dx, dy: Int64): TPaths;
function OffsetPathsD(const paths: TPathsD; dx, dy: double): TPathsD;

function ReversePath(const path: TPath): TPath;
function ReversePaths(const paths: TPaths): TPaths;

procedure AppendPaths(var paths: TPaths; const extra: TPaths);

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths;

const
  nullRect: TRect64 = (left:0; top: 0; right:0; Bottom: 0);
  nullRectD: TRectD = (left:0.0; top: 0.0; right:0.0; Bottom: 0.0);

implementation

//------------------------------------------------------------------------------
// TRect64 methods ...
//------------------------------------------------------------------------------

function TRect64.GetWidth: Int64;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRect64.GetHeight: Int64;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRect64.GetIsEmpty: Boolean;
begin
  result := (bottom <= top) or (right <= left);
end;

//------------------------------------------------------------------------------
// TRectD methods ...
//------------------------------------------------------------------------------

function TRectD.GetWidth: double;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRectD.GetHeight: double;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRectD.GetIsEmpty: Boolean;
begin
  result := (bottom > top) and (right > left);
end;


//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

procedure RaiseError(const msg: string); {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipperLibException.Create(msg);
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPaths; sx, sy: double): TPaths;
var
  i,j,k,len: integer;
begin
  //strips duplicates
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    len := length(paths[i]);
    setlength(result[i], len);
    if len = 0 then Continue;
    result[i][0].X := Round(paths[i][0].X * sx);
    result[i][0].Y := Round(paths[i][0].Y * sy);
    k := 1;
    for j := 1 to len -1 do
    begin
      result[i][k].X := Round(paths[i][j].X * sx);
      result[i][k].Y := Round(paths[i][j].Y * sy);
      if (result[i][k].X <> result[i][k-1].X) or
        (result[i][k].Y <> result[i][k-1].Y) then inc(k);
    end;
    if (k > 1) and (result[i][k-1].X = result[i][0].X) and
        (result[i][k-1].Y = result[i][0].Y) then dec(k);
    SetLength(result[i], k);
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPaths; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * sx;
      result[i][j].Y := paths[i][j].Y * sy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * sx;
      result[i][j].Y := paths[i][j].Y * sy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function OffsetPaths(const paths: TPaths; dx, dy: Int64): TPaths;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X + dx;
      result[i][j].Y := paths[i][j].Y + dy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function OffsetPathsD(const paths: TPathsD; dx, dy: double): TPathsD;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X + dx;
      result[i][j].Y := paths[i][j].Y + dy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPath): TPath;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function ReversePaths(const paths: TPaths): TPaths;
var
  i, j, highJ: Integer;
begin
  i := length(paths);
  SetLength(Result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(paths[i]);
    SetLength(Result[i], highJ+1);
    for j := 0 to highJ do
      Result[i][j] := paths[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var paths: TPaths; const extra: TPaths);
var
  i, len1, len2: Integer;
begin
  len1 := length(paths);
  len2 := length(extra);
  SetLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1 + i] := extra[i];
end;
//------------------------------------------------------------------------------

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths;
var
  i,j,k, len, cnt: integer;
begin
  cnt := 0;
  len := length(ap);
  for i := 0 to len -1 do
    inc(cnt, length(ap[i]));
  k := 0;
  setlength(result, cnt);
  for i := 0 to len -1 do
    for j := 0 to length(ap[i]) -1 do
    begin
      result[k] := ap[i][j];
      inc(k);
    end;
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

function RectD(const left, top, right, bottom: double): TRectD;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Right := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
var
  i,j,k: Integer;
begin
  Result := Rect64(High(Int64), High(Int64), Low(Int64), Low(Int64));
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      for k := 0 to High(paths[i][j]) do
      begin
        if paths[i][j][k].X < Result.Left then Result.Left := paths[i][j][k].X;
        if paths[i][j][k].X > Result.Right then Result.Right := paths[i][j][k].X;
        if paths[i][j][k].Y < Result.Top then Result.Top := paths[i][j][k].Y;
        if paths[i][j][k].Y > Result.Bottom then Result.Bottom := paths[i][j][k].Y;
      end;
  if Result.Left > Result.Right then Result := nullRect;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPaths): TRect64;
var
  i,j: Integer;
begin
  Result := Rect64(High(Int64), High(Int64), Low(Int64), Low(Int64));
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
    begin
      if paths[i][j].X < Result.Left then Result.Left := paths[i][j].X;
      if paths[i][j].X > Result.Right then Result.Right := paths[i][j].X;
      if paths[i][j].Y < Result.Top then Result.Top := paths[i][j].Y;
      if paths[i][j].Y > Result.Bottom then Result.Bottom := paths[i][j].Y;
    end;
  if Result.Left > Result.Right then Result := nullRect;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPathsD): TRectD;
var
  i,j: Integer;
begin
  Result := RectD(MaxDouble, MaxDouble, -MaxDouble, -MaxDouble);
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
    begin
      if paths[i][j].X < Result.Left then Result.Left := paths[i][j].X;
      if paths[i][j].X > Result.Right then Result.Right := paths[i][j].X;
      if paths[i][j].Y < Result.Top then Result.Top := paths[i][j].Y;
      if paths[i][j].Y > Result.Bottom then Result.Bottom := paths[i][j].Y;
    end;
  result.Left := Floor(Result.Left);
  result.Top := Floor(Result.Top);
  result.Right := Ceil(Result.Left);
  result.Bottom := Ceil(Result.Top);
  if Result.Left > Result.Right then Result := nullRectD;
end;
//------------------------------------------------------------------------------

function InflateRect(const rec: TRect64; dx, dy: Int64): TRect64;
begin
  result := rec;
  dec(result.Left, dx);
  inc(result.Right, dx);
  dec(result.Top, dy);
  inc(result.Bottom, dy);
end;
//------------------------------------------------------------------------------

function UnionRects(const rec, rec2: TRect64): TRect64;
begin
  if rec.IsEmpty then result := rec2
  else if rec2.IsEmpty then result := rec
  else
  begin
    result.Left := min(rec.Left, rec2.Left);
    result.Right := max(rec.Right, rec2.Right);
    result.Top := min(rec.Top, rec2.Top);
    result.Bottom := max(rec.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function Area(const path: TPath): Double;
var
  i, j, highI: Integer;
  d: Double;
begin
  Result := 0.0;
  highI := High(path);
  if (highI < 2) then Exit;
  j := highI;
  for i := 0 to highI do
  begin
    d := (path[j].X + path[i].X);
    Result := Result + d * (path[j].Y - path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

function Orientation(const path: TPath): Boolean;
begin
  Result := Area(path) >= 0;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPoint64;
  const path: TPath): TPointInPolygonResult;
var
  i, val, cnt: Integer;
  d, d2, d3: Double; //using doubles to avoid possible integer overflow
  ip, ipNext: TPoint64;
begin
  cnt := Length(path);
  if cnt < 3 then
  begin
    result := pipOutside;
    Exit;
  end;
  ip := path[0];
  Result := pipOn;
  val := 0;
  for i := 1 to cnt do
  begin
    if i < cnt then ipNext := path[i]
    else ipNext := path[0];

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
    ip := ipNext;
  end;

  case val of
    -1: result := pipOn;
     1: result := pipInside;
     else result := pipOutside;
  end;
end;
//---------------------------------------------------------------------------

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
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.

