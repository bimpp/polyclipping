unit ClipperText;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  14 January 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module that converts Windows fonts into paths                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, ClipperCore;

type
  TGlyphMetricsArray = array of TGlyphMetrics;

function GetCharInfo(c: Char;
  trueTypeFont: HFont; out metrics: TGlyphMetrics): TPaths;
function GetCharInfos(s: string;
  trueTypeFont: HFont; out metrics: TGlyphMetricsArray): TArrayOfPaths;

function TextToPaths(const text: string;
  x, y: Int64; trueTypeFont: HFont; out gma: TGlyphMetricsArray): TPaths;
function TextToPathsD(const text: string;
  x, y: Int64; trueTypeFont: HFont; out gma: TGlyphMetricsArray): TPathsD;
function FixedToRect64(const rec: TRect64): TRect64;

implementation

const
  identity_mat2: TMat2 =
    (eM11:(fract: 0; value: 1);
     eM12:(fract: 0; value: 0);
     eM21:(fract: 0; value: 0);
     eM22:(fract: 0; value: 1));
  vert_flip_mat2: TMat2 =
    (eM11:(fract: 0; value: 1);
     eM12:(fract: 0; value: 0);
     eM21:(fract: 0; value: 0);
     eM22:(fract: 0; value: -1));

  GGO_BEZIER        = $3;
  GGO_UNHINTED      = $100;
  TT_PRIM_CSPLINE   = $3;
  cbezier_tolerance = $1000;
  qbezier_tolerance = $1000;
  buff_size         = 128;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function PointFxToPoint64(const fxpt: TPointFX; offset: TPoint64): TPoint64;
begin
  result.X := (Integer(fxpt.x) + offset.X);
  result.Y := (Integer(fxpt.y) + offset.Y);
end;
//------------------------------------------------------------------------------

function FixedToRect64(const rec: TRect64): TRect64;
begin
  result.Left := rec.Left div $10000;
  result.Top := rec.Top div $10000;
  result.Right := rec.Right div $10000 +1;
  result.Bottom := rec.Bottom div $10000 +1;
end;
//------------------------------------------------------------------------------

procedure AppendPathSkip1(var appendTo: TPath; const path: TPath);
var
  i, len, len2: integer;
begin
  len := length(appendTo);
  len2 := length(path);
  if len2 < 2 then exit;
  setLength(appendTo, len + len2 -1);
  for i := 1 to len2 -1 do appendTo[len+i-1] := path[i];
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var appendTo: TPaths; const paths: TPaths);
var
  i, len1, len2: integer;
begin
  len1 := length(appendTo);
  len2 := length(paths);
  setLength(appendTo, len1 + len2);
  for i := 0 to len2 -1 do
    appendTo[len1+i] := paths[i];
end;
//------------------------------------------------------------------------------

function GetQSplinePoints(const control_points: array of TPoint64): TPath;
var
  i, arrayLen, resultCnt, arrayLenMin3: integer;
  pts1, pts2, pts3: TPoint64;

  procedure RecursiveQSpline(const p1, p2, p3: TPoint64);
  var
    p12, p23, p123: TPoint64;
  begin
    //assess flatness of curve ...
    if (abs(p1.x + p3.x - 2 * p2.x) +
      abs(p1.y + p3.y - 2 * p2.y) < qbezier_tolerance) then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) + buff_size);
      result[resultCnt] := p3;
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) div 2;
      p12.Y := (p1.Y + p2.Y) div 2;
      p23.X := (p2.X + p3.X) div 2;
      p23.Y := (p2.Y + p3.Y) div 2;
      p123.X := (p12.X + p23.X) div 2;
      p123.Y := (p12.Y + p23.Y) div 2;
      RecursiveQSpline(p1, p12, p123);
      RecursiveQSpline(p123, p23, p3);
    end;
  end;

begin
  arrayLen := length(control_points);
  if (arrayLen < 3) then exit;
  arrayLenMin3 := arrayLen -3;
  setLength(result, buff_size);
  result[0] := control_points[0];
  resultCnt := 1;

  pts1 := control_points[0];
  for i := 0 to arrayLenMin3 do
  begin
    pts2 := control_points[i+1];
    if i < arrayLenMin3 then
    begin
      pts3 := Point64(
        (control_points[i+1].X + control_points[i+2].X) div 2,
        (control_points[i+1].Y + control_points[i+2].Y) div 2);
    end else
      pts3 := control_points[i+2];
    RecursiveQSpline(pts1, pts2, pts3);
    pts1 := pts3;
  end;
  SetLength(result, resultCnt);
end;
//------------------------------------------------------------------------------

function GetCBezierPoints(const control_points: array of TPoint64): TPath;
var
  i, j, arrayLen, resultCnt: integer;
  ctrlPts: array [ 0..3] of TPoint64;

  procedure RecursiveCBezier(const p1, p2, p3, p4: TPoint64);
  var
    p12, p23, p34, p123, p234, p1234: TPoint64;
  begin
    if (abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y)) <
        cbezier_tolerance then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) +buff_size);
      result[resultCnt] := p4;
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) div 2;
      p12.Y := (p1.Y + p2.Y) div 2;
      p23.X := (p2.X + p3.X) div 2;
      p23.Y := (p2.Y + p3.Y) div 2;
      p34.X := (p3.X + p4.X) div 2;
      p34.Y := (p3.Y + p4.Y) div 2;
      p123.X := (p12.X + p23.X) div 2;
      p123.Y := (p12.Y + p23.Y) div 2;
      p234.X := (p23.X + p34.X) div 2;
      p234.Y := (p23.Y + p34.Y) div 2;
      p1234.X := (p123.X + p234.X) div 2;
      p1234.Y := (p123.Y + p234.Y) div 2;
      RecursiveCBezier(p1, p12, p123, p1234);
      RecursiveCBezier(p1234, p234, p34, p4);
    end;
  end;

begin
  //first check that the 'control_points' count is valid ...
  arrayLen := length(control_points);
  if (arrayLen < 4) or ((arrayLen -1) mod 3 <> 0) then exit;

  setLength(result, buff_size);
  result[0] := control_points[0];
  resultCnt := 1;
  for i := 0 to (arrayLen div 3)-1 do
  begin
    for j := 0 to 3 do
      ctrlPts[j] := control_points[i*3 +j];
    RecursiveCBezier(ctrlPts[0], ctrlPts[1], ctrlPts[2], ctrlPts[3]);
  end;
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function ParseFontCharInfo(info: PByte; infoSize: cardinal;  offset: TPoint64): TPaths;
var
  tmpCurvePts: TPath;
  i, resultCntMin1, pathCnt, pathCurrCnt: integer;
  endInfo, endContour:  PByte;
begin
  resultCntMin1 := -1;
  endInfo := info + infoSize;
  while Info < endInfo do
  begin
    with PTTPolygonHeader(info)^ do
    begin
      if dwType <> TT_POLYGON_TYPE then Exit;
      endContour := info + cb;
      inc(info, SizeOf(TTTPolygonHeader));
      inc(resultCntMin1);
      setLength(Result, resultCntMin1 +1);
      setlength(Result[resultCntMin1],1);
      Result[resultCntMin1][0] := PointFxToPoint64(pfxStart, offset);
      pathCurrCnt := 1;
    end;
    while info < endContour do
    begin
      with PTTPolyCurve(info)^ do
      begin
        pathCnt := cpfx;
        inc(info, SizeOf(Word)*2);
        case wType of
          TT_PRIM_LINE:
            begin
              setlength(Result[resultCntMin1], pathCurrCnt + pathCnt);
              for i := 0 to pathCnt -1 do
              begin
                Result[resultCntMin1][pathCurrCnt+i] :=
                  PointFxToPoint64(PPointfx(info)^, offset);
                inc(info, sizeOf(TPointfx));
              end;
            end;
          TT_PRIM_QSPLINE:
            begin
              setLength(tmpCurvePts, pathCnt+1);
              tmpCurvePts[0] := Result[resultCntMin1][pathCurrCnt-1];
              for i := 1 to pathCnt do
              begin
                tmpCurvePts[i] := PointFxToPoint64(PPointfx(info)^, offset);
                inc(info, sizeOf(TPointfx));
              end;
              tmpCurvePts := GetQSplinePoints(tmpCurvePts);
              //nb: first point already added ...
              AppendPathSkip1(Result[resultCntMin1], tmpCurvePts);
              pathCnt := length(tmpCurvePts)-1;
            end;
          TT_PRIM_CSPLINE:
            begin
              //tmpCurvePts is used for the CSPLINE control points ...
              setLength(tmpCurvePts, pathCnt+1);
              //must include the previous point in the CSPLINE ...
              tmpCurvePts[0] := Result[resultCntMin1][pathCurrCnt-1];
              for i := 1 to pathCnt do
              begin
                tmpCurvePts[i] := PointFxToPoint64(PPointfx(info)^, offset);
                inc(info, sizeOf(TPointfx));
              end;
              tmpCurvePts := GetCBezierPoints(tmpCurvePts);
              //nb: first point already added ...
              AppendPathSkip1(Result[resultCntMin1], tmpCurvePts);
              pathCnt := length(tmpCurvePts)-1;
            end;
          else Exit;
        end;
        inc(pathCurrCnt, pathCnt);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetCharInfoInternal(memDC: HDC; c: Char;
  out metrics: TGlyphMetrics; offset: TPoint64): TPaths;
var
  size: DWord;
  info, startInfo:  PByte;
begin
  result := nil;
  startInfo := nil;
  size := GetGlyphOutline(memDC, cardinal(c),
    GGO_NATIVE or GGO_UNHINTED, metrics, 0, nil, vert_flip_mat2);
  if (size = GDI_ERROR) or (size = 0) then exit;
  GetMem(info, size);
  try
    startInfo := info;
    if GetGlyphOutline(memDC, cardinal(c), GGO_NATIVE or GGO_UNHINTED,
      metrics, size, info, vert_flip_mat2) <> GDI_ERROR then
        Result := ParseFontCharInfo(info, size, offset);
  finally
    FreeMem(startInfo);
  end;
end;
//------------------------------------------------------------------------------

function TextToPaths(const text: string;
  x, y: Int64; trueTypeFont: HFont;
  out gma: TGlyphMetricsArray): TPaths;
var
  i, len: integer;
  gm: TGlyphMetrics;
  memDC: HDC;
  oldFont: HFont;
  tmp: TPaths;
  offset: TPoint64;
const
  fixed_mul = $10000;
begin
  result := nil;
  len := length(text);
  if (len = 0) or (trueTypeFont = 0) then exit;
  SetLength(gma, len);
  offset := Point64(x, y);
  memDC := CreateCompatibleDC(0);
  if memDC = 0 then exit;
  oldFont := windows.SelectObject(memDC, trueTypeFont);
  try
    for i := 1 to len do
    begin
      tmp := GetCharInfoInternal(memDC, text[i], gm, offset);
      gma[i-1] := gm;
      AppendPaths(result, tmp);
      inc(offset.X, gm.gmCellIncX * fixed_mul);
      dec(offset.Y, gm.gmCellIncY * fixed_mul);
    end;
  finally
    windows.SelectObject(memDC, oldFont);
    DeleteDC(memDC);
  end;
end;
//------------------------------------------------------------------------------

function TextToPathsD(const text: string;
  x, y: Int64; trueTypeFont: HFont;
  out gma: TGlyphMetricsArray): TPathsD;
var
  i, j, len, len2: integer;
  gm: TGlyphMetrics;
  memDC: HDC;
  oldFont: HFont;
  tmpResult: TPaths;
  tmp: TPaths;
  offset: TPoint64;
const
  fixed_mul = $10000;
  div_Fixed = 1/fixed_mul;
begin
  len := length(text);
  if (len = 0) or (trueTypeFont = 0) then exit;
  SetLength(gma, len);
  offset := Point64(x, y);
  memDC := CreateCompatibleDC(0);
  if memDC = 0 then exit;
  oldFont := windows.SelectObject(memDC, trueTypeFont);
  try
    for i := 1 to len do
    begin
      tmp := GetCharInfoInternal(memDC, text[i], gm, offset);
      gma[i-1] := gm;
      AppendPaths(tmpResult, tmp);
      inc(offset.X, gm.gmCellIncX * fixed_mul);
      inc(offset.Y, gm.gmCellIncY * fixed_mul);
    end;
  finally
    windows.SelectObject(memDC, oldFont);
    DeleteDC(memDC);
  end;

  len := length(tmpResult);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    len2 := length(tmpResult[i]);
    setLength(result[i], len2);
    for j := 0 to len -1 do
    begin
      result[i][j].X := tmpResult[i][j].X * div_Fixed;
      result[i][j].Y := tmpResult[i][j].Y * div_Fixed;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetCharInfo(c: Char;
  trueTypeFont: HFont; out metrics: TGlyphMetrics): TPaths;
var
  memDC: HDC;
  oldFont: HFont;
const
  offset: TPoint64 = (X: 0; Y:0);
begin
  result := nil;
  if (trueTypeFont = 0) then exit;
  memDC := CreateCompatibleDC(0);
  if memDC = 0 then exit;
  oldFont := windows.SelectObject(memDC, trueTypeFont);
  try
    result := GetCharInfoInternal(memDC, c, metrics, offset);
  finally
    windows.SelectObject(memDC, oldFont);
    DeleteDC(memDC);
  end;
end;
//------------------------------------------------------------------------------

function GetCharInfos(s: string;
  trueTypeFont: HFont; out metrics: TGlyphMetricsArray): TArrayOfPaths;
var
  i, len: integer;
  memDC: HDC;
  oldFont: HFont;
const
  offset: TPoint64 = (X: 0; Y:0);
begin
  result := nil;
  if (trueTypeFont = 0) then exit;
  len := length(s);
  setLength(result, len);
  setLength(metrics, len);
  memDC := CreateCompatibleDC(0);
  if memDC = 0 then exit;
  oldFont := windows.SelectObject(memDC, trueTypeFont);
  try
    for i := 0 to length(s) -1 do
      result[i] := GetCharInfoInternal(memDC, s[i+1], metrics[i], offset);
  finally
    windows.SelectObject(memDC, oldFont);
    DeleteDC(memDC);
  end;
end;
//------------------------------------------------------------------------------

end.

