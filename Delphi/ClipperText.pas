unit ClipperText;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  23 February 2019                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module that converts Windows fonts into paths                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, ClipperCore, Image32;

type
  TGlyphMetricsArray = array of TGlyphMetrics;

//CreateCharImage: gets a Windows generated 'hinted' glyph image
function CreateCharImage(memDC: HDC; c: Char;
  out metrics: TGlyphMetrics): TImage32;
//GetCharPaths: gets the glyph's raw vectors
function GetCharPaths(memDC: HDC; c: Char;
  hinted: Boolean; out metrics: TGlyphMetrics): TPaths;

function FixedToRect64(const rec: TRect64): TRect64;
function FixedToRect(const rec: TRect64): TRect;

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
  CBezierTolerance  = $1000;
  QBezierTolerance  = $1000;
  BuffSize          = 128;
  FixedMul          = $10000;
  DivFixed: double  = 1/FixedMul;
  piDiv1800: double = pi/1800;

var
  scale64To255: array[0..64] of byte;
  GgoHinted: array [boolean] of Cardinal = (GGO_UNHINTED, $0);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function PointFxToPoint64(const fxpt: TPointFX): TPoint64;
begin
  result.X := Integer(fxpt.x);
  result.Y := Integer(fxpt.y);
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

function FixedToRect(const rec: TRect64): TRect;
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
  Move(Path[1], appendTo[len], (len2 -1) * SizeOf(TPoint64));
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
      abs(p1.y + p3.y - 2 * p2.y) < QBezierTolerance) then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) + BuffSize);
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
  setLength(result, BuffSize);
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
        CBezierTolerance then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) +BuffSize);
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

  setLength(result, BuffSize);
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

{$IFNDEF UNICODE} type PByte = PChar; {$ENDIF}

function ParseFontCharInfo(info: PByte; infoSize: cardinal): TPaths;
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
      Result[resultCntMin1][0] := PointFxToPoint64(pfxStart);
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
                  PointFxToPoint64(PPointfx(info)^);
                inc(info, sizeOf(TPointfx));
              end;
            end;
          TT_PRIM_QSPLINE:
            begin
              setLength(tmpCurvePts, pathCnt+1);
              tmpCurvePts[0] := Result[resultCntMin1][pathCurrCnt-1];
              for i := 1 to pathCnt do
              begin
                tmpCurvePts[i] := PointFxToPoint64(PPointfx(info)^);
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
                tmpCurvePts[i] := PointFxToPoint64(PPointfx(info)^);
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

function GetCharPaths(memDC: HDC; c: Char;
  hinted: Boolean; out metrics: TGlyphMetrics): TPaths;
var
  size: DWord;
  info, startInfo:  PByte;
//  rec: TRect;
begin
  size := GetGlyphOutline(memDC, cardinal(c),
    GGO_NATIVE or GgoHinted[hinted], metrics, 0, nil, vert_flip_mat2);
  if (size = GDI_ERROR) or (size = 0) then
  begin
    if (size = GDI_ERROR) then
      FillChar(metrics, SizeOf(metrics), 0);
    result := nil;
    exit;
  end;

  startInfo := nil;
  GetMem(info, size);
  try
    startInfo := info;
    if GetGlyphOutline(memDC, cardinal(c), GGO_NATIVE or GgoHinted[hinted],
      metrics, size, info, vert_flip_mat2) <> GDI_ERROR then
        Result := ParseFontCharInfo(info, size);
//    //and possibly some very minor adjustments to metrics ...
//    rec := FixedToRect(GetBounds(Result));
//    metrics.gmptGlyphOrigin.X := rec.left;
//    metrics.gmptGlyphOrigin.Y := rec.Bottom;
//    metrics.gmBlackBoxX := rec.Width;
//    metrics.gmBlackBoxY := rec.Height;
  finally
    FreeMem(startInfo);
  end;
end;
//------------------------------------------------------------------------------

function CreateCharImage(memDC: HDC; c: Char;
  out metrics: TGlyphMetrics): TImage32;
var
  i, h,w: integer;
  size: DWord;
  bytes: TBytes;
  pb: PByte;
  pc: PARGB;
begin
  Result := nil;
  size := GetGlyphOutline(memDC, cardinal(c),
    GGO_GRAY8_BITMAP, metrics, 0, nil, vert_flip_mat2);
  if (size = GDI_ERROR) or (size = 0) then
  begin
    if (size = GDI_ERROR) then FillChar(metrics, SizeOf(metrics), 0);
    Exit;
  end;

  Result := TImage32.Create(metrics.gmBlackBoxX, metrics.gmBlackBoxY);
  SetLength(bytes, size);
  GetGlyphOutline(memDC, cardinal(c), GGO_GRAY8_BITMAP,
    metrics, size, @bytes[0], vert_flip_mat2);

  i := metrics.gmBlackBoxX mod 4;
  if i > 0 then i := 4 - i;
  pb := @bytes[0];
  pc := PARGB(Result.PixelBase);
  for h := 0 to metrics.gmBlackBoxY -1 do
  begin
    for w := 0 to metrics.gmBlackBoxX -1 do
    begin
      pc.A := scale64To255[pb^];
      inc(pb); inc(pc);
    end;
    for w := 1 to i do inc(pb); //dword aligned
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MakeScaleTable;
var
  i: integer;
begin
  for i := 0 to 64 do
    scale64To255[i] := Round(i * 255 / 64)
end;
//------------------------------------------------------------------------------

initialization
  MakeScaleTable;

end.

