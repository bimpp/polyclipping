unit Image32;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  7 February 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module to maniputlate 32bit images                              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}

interface

uses
  SysUtils, Classes, Windows, Math;

type
  PColor32 = ^TColor32;
  TColor32 = type Cardinal;
  TArrayOfColor32 = array of TColor32;

  TImage32ExtClass = class of TImage32Ext;
  TImage32 = class;

  //TImage32Ext: An abstract base class that templates image file access.
  //For BMP files see the accompanying Image32_BMP unit.
  TImage32Ext = class
    class function SaveToFile(const filename: string;
      img32: TImage32): Boolean; overload; virtual; abstract;
    class function LoadFromFile(const filename: string;
      img32: TImage32): Boolean; virtual; abstract;
  end;

  TBlendFunction = function(bgColor, fgColor: TColor32): TColor32;
  TCompareFunction = function(color, fillColor: TColor32): Boolean;

  TImage32 = class
  private
    fPixels: TArrayOfColor32;
    fWidth: integer;
    fHeight: integer;
    fAntiAliase: Boolean;
    function GetPixel(x,y: integer): TColor32;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure SetPixel(x,y: integer; color: TColor32);
    function GetWeightedPixel(x256, y256: integer): TColor32;
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetPixelBase: PColor32; {$IFDEF INLINING} inline; {$ENDIF}
    procedure DoResizeAA(newWidth, newHeight: integer);
    procedure DoResize(newWidth, newHeight: integer);
    function GetColorCount: integer;
    function GetAlphaIsUsed: Boolean;
    function GetBounds: TRect; {$IFDEF INLINING} inline; {$ENDIF}
    class function IsValidFileExt(var ext: string): Boolean;
  protected
    //CheckRect: makes sure the returned rec is within image bounds
    function CheckRect(var rec: TRect): Boolean;
    //InvertRect: needed because images are always stored bottom up
    procedure InvertRect(var rec: TRect);
    function CopyPixels(rec: TRect): TArrayOfColor32;
    //CopyInternal: internal because 'recs' are inverted & no bounds checking
    procedure CopyInternal(src: TImage32;
      const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
    procedure DeMultipleAlpha(rec: TRect);
    procedure PreMultipleAlpha(rec: TRect);
  public
    constructor Create(width: integer = 0; height: integer = 0); overload;
    constructor Create(src: TImage32); overload;
    constructor Create(src: TImage32; const srcRec: TRect); overload;
    destructor Destroy; override;

    procedure Assign(src: TImage32);
    procedure AssignTo(dst: TImage32);
    procedure SetSize(newWidth, newHeight: integer); overload;
    procedure SetSize(newWidth, newHeight: integer; colorFill: TColor32); overload;
    procedure Resize(newWidth, newHeight: integer; stretchImage: Boolean = true);
    procedure Scale(sx, sy: single);

    function CopyFrom(src: TImage32;
      srcRec, dstRec: TRect; blendFunc: TBlendFunction = nil): Boolean;

    procedure Crop(const rec: TRect);
    procedure CropTransparentPixels;
    procedure Rotate(angleRads: single);
    procedure FlipVertical;
    procedure FlipHorizontal;
    procedure Fill(color: TColor32); overload;
    procedure Fill(rec: TRect; color: TColor32); overload;
    procedure Grayscale;
    procedure InvertColors;
    procedure ColorToAlpha(color: TColor32);
    procedure AdjustHue(percent: integer);         //ie +/- 100%
    procedure AdjustLuminance(percent: integer);   //ie +/- 100%
    procedure AdjustSaturation(percent: integer);  //ie +/- 100%
    //BoxBlur: a very fast box blur (and much faster than GaussianBlur)
    procedure BoxBlur(radius, repeats: integer);
    procedure GaussianBlur(radius: integer);
    procedure Emboss(radius: integer = 1; depth: integer = 10;
      luminance: integer = 75; preserveColor: Boolean = false);
    procedure Shadow(radius: integer; dx, dy: integer; color: TColor32);
    procedure Glow(radius: integer; color: TColor32);
    //Sharpen: radius range is 1 - 10; amount range is 0.05 - 5.0.
    procedure Sharpen(radius: integer = 2; amount: single = 0.5);
    procedure FloodFill(x,y: integer;
      newColor: TColor32; compareFunc: TCompareFunction = nil);

    class procedure RegisterExtension(ext: string;
      bm32ExClass: TImage32ExtClass);
    class function GetImage32ExtClass(ext: string): TImage32ExtClass;

    //SaveToFile: requires a registered TImage32Ext
    function SaveToFile(const filename: string): Boolean; overload;
    //LoadFromFile: requires a registered TImage32Ext
    function LoadFromFile(const filename: string): Boolean;

    //properties ...
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    property Bounds: TRect read GetBounds;
    property IsEmpty: Boolean read GetIsEmpty;
    property Pixel[x,y: integer]: TColor32 read GetPixel write SetPixel;
    property Pixels: TArrayOfColor32 read fPixels;
    property PixelBase: PColor32 read GetPixelBase;
    property ColorCount: integer read GetColorCount;
    property UsesAlphaChannel: Boolean read GetAlphaIsUsed;
    //AntiAliased: antialiasing is used when scaling and rotating
    property AntiAliased: Boolean read fAntiAliase write fAntiAliase;
  end;

  PARGB = ^TARGB;
  TARGB = packed record
    case boolean of
      false: (B: Byte; G: Byte; R: Byte; A: Byte);
      true : (Color: TColor32);
  end;

  THsl = packed record
    hue  : byte;
    sat  : byte;
    lum  : byte;
    alpha: byte;
  end;

  //BLENDING FUNCTIONS ( used with TImage32.CopyFrom() ) ...

  //BlendToOpaque: blends a semi-transparent image onto an opaque background
  function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
  //BlendToAlpha: blends two semi-transparent images (slower than above)
  function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
  //BlendAverage: averages color channels using alpha weighting
  function BlendAverage(bgColor, fgColor: TColor32): TColor32;
  //BlendAlphas: multiplies (reduces) background alpha using foreground alpha
  function BlendAlphas(bgColor, fgColor: TColor32): TColor32;

  //MISCELLANEOUS FUNCTIONS ...

  function RgbtoHsl(color: TColor32): THsl;
  function HslToRgb(hslColor: THsl): TColor32;

const
  clAqua32     = TColor32($FF00FFFF);
  clBlack32    = TColor32($FF000000);
  clBlue32     = TColor32($FF0000FF);
  clFuchsia32  = TColor32($FFFF00FF);
  clGray32     = TColor32($FF808080);
  clGreen32    = TColor32($FF008000);
  clLime32     = TColor32($FF00FF00);
  clMaroon32   = TColor32($FF000080);
  clNavy32     = TColor32($FF000080);
  clNone32     = TColor32($00000000);
  clRed32      = TColor32($FFFF0000);
  clSilver32   = TColor32($FFC0C0C0);
  clWhite32    = TColor32($FFFFFFFF);
  clYellow32   = TColor32($FFFFFF00);

var
  //tables for blend functions
  MulTable: array [Byte,Byte] of Byte; //MulTable[a,b] = a * b / 255
  DivTable: array [Byte,Byte] of Byte; //DivTable[a,b] = a * 255/b (for a <= b)

implementation

var
  Image32ExtClassList: TStringList; //list of supported file extensions

const
  div255 : Double = 1 / 255;
  div6   : Double = 1 / 6;
  MaxBlur = 20;

type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;

  TColor24 = packed record R,G,B: byte; end;
  TColor24Array = array of TColor24;
  TArrayofHSL = array of THsl;

  PWeightedColor = ^TWeightedColor;
  TWeightedColor = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF}
  private
    fAddCount : integer;
    fAlphaTot : integer;
    fColorTotR: integer;
    fColorTotG: integer;
    fColorTotB: integer;
    function GetColor: TColor32;
  public
    procedure Reset;
    procedure Add(c: TColor32; weight: integer);
    procedure Subtract(c: TColor32; weight: integer);
    procedure AddWeight(weight: integer);
    property AddCount: integer read fAddCount;
    property Color: TColor32 read GetColor;
  end;

  TPointD = record X, Y: double; end;
  TPathD = array of TPointD;
  TRectD = record Left, Top, Right, Bottom: double; end;

  TArrayOfInteger = array of Integer;
  TArrayOfByte = array of Byte;
  TArrayOfWeightedColor = array of TWeightedColor;

  PWeightedColorArray = ^TWeightedColorArray;
  TWeightedColorArray = array [0.. $FFFFFF] of TWeightedColor;

  //FloodFill structures ...

  PFloodFillRec = ^TFloodFillRec;
  TFloodFillRec = record
    xLeft     : integer;
    xRight    : integer;
    y         : integer;
    direction : integer;
    next      : PFloodFillRec;
  end;

  TFloodFillStack = class
    first     : PFloodFillRec;
    maxY      : integer;
    constructor Create(maxY: integer);
    destructor Destroy; override;
    procedure Push(xLeft, xRight,y, direction: integer);
    procedure Pop(out xLeft, xRight,y, direction: integer);
    function IsEmpty: Boolean;
  end;

  TFloodFillMask = {$IFDEF UNICODE} record {$ELSE} object {$ENDIF}
    mask         : TArrayOfByte;
    width        : integer;
    height       : integer;
    compareColor : TColor32;
    colorsBase   : PColor32Array;
    colorsRow    : PColor32Array;
    maskRow      : PByteArray;
    compareFunc  : TCompareFunction;
    procedure Reset(Width, Height, x, y: integer;
      pixelBase: PColor32; compareFunc: TCompareFunction);
    procedure SetCurrentY(y: integer);
    function IsMatch(x: integer): Boolean;
  end;

//------------------------------------------------------------------------------
// Blend functions - used by TImage32.CopyFrom()
//------------------------------------------------------------------------------

function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  fw,bw: PByteArray;
begin
  if fg.A = 0 then Exit(bgColor)
  else if fg.A = 255 then Exit(fgColor);
  //uses only the foreground alpha for color weighting ...
  res.A := 255;
  fw := @MulTable[fg.A];         //ie fw == multiples of fg.A
  bw := @MulTable[not fg.A];     //ie bw == multiples of (255 - fg.A)
  res.R := fw[fg.R] + bw[bg.R];
  res.G := fw[fg.G] + bw[bg.G];  //ie res.G == fg.A*fg.G + (255-fg.A) *bg.G
  res.B := fw[fg.B] + bw[bg.B];
end;
//------------------------------------------------------------------------------

function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  r: byte;
  arrayR,arrayInvR: PByteArray;
begin
  //(see https://en.wikipedia.org/wiki/Alpha_compositing)
  if fg.A < 3 then Exit(bgColor)
  else if fg.A = 255 then Exit(fgColor);
  //merge alphas ...
  res.A := MulTable[fg.A xor 255, bg.A xor 255] xor 255;
  //r = the ratio of the foreground alpha to the result alpha
  //  = fg.A * 255 / res.A (where fg.A is always less than or equal res.A )
  r := DivTable[fg.A, res.A];
  arrayR    := @MulTable[r];               //ie an array of r multiples
  arrayInvR := @MulTable[not r];           //ie an array of (255-r) multiples
  res.R := arrayR[fg.R] + arrayInvR[bg.R];
  res.G := arrayR[fg.G] + arrayInvR[bg.G]; //res.G := r * fg.G + (255-r) * bg.G
  res.B := arrayR[fg.B] + arrayInvR[bg.B];
end;
//------------------------------------------------------------------------------

function BlendAverage(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  twoA: cardinal;
begin
  res.A := MulTable[fg.A xor 255, bg.A xor 255] xor 255;
  if res.A = 0 then Exit;
  twoA := bg.A + fg.A;
  res.R := ((bg.R * bg.A) + (fg.R * fg.A)) div twoA;
  res.G := ((bg.G * bg.A) + (fg.G * fg.A)) div twoA;
  res.B := ((bg.B * bg.A) + (fg.B * fg.A)) div twoA;
end;

//------------------------------------------------------------------------------

function BlendAlphas(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  Result := bgColor;
  res.A := MulTable[bg.A, fg.A];
  if res.A = 0 then Result := 0;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function IsEqualRecSizes(const rec1, rec2: TRect): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (rec1.Width = rec2.Width) and (rec1.Height = rec2.Height);
end;
//------------------------------------------------------------------------------

function ClampByte(val: integer): byte; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if val < 0 then result := 0
  else if val > 255 then result := 255
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: integer): integer; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: single): single; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

function IncPColor32(pc: PColor32; cnt: integer): PColor32;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := PColor32(PByte(pc) + cnt * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

function DivRound(num, denom: Cardinal): Cardinal;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (num  + denom div 2) div denom;
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

procedure RotatePt(var pt: TPointD; const origin: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-origin.X;
  tmpY := pt.Y-origin.Y;
  pt.X := tmpX * cosA - tmpY * sinA + origin.X;
  pt.Y := tmpX * sinA + tmpY * cosA + origin.Y;
end;
//------------------------------------------------------------------------------

function GetRotatedRectD(const rec: TRectD; angleRad: double): TRectD;
var
  i: integer;
  sinA, cosA: double;
  cp: TPointD;
  pts: TPathD;
begin
  setLength(pts, 4);
  sinA := Sin(-angleRad);
  cosA := cos(-angleRad);
  cp.X := (rec.Right + rec.Left) / 2;
  cp.Y := (rec.Bottom + rec.Top) / 2;
  pts[0] := PointD(rec.Left, rec.Top);
  pts[1] := PointD(rec.Right, rec.Top);
  pts[2] := PointD(rec.Left, rec.Bottom);
  pts[3] := PointD(rec.Right, rec.Bottom);
  for i := 0 to 3 do RotatePt(pts[i], cp, sinA, cosA);
  result.Left := pts[0].X;
  result.Right := result.Left;
  result.Top := pts[0].Y;
  result.Bottom := result.Top;
  for i := 1 to 3 do
  begin
    if pts[i].X < result.Left then result.Left := pts[i].X;
    if pts[i].Y < result.Top then result.Top := pts[i].Y;
    if pts[i].X > result.Right then result.Right := pts[i].X;
    if pts[i].Y > result.Bottom then result.Bottom := pts[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function GetWeightedColor(const srcBits: TArrayOfColor32;
  x256, y256, xx256, yy256, maxX: integer): TColor32;
var
  i, j, xi, yi, xxi, yyi, weight: integer;
  xf, yf, xxf, yyf: cardinal;
  color: TWeightedColor;
begin
  color.Reset;

  xi := x256 shr 8; xf := x256 and $FF;
  yi := y256 shr 8; yf := y256 and $FF;
  xxi := xx256 shr 8; xxf := xx256 and $FF;
  yyi := yy256 shr 8; yyf := yy256 and $FF;

  //1. average the corners ...
  weight := (($100 - xf) * ($100 - yf)) shr 8;
  color.Add(srcBits[xi + yi * maxX], weight);
  weight := (xxf * ($100 - yf)) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yi * maxX], weight);
  weight := (($100 - xf) * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xi + yyi * maxX], weight);
  weight := (xxf * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yyi * maxX], weight);

  //2. average the edges
  if (yi +1 < yyi) then
  begin
    xf := $100 - xf;
    for i := yi + 1 to yyi - 1 do
      color.Add(srcBits[xi + i * maxX], xf);
    if (xxf <> 0) then
      for i := yi + 1 to yyi - 1 do
        color.Add(srcBits[xxi + i * maxX], xxf);
  end;
  if (xi + 1 < xxi) then
  begin
    yf := $100 - yf;
    for i := xi + 1 to xxi - 1 do
      color.Add(srcBits[i + yi * maxX], yf);
    if (yyf <> 0) then
      for i := xi + 1 to xxi - 1 do
        color.Add(srcBits[i + yyi * maxX], yyf);
  end;

  //3. average the non-fractional pixel 'internals' ...
  for i := xi + 1 to xxi - 1 do
    for j := yi + 1 to yyi - 1 do
      color.Add(srcBits[i + j * maxX], $100);

  if color.AddCount = 0 then
    Result := srcBits[xi + yi * maxX] else
    Result := color.Color;
end;
//------------------------------------------------------------------------------

function RgbtoHsl(color: TColor32): THsl;
var
  rgba: TARGB absolute color;
  hsl: THsl absolute result;
  r,g,b: byte;
  maxRGB, minRGB, mAdd, mSub: integer;
begin
  //https://en.wikipedia.org/wiki/HSL_and_HSV and
  //http://en.wikipedia.org/wiki/HSL_color_space
  r := rgba.R; g := rgba.G; b := rgba.B;
  maxRGB := Max(r, Max(g, b));
  minRGB := Min(r, Min(g, b));
  mAdd := maxRGB + minRGB;
  hsl.lum := mAdd shr 1;
  hsl.alpha := rgba.A;

  if maxRGB = minRGB then
  begin
    hsl.hue := 0; //hsl.hue is undefined when gray
    hsl.sat := 0;
    Exit;
  end;

  mSub := maxRGB - minRGB;
  if mAdd <= 255 then
    hsl.sat := DivTable[mSub, mAdd] else
    hsl.sat := DivTable[mSub, 511 - mAdd];

  mSub := mSub * 6;
  if r = maxRGB then
  begin
    if g >= b then
      hsl.hue := (g - b) * 255 div mSub else
      hsl.hue := 255 - ((b - g) * 255 div mSub);
  end
  else if G = maxRGB then
  begin
    if b > r then
      hsl.hue := 85 + (b - r) * 255 div mSub else
      hsl.hue := 85 - (r - b)  * 255 div mSub;
  end else
  begin
    if r > g then
      hsl.hue := 170 + (r - g)  * 255 div mSub else
      hsl.hue := 170 - (g - r)  * 255 div mSub;
  end;
end;
//------------------------------------------------------------------------------

function HslToRgb(hslColor: THsl): TColor32;
var
  rgba: TARGB absolute result;
  hsl: THsl absolute hslColor;
  c,x,m: integer;
begin
  //formula from https://www.rapidtables.com/convert/color/hsl-to-rgb.html
  c := (255 - abs(2 * hsl.lum - 255)) * hsl.sat div 255;
  x := c * (255 - abs((hsl.hue mod 85) * 6 - 255)) div 255;
  m := hsl.lum - c div 2;
  rgba.A := hsl.alpha;
  case (hsl.hue * 6) shr 8 of
    0:
      begin
        rgba.R := c + m;
        rgba.G := x + m;
        rgba.B := 0 + m;
      end;
    1:
      begin
        rgba.R := x + m;
        rgba.G := c + m;
        rgba.B := 0 + m;
      end;
    2:
      begin
        rgba.R := 0 + m;
        rgba.G := c + m;
        rgba.B := x + m;
      end;
    3:
      begin
        rgba.R := 0 + m;
        rgba.G := x + m;
        rgba.B := c + m;
      end;
    4:
      begin
        rgba.R := x + m;
        rgba.G := 0 + m;
        rgba.B := c + m;
      end;
    5:
      begin
        rgba.R := c + m;
        rgba.G := 0 + m;
        rgba.B := x + m;
      end;
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfColor32ToArrayHSL(const clr32Arr: TArrayOfColor32): TArrayofHSL;
var
  i, len: integer;
begin
  len := length(clr32Arr);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := RgbtoHsl(clr32Arr[i]);
end;
//------------------------------------------------------------------------------

function ArrayOfHSLToArrayColor32(const hslArr: TArrayofHSL): TArrayOfColor32;
var
  i, len: integer;
begin
  len := length(hslArr);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := HslToRgb(hslArr[i]);
end;
//------------------------------------------------------------------------------

function Intensity(color: TColor32): byte; {$IFDEF INLINING} inline; {$ENDIF}
var
  c: TARGB absolute color;
begin
  Result := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
end;
//------------------------------------------------------------------------------

function Gray(color: TColor32): TColor32; {$IFDEF INLINING} inline; {$ENDIF}
var
  c: TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.A := c.A;
  res.R := Intensity(color);
  res.G := res.R;
  res.B := res.R;
end;

//------------------------------------------------------------------------------
// TImage32 methods
//------------------------------------------------------------------------------

constructor TImage32.Create(width: integer = 0; height: integer = 0);
begin
  fAntiAliase := true;
  SetSize(width, height); //pixels undefined
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32);
begin
  Assign(src);
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32; const srcRec: TRect);
begin
  fAntiAliase := src.fAntiAliase;
  SetSize(srcRec.Width, srcRec.Height);
  if not srcRec.IsEmpty then
    fPixels := src.CopyPixels(srcRec);
end;
//------------------------------------------------------------------------------

destructor TImage32.Destroy;
begin
  fPixels := nil;
  inherited;
end;
//------------------------------------------------------------------------------

class function TImage32.IsValidFileExt(var ext: string): Boolean;
begin
  result := false;
  ext := lowercase(ext);
  if (ext = '') or (ext = '.') then Exit //invalid
  else if (ext[1] = '.') then Delete(ext, 1,1);
  result := CharInSet(ext[1], ['a'..'z']);
end;
//------------------------------------------------------------------------------

class procedure TImage32.RegisterExtension(ext: string;
  bm32ExClass: TImage32ExtClass);
var
  idx: integer;
begin
  if IsValidFileExt(ext) and not Image32ExtClassList.Find(ext, idx) then
    Image32ExtClassList.AddObject(ext, Pointer(bm32ExClass));
end;
//------------------------------------------------------------------------------

class function TImage32.GetImage32ExtClass(ext: string): TImage32ExtClass;
var
  idx: integer;
begin
  if IsValidFileExt(ext) and Image32ExtClassList.Find(ext, idx) then
    Result := TImage32ExtClass(Image32ExtClassList.objects[idx]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TImage32.Assign(src: TImage32);
begin
  src.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TImage32.AssignTo(dst: TImage32);
begin
  dst.fAntiAliase := fAntiAliase;
  dst.SetSize(Width, Height);
  if (Width > 0) and (Height > 0) then
    move(fPixels[0], dst.fPixels[0], Width * Height * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

procedure TImage32.Fill(color: TColor32);
var
  i: integer;
begin
  if IsEmpty then Exit;
  if color = clNone32 then
    FillChar(fPixels[0], Width * Height * SizeOf(TColor32), 0)
  else
    for i := Width * Height -1 downto 0 do fPixels[i] := color;
end;
//------------------------------------------------------------------------------

procedure TImage32.Fill(rec: TRect; color: TColor32);
var
  i,j, recWidth: integer;
  c: PColor32;
begin
  if not CheckRect(rec) then Exit;
  recWidth := rec.Width;
  InvertRect(rec);
  c := @Pixels[rec.Top * Width + rec.Left];
  for i := rec.Top to rec.Bottom -1 do
  begin
    for j := 1 to recWidth do
    begin
      c^ := color;
      inc(c);
    end;
    inc(c, Width - recWidth);
  end;
end;
//------------------------------------------------------------------------------

function TImage32.CopyPixels(rec: TRect): TArrayOfColor32;
var
  i, clipW, w,h: integer;
  pSrc, pDst, pDst2: PColor32;
  recClipped: TRect;
begin
  w := rec.Width;
  h := rec.Height;
  setLength(result, w * h);

  if w * h = 0 then Exit;
  InvertRect(rec);
  Windows.IntersectRect(recClipped, rec, Bounds);

  if recClipped.IsEmpty then
  begin
    //rec is considered valid even when completely outside the image bounds,
    //and so when that happens we simply return a fully transparent image ...
    FillChar(Result[0], w * h * SizeOf(TColor32), 0);
    Exit;
  end;

  pDst := @Result[0];
  for i := rec.Top to -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
  pSrc := @fPixels[recClipped.Top * Width + Max(0,rec.Left)];
  if (rec.Left < 0) or (rec.Right > Width) then
  begin
    clipW := recClipped.Width;
    pDst2 := IncPColor32(pDst, -Min(0, rec.Left));
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      //when rec.left < 0 or rec.right > width it's simplest to
      //start with a prefilled row of transparent pixels
      FillChar(pDst^, w * SizeOf(TColor32), 0);
      Move(pSrc^, pDst2^, clipW * SizeOf(TColor32));
      inc(pDst, w); inc(pDst2, w); inc(pSrc, Width);
    end;
  end else
  begin
    //things are a bit simpler when there's no part of rec is
    //outside the image, at least not on the left or right sides ...
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, Width); inc(pDst, w);
    end;
  end;
  for i := Height to rec.Bottom -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Crop(const rec: TRect);
var
  newPixels: TArrayOfColor32;
begin
  newPixels := CopyPixels(rec);
  SetSize(rec.Width, rec.Height);
  if not rec.IsEmpty then fPixels := newPixels;
end;
//------------------------------------------------------------------------------

procedure TImage32.CropTransparentPixels;
var
  x,y, x1,x2,y1,y2: integer;
  rec: TRect;
  found: Boolean;
begin
  y1 := 0; y2 := 0;
  found := false;
  for y := 0 to Height -1 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y1 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  if not found then
  begin
    SetSize(0, 0);
    Exit;
  end;

  found := false;
  for y := Height -1 downto 0 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y2 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  x1 := Width; x2 := 0;
  for y := y1 to y2 do
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        if x < x1 then x1 := x;
        if x > x2 then x2 := x;
      end;

  rec := Rect(x1, height-y2-1, x2+1, height-y1);
  Crop(rec);
end;
//------------------------------------------------------------------------------

function TImage32.GetBounds: TRect;
begin
  result := Rect(0, 0, Width, Height);
end;
//------------------------------------------------------------------------------

procedure TImage32.SetSize(newWidth, newHeight: integer);
begin
  fwidth := newWidth;
  fheight := newHeight;
  setLength(fPixels, newWidth * newHeight);
end;
//------------------------------------------------------------------------------

procedure TImage32.SetSize(newWidth, newHeight: integer; colorFill: TColor32);
begin
  SetSize(newWidth, newHeight);
  Fill(colorFill);
end;
//------------------------------------------------------------------------------

procedure TImage32.Resize(newWidth, newHeight: integer; stretchImage: Boolean);
var
  tmp: TImage32;
  rec: TRect;
begin
  if (newWidth <= 0) or (newHeight <= 0) then
  begin
    fwidth := 0;
    fheight := 0;
    fPixels := nil;
    Exit;
  end;
  if (newWidth = fwidth) and (newHeight = fheight) then Exit;

  if stretchImage then
  begin
    if fAntiAliase then
      DoResizeAA(newWidth, newHeight) else
      DoResize(newWidth, newHeight);
  end else
  begin
    tmp := TImage32.create(self);
    try
      rec := Bounds;
      SetSize(newWidth, newHeight, clNone32);
      CopyFrom(tmp, rec, rec);
    finally
      tmp.Free;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.DoResize(newWidth, newHeight: integer);
var
  x, y, srcY: integer;
  scaledXi, scaledYi: TArrayOfInteger;
  tmp: TArrayOfColor32;
  pc: PColor32;
begin
  SetLength(tmp, newWidth * newHeight * SizeOf(TColor32));

  //get scaled X & Y values once only (storing them in lookup arrays) ...
  SetLength(scaledXi, newWidth);
  for x := 0 to newWidth -1 do
    scaledXi[x] := Floor(x * fWidth / newWidth);
  SetLength(scaledYi, newHeight);
  for y := 0 to newHeight -1 do
    scaledYi[y] := Floor(y * fHeight / newHeight);

  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    srcY := scaledYi[y];
    if (srcY < 0) or (srcY >= fHeight) then Continue;
    for x := 0 to newWidth - 1 do
    begin
      pc^ := fPixels[scaledXi[x] + srcY * fWidth];
      inc(pc);
    end;
  end;

  fPixels := tmp;
  fwidth := newWidth;
  fheight := newHeight;
end;
//------------------------------------------------------------------------------

procedure TImage32.DoResizeAA(newWidth, newHeight: integer);
var
  x,y, x256,y256,xx256,yy256: integer;
  yy, sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: array of integer;
begin
  sx := fWidth/newWidth * 256;
  sy := fHeight/newHeight;
  SetLength(tmp, newWidth * newHeight);

  SetLength(scaledX, newWidth);
  scaledX[0] := 0;
  for x := 1 to newWidth - 1 do
    scaledX[x] := Round(scaledX[x-1] + sx);

  yy := 0; y256 := 0;
  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    x256 := 0;
    yy := yy + sy;
    yy256 := Round(yy * 256);
    for x := 0 to newWidth - 1 do
    begin
      xx256 := scaledX[x];
      pc^ := GetWeightedColor(fPixels, x256, y256, xx256, yy256, fWidth);
      x256 := xx256;
      inc(pc);
    end;
    y256 := yy256;
  end;

  fPixels := tmp;
  fwidth := newWidth;
  fheight := newHeight;
end;
//------------------------------------------------------------------------------

procedure TImage32.Scale(sx, sy: single);
begin
  if (sx > 0) and (sy > 0) then
    ReSize(Round(width * sx), Round(height * sy));
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertRect(var rec: TRect);
var
  t: integer;
begin
  t := rec.Top;
  rec.Top := Height - rec.Bottom;
  rec.Bottom := Height - t;
end;
//------------------------------------------------------------------------------

function TImage32.GetWeightedPixel(x256, y256: integer): TColor32;
var
  xi, yi, weight: integer;
  color: TWeightedColor;
  xf, yf: cardinal;
begin
  //coordinate integers (can be negative) -> x256 div 256 & y256 div 256.
  //coordinate fractions ->  (x256 and $FF) / 256 & (y256 and $FF) / 256
  if (x256 < -$FF) or (y256 < -$FF) or
    (x256 >= fWidth * $100) or (y256 >= fHeight * $100) then
  begin
    result := clNone32;
    Exit;
  end;
  xi := abs(x256) shr 8;
  xf := x256 and $FF;
  yi := abs(y256) shr 8;
  yf := y256 and $FF;

  color.Reset;
  weight := (($100 - xf) * ($100 - yf)) shr 8;         //top-left
  if (x256 < 0) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + yi * fWidth], weight);

  weight := (xf * ($100 - yf)) shr 8;                  //top-right
  if (xi + 1 >= fWidth) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + 1 + yi * fWidth], weight);

  weight := (($100 - xf) * yf) shr 8;                  //bottom-left
  if (x256 < 0) or (yi + 1 = fHeight) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + (yi +1) * fWidth], weight);

  weight := (xf * yf) shr 8;                           //bottom-right
  if (xi + 1 >= fWidth) or (yi + 1 = fHeight) then
    color.AddWeight(weight) else
    color.Add(fPixels[(xi + 1)  + (yi + 1) * fWidth], weight);
  Result := color.Color;
end;
//------------------------------------------------------------------------------

procedure TImage32.Rotate(angleRads: single);
var
  tmp: TArrayOfColor32;
  x, y, xi, yi, newWidth, newHeight: integer;
  sinA, cosA, dx, dy: double;
  pt, cp, cp2: TPointD;
  rec: TRectD;
  dstColor: PColor32;
begin
  if IsEmpty then Exit;
  sinA := Sin(-angleRads); cosA := cos(-angleRads);
  cp := PointD(width / 2, height / 2);
  rec.Left := 0; rec.Top := 0;
  rec.Right := Width; rec.Bottom := Height;
  rec := GetRotatedRectD(rec, angleRads);
  newWidth := Ceil(rec.Right - rec.Left);
  newHeight := Ceil(rec.Bottom - rec.Top);
  cp2 := PointD(newWidth / 2, newHeight / 2);
  SetLength(tmp, newWidth * newHeight);
  dstColor := @tmp[0];
  dx := (newWidth - fWidth) / 2;
  dy := (newHeight - fHeight) / 2;
  if fAntiAliase then
  begin
    for y := 0 to newHeight -1 do
      for x := 0 to newWidth -1 do
      begin
        pt := PointD(x, y);
        RotatePt(pt, cp2, sinA, cosA);
        xi := Round((pt.X - dx) * 256);
        yi := Round((pt.Y - dy) * 256);
        dstColor^ := GetWeightedPixel(xi, yi);
        inc(dstColor);
      end;
  end else
  begin
    for y := 0 to newHeight -1 do
      for x := 0 to newWidth -1 do
      begin
        pt := PointD(x, y);
        RotatePt(pt, cp2, sinA, cosA);
        xi := Round(pt.X - dx);
        yi := Round(pt.Y - dy);
        if (xi < 0) or (xi >= Width) or (yi < 0) or (yi >= Height) then
          dstColor^ := clNone32
        else
          dstColor^ := fPixels[xi + yi * Width];
        inc(dstColor);
      end;
  end;
  fPixels := tmp;
  fWidth := newWidth;
  fHeight := newHeight;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipVertical;
var
  i: integer;
  a: TArrayOfColor32;
  row: PColor32;
begin
  if IsEmpty then Exit;
  SetLength(a, fWidth * fHeight);
  row := @fPixels[(height-1) * width];
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[i * fWidth], fWidth * SizeOf(TColor32));
    dec(row, fWidth);
  end;
  fPixels := a;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipHorizontal;
var
  i,j, widthLess1: integer;
  a: TArrayOfColor32;
  row: PColor32;
begin
  if IsEmpty then Exit;
  SetLength(a, fWidth);
  widthLess1 := fWidth -1;
  row := @fPixels[(height-1) * width]; //top row
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[0], fWidth * SizeOf(TColor32));
    for j := 0 to widthLess1 do
    begin
      row^ := a[widthLess1 - j];
      inc(row);
    end;
    dec(row, fWidth *2);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Grayscale;
begin
  AdjustSaturation(-100);
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertColors;
var
  pc: PARGB;
  i: integer;
begin
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    pc.R := 255 - pc.R;
    pc.G := 255 - pc.G;
    pc.B := 255 - pc.B;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.ColorToAlpha(color: TColor32);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: integer;
  Q: byte;
begin
  //see - https://stackoverflow.com/questions/9280902/
  bg := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    if bg.A > 0 then
    begin
      Q := 0;
      if (bg.R > fg.R) then Q := Max(Q, DivTable[bg.R - fg.R, fg.R xor 255])
      else if (bg.R < fg.R) then Q := Max(Q, DivTable[fg.R - bg.R, fg.R]);
      if (bg.G > fg.G) then Q := Max(Q, DivTable[bg.G - fg.G, fg.G xor 255])
      else if (bg.G < fg.G) then Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);
      if (bg.B > fg.B) then Q := Max(Q, DivTable[bg.B - fg.B, fg.B xor 255])
      else if (bg.B < fg.B) then Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);
      if Q > 0 then
      begin
        bg.A := MulTable[bg.A, Q];
        bg.R := DivTable[bg.R - MulTable[fg.R, 255 - Q], Q];
        bg.G := DivTable[bg.G - MulTable[fg.G, 255 - Q], Q];
        bg.B := DivTable[bg.B - MulTable[fg.B, 255 - Q], Q];
      end else
        bg.Color := clNone32;
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustHue(percent: integer);
var
  i: integer;
  tmpImage: TArrayofHSL;
  lut: array [byte] of byte;
begin
  percent := ClampRange(percent, -100, 100);
  if percent < 0 then inc(percent, 100);
  percent := Round(percent * 255 / 100);
  if (percent = 0) or IsEmpty then Exit;
  for i := 0 to 255 do lut[i] := (i + percent) mod 255;
  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].hue := lut[ tmpImage[i].hue ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustLuminance(percent: integer);
var
  i: integer;
  tmpImage: TArrayofHSL;
  pc: single;
  lut: array [byte] of byte;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := ClampRange(percent, -100, 100);
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].lum := lut[ tmpImage[i].lum ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustSaturation(percent: integer);
var
  i: integer;
  tmpImage: TArrayofHSL;
  lut: array [byte] of byte;
  pc: single;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := ClampRange(percent, -100, 100);
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].sat := lut[ tmpImage[i].sat ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.PreMultipleAlpha(rec: TRect);
var
  i,j: integer;
  c: PARGB;
begin
  if IsEmpty then Exit;
  if rec.IsEmpty then rec := Bounds
  else CheckRect(rec);
  InvertRect(rec);
  for i := rec.Top to rec.Bottom -1 do
  begin
    c := @Pixels[i * Width];
    for j := rec.Left to rec.Right -1 do
    begin
      c.R  := MulTable[c.R, c.A];
      c.G  := MulTable[c.G, c.A];
      c.B  := MulTable[c.B, c.A];
      inc(c);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.DeMultipleAlpha(rec: TRect);
var
  i,j: integer;
  c: PARGB;
begin
  if IsEmpty then Exit;
  if rec.IsEmpty then rec := Bounds
  else CheckRect(rec);
  InvertRect(rec);
  for i := rec.Top to rec.Bottom -1 do
  begin
    c := @Pixels[i * Width];
    for j := rec.Left to rec.Right -1 do
    begin
      if c.A > 0 then
      begin
        c.R  := DivTable[c.R, c.A];
        c.G  := DivTable[c.G, c.A];
        c.B  := DivTable[c.B, c.A];
      end;
      inc(c);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.GaussianBlur(radius: integer);
var
  i, x,y,yy,z: integer;
  gaussTable: array [-MaxBlur .. MaxBlur] of Cardinal;
  wc: TWeightedColor;
  wca: TArrayOfWeightedColor;
  row: PColor32Array;
  wcRow: PWeightedColorArray;
begin

  if radius < 1 then Exit
  else if radius > MaxBlur then radius := MaxBlur;

  for i := 0 to radius do
  begin
    gaussTable[i] := Sqr(Radius - i +1);
    gaussTable[-i] := gaussTable[i];
  end;

  setLength(wca, Width * height);
  FillChar(wca[0], Width * height * SizeOf(TWeightedColor), 0);

  for y := 0 to height -1 do
  begin
    row := PColor32Array(@fPixels[y * Width]);
    wcRow := PWeightedColorArray(@wca[y * width]);
    for x := 0 to width -1 do
      for z := max(0, x - radius) to min(Width -1, x + radius) do
        wcRow[x].Add(row[z], gaussTable[x-z]);
  end;

  for x := 0 to Width -1 do
  begin
    for y := 0 to Height -1 do
    begin
      wc.Reset;
      yy := max(0, y - radius) * Width;
      for z := max(0, y - radius) to min(Height -1, y + radius)  do
      begin
        wc.Add(wca[x + yy].Color, gaussTable[y-z]);
        inc(yy, width);
      end;
      fPixels[x + y * Width] := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.BoxBlur(radius, repeats: integer);
var
  i, x,y, widthLess1, heightLess1: integer;
  pc0, pcB, pcF: PColor32;
  wc: TWeightedColor;
  buffer: TArrayOfColor32;
begin
  //nb: Using a smaller radius and repeating several times, BoxBlur can
  //achieve results very similar to GaussianBlur and in a much shorter time.

  widthLess1 := Width -1;
  heightLess1 := Height -1;
  radius := ClampRange(radius, 1, Min(widthLess1, MaxBlur));
  repeats := ClampRange(repeats, 0, 10);
  setLength(buffer, max(Width, Height));

  for repeats := 0 to repeats do
  begin
    //horizontal pass ...
    for y := 0 to heightLess1 do
    begin
      pc0 := @fPixels[y * width];
      //copy the row's pixels into a buffer because blurring spoils the color
      //of pixels being removed from the kernel (especially with larger radii).
      Move(pc0^, buffer[0], Width * SizeOf(TColor32));

      wc.Reset;
      //build the horizontal kernel (wc) using the first pixel in each row ...
      wc.Add(pc0^, 1);
      pcB := @buffer[0]; pcF := pcB;
      for i := 1 to radius do
      begin
        inc(pcF);
        wc.Add(pcF^, 1);
      end;
      pc0^ := wc.Color; //updates the first pixel in the row

      inc(pcF);
      //pcB & pcF now pointer to the color buffer representing the
      //left-most and right-most kernel pixels

      //process the rest of the row, updating the kernel each time - removing
      //the old left-most pixel in the kernel and adding the new right-most one.
      for x := 1 to widthLess1 do
      begin
        if x > radius then
        begin
          wc.Subtract(pcB^, 1);
          inc(pcB);
        end;
        wc.add(pcF^, 1);
        if x < (widthLess1 - radius) then inc(pcF);
        //updating each pixel ...
        inc(pc0);
        pc0^ := wc.Color;
      end;
    end;

    //Now repeat a very similar process in the vertical pass ...
    for x := 0 to widthLess1 do
    begin
      pc0 := @fPixels[x];
      //it's just a bit more work building a vertical pixel buffer
      pcF := pc0;
      for i := 0 to heightLess1 do
      begin
        buffer[i] := pcF^;
        inc(pcF, Width);
      end;
      wc.Reset;
      wc.Add(pc0^, 1);
      pcB := @buffer[0]; pcF := pcB;
      for i := 1 to radius do
      begin
        inc(pcF);
        wc.Add(pcF^, 1);
      end;
      pc0^ := wc.Color;
      inc(pcF);
      for y := 1 to heightLess1 do
      begin
        if y > radius then
        begin
          wc.Subtract(pcB^, 1);
          inc(pcB);
        end;
        wc.add(pcF^, 1);
        if y < (heightLess1 - radius) then inc(pcF);
        inc(pc0, Width);
        pc0^ := wc.Color;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Sharpen(radius: integer; amount: single);
var
  i: integer;
  weightAmount: array [-255 .. 255] of integer;
  bmpBlur: TImage32;
  pColor, pBlur: PARGB;
begin
  //see https://en.wikipedia.org/wiki/Unsharp_masking

  amount := ClampRange(amount, 0.05, 5);
  radius := ClampRange(radius, 1, 10);
  for i := -255 to 255 do
    weightAmount[i] := Round(amount * i);

  bmpBlur := TImage32.Create(self); //clone self
  try
    pColor := PARGB(pixelBase);
    bmpBlur.GaussianBlur(radius);
    pBlur := PARGB(bmpBlur.pixelBase);
    for i := 1 to Width * Height do
    begin
      if pColor.A > 0 then
      begin
        pColor.R := ClampByte(pColor.R  + weightAmount[pColor.R - pBlur.R]);
        pColor.G := ClampByte(pColor.G  + weightAmount[pColor.G - pBlur.G]);
        pColor.B := ClampByte(pColor.B  + weightAmount[pColor.B - pBlur.B]);
      end;
      Inc(pColor); Inc(pBlur);
    end;
  finally
    bmpBlur.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Emboss(radius: integer;
  depth: integer; luminance: integer; preserveColor: Boolean);
var
  yy,xx, x,y: integer;
  b: byte;
  kernel: array [0 .. MaxBlur, 0 .. MaxBlur] of integer;
  wca: TArrayOfWeightedColor;
  pcf, pcb: PColor32;     //pointers to pixels (forward & backward in kernel)
  pw: PWeightedColor;     //pointer to weight
  customGray: TColor32;
const
  maxDepth = 50;
begin
  //convert luminance (%) to a gray color (ignored when preserveColor = true)
  luminance := ClampRange(luminance, 0, 100);
  b := luminance *255 div 100;
  customGray := $FF000000 + b shl 16 + b shl 8 + b;

  ClampRange(radius, 1, 5);

  inc(depth);
  ClampRange(depth, 2, maxDepth);

  kernel[0][0] := 1;
  for y := 1 to radius do
    for x := 1 to radius do
      kernel[y][x] := depth;

  setLength(wca, Width * height);
  FillChar(wca[0], Width * height * SizeOf(TWeightedColor), 0);

  for y := radius to height -1 - radius do
  begin
    for x := radius to width -1 - radius do
    begin
      pw := @wca[y * width +x];
      pcb := PColor32(@fPixels[(y * Width) + x - 1]);
      if preserveColor then
      begin
        pcf := PColor32(@fPixels[(y * Width) + x]);
        pw^.Add(pcf^, kernel[0,0]);
        inc(pcf);
      end else
      begin
        pw^.Add(customGray, kernel[0,0]);
        pcf := PColor32(@fPixels[(y * Width) + x + 1]);
      end;

      //parse the kernel ...
      for yy := 1 to radius do
      begin
        for xx := 1 to radius do
        begin
          pw^.Subtract(Gray(pcf^), kernel[yy,xx]);
          pw^.Add(Gray(pcb^), kernel[yy,xx]);
          dec(pcb); inc(pcf);
        end;
        dec(pcb, Width - radius);
        inc(pcf, Width - radius);
      end;
    end;
  end;

  for x := 0 to width * height - 1 do
    fPixels[x] := wca[x].Color or $FF000000;
end;
//------------------------------------------------------------------------------

function MinAlpha(color1, color2: TColor32): TColor32;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  clr1: TARGB absolute color1;
  clr2: TARGB absolute color2;
begin
  result := Min(clr1.A, clr2.A) shl 24;
end;
//------------------------------------------------------------------------------

procedure TImage32.Glow(radius: integer; color: TColor32);
var
  i: integer;
  pc: PColor32;
  rec: TRect;
  tmp: TImage32;
begin
  if IsEmpty then Exit;

  rec := Bounds;
  tmp := TImage32.Create(self); //creates a clone of self
  try
    //re-color all pixels using the glow color ...
    pc := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      if PARGB(pc).A > 0 then
        pc^ := MinAlpha(pc^, color) or (color and $FFFFFF);
      inc(pc);
    end;

    //blur ...
    radius := ClampRange(radius, 1, MaxBlur);
    BoxBlur(radius, 2);

    //'Glow' is exponentially less dense visually than 'shadow' because it's not
    //offset so that the densest part of the glow remains hidden by the image.
    pc := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      if PARGB(pc).A > 0 then
        PARGB(pc).A := ClampByte(PARGB(pc).A * 4); //nb: multiplying alpha * 4
      inc(pc);
    end;

    //finally merge copy the original image back over the glow ...
    CopyInternal(tmp, Bounds, Bounds, BlendToAlpha);
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Shadow(radius: integer; dx, dy: integer; color: TColor32);
var
  i: integer;
  pc: PColor32;
  srcRec: TRect;
  tmp: TImage32;
begin
  //clone copy an offset of self ...
  srcRec := Bounds;
  Windows.OffsetRect(srcRec, -radius, -radius);
  tmp := TImage32.Create(self, srcRec);
  try
    //re-color all pixels using the shadow color ...
    pc := tmp.PixelBase;
    for i := 0 to tmp.Width * tmp.Height -1 do
    begin
      if PARGB(pc).A > 0 then
        pc^ := MinAlpha(pc^, color) or color and $FFFFFF;
      inc(pc);
    end;
    //blur the shadow ...
    radius := ClampRange(radius, 1, MaxBlur);
    tmp.GaussianBlur(radius);

    //merge copy self to the new shadow ...
    tmp.CopyInternal(self, self.Bounds, tmp.Bounds, BlendToAlpha);
    //finally copy the newly composed image in tmp back to self ....
    self.fPixels := tmp.fPixels;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.GetColorCount: integer;
var
  allColors: PByteArray;
  i: integer;
  c: PColor32;
const
  cube256 = 256 * 256 * 256;
begin
  result := 0;
  if IsEmpty then Exit;
  //it's probably safer to allocate allColors (a fairly large chunk of memory)
  //on the heap rather than in the stack. AllocMem zero initializes too ...
  allColors := AllocMem(cube256);
  try
    c := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      allColors[c^ and $FFFFFF] := 1;
      inc(c);
    end;
    for i := 0 to cube256 -1 do
      if allColors[i] = 1 then inc(Result);
  finally
    FreeMem(allColors);
  end;
end;
//------------------------------------------------------------------------------

function TImage32.GetAlphaIsUsed: Boolean;
var
  i: integer;
  pc: PARGB;
begin
  result := true;
  If IsEmpty then Exit;
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    if pc.A < 255 then Exit;
    inc(pc);
  end;
  result := false;
end;
//------------------------------------------------------------------------------

function TImage32.SaveToFile(const filename: string): Boolean;
var
  Image32ExtClass: TImage32ExtClass;
begin
  Image32ExtClass := GetImage32ExtClass(ExtractFileExt(filename));
  result := assigned(Image32ExtClass) and
    Image32ExtClass.SaveToFile(filename, self);
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromFile(const filename: string): Boolean;
var
  Image32ExtClass: TImage32ExtClass;
begin
  Image32ExtClass := GetImage32ExtClass(ExtractFileExt(filename));
  result := assigned(Image32ExtClass) and
    Image32ExtClass.LoadFromFile(filename, self);
end;
//------------------------------------------------------------------------------

function TImage32.GetPixel(x, y: integer): TColor32;
begin
  result := fPixels[y * width + x];
end;
//------------------------------------------------------------------------------

procedure TImage32.SetPixel(x,y: integer; color: TColor32);
begin
  fPixels[y * width + x] := color;
end;
//------------------------------------------------------------------------------

function TImage32.GetIsEmpty: Boolean;
begin
  result := fPixels = nil;
end;
//------------------------------------------------------------------------------

function TImage32.GetPixelBase: PColor32;
begin
  if IsEmpty then result := nil
  else result := @fPixels[0];
end;
//------------------------------------------------------------------------------

function TImage32.CheckRect(var rec: TRect): Boolean;
begin
  IntersectRect(rec, rec, Bounds);
  result := not rec.IsEmpty;
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyInternal(src: TImage32;
  const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
var
  i, j, srcRecWidth: integer;
  s, d: PColor32;
begin
  srcRecWidth := srcRec.Right - srcRec.Left;

  s := @src.Pixels[srcRec.Top * src.Width + srcRec.Left];
  d := @Pixels[dstRec.top * Width + dstRec.Left];

  if assigned(blendFunc) then
    for i := srcRec.Top to srcRec.Bottom -1 do
    begin
      for j := 1 to srcRecWidth do
      begin
        d^ := blendFunc(d^, s^);
        inc(s); inc(d);
      end;
      inc(s, src.Width - srcRecWidth);
      inc(d, Width - srcRecWidth);
    end
  else
    //simply overwrite src with dst (ie without blending)
    for i := srcRec.Top to srcRec.Bottom -1 do
    begin
      move(s^, d^, srcRecWidth * SizeOf(TColor32));
      inc(s, src.Width);
      inc(d, Width);
    end;
end;
//------------------------------------------------------------------------------

procedure ScaleRect(var rec: TRect; scale: TPointD);
begin
  rec.Width := Round(rec.Width * scale.X);
  rec.Height := Round(rec.Height * scale.Y);
end;
//------------------------------------------------------------------------------

function TImage32.CopyFrom(src: TImage32; srcRec, dstRec: TRect;
  blendFunc: TBlendFunction): Boolean;
var
  tmp: TImage32;
  srcRecClipped, dstRecClipped, dummy: TRect;
  scale, scaleSrc, scaleDst: TPointD;
begin
  result := false;
  if srcRec.IsEmpty or dstRec.IsEmpty then Exit;
  IntersectRect(srcRecClipped, srcRec, src.Bounds);

  //get the scaling amount (if any) before
  //dstRec might be adjusted due to clipping ...
  scale.X := dstRec.Width/srcRec.Width;
  scale.Y := dstRec.Height/srcRec.Height;

  //check if the source rec has been clipped ...
  if not IsEqualRecSizes(srcRecClipped, srcRec) then
  begin
    if srcRecClipped.IsEmpty then Exit;
    //the source has been clipped so clip the destination too ...
    scaleSrc.X := srcRecClipped.Width/srcRec.Width;
    scaleSrc.Y := srcRecClipped.Height/srcRec.Height;
    ScaleRect(dstRec, scaleSrc);
    OffsetRect(dstRec,
      srcRecClipped.Left - srcRec.Left,
      srcRecClipped.Top - srcRec.Top);
  end;

  if (scale.X <> 1.0) or (scale.Y <> 1.0) then
  begin
    //scale source (tmp) to the destination then call CopyFrom() again ...
    tmp := TImage32.Create(src, srcRecClipped);
    try
      tmp.Scale(scale.X, scale.Y);
      result := CopyFrom(tmp, tmp.Bounds, dstRec, blendFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;

  IntersectRect(dstRecClipped, dstRec, Bounds);
  if dstRecClipped.IsEmpty then Exit;

  //there's no scaling if we get here, but further clipping may be needed if
  //the destination rec is partially outside the destination image's bounds

  if not IsEqualRecSizes(dstRecClipped, dstRec) then
  begin
    //the destination rec has been clipped so clip the source too ...
    scaleDst.X := dstRecClipped.Width/dstRec.Width;
    scaleDst.Y := dstRecClipped.Height/dstRec.Height;
    ScaleRect(srcRecClipped, scaleDst);
    OffsetRect(srcRecClipped,
      dstRecClipped.Left - dstRec.Left,
      dstRecClipped.Top - dstRec.Top);
  end;

  //when copying to self and srcRec & dstRec overlap then
  //copy srcRec to a temporary image and use it as the source ...
  if (src = self) and
    IntersectRect(dummy, srcRecClipped, dstRecClipped) then
  begin
    tmp := TImage32.Create(self, srcRecClipped);
    try
      result := src.CopyFrom(tmp, tmp.Bounds, dstRecClipped, blendFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;

  src.InvertRect(srcRecClipped);
  InvertRect(dstRecClipped);
  CopyInternal(src, srcRecClipped, dstRecClipped, blendFunc);
  result := true;
end;
//------------------------------------------------------------------------------

procedure TImage32.FloodFill(x, y: integer;
  newColor: TColor32; compareFunc: TCompareFunction);
var
  i, xl, xr, xr2, direction: integer;
  maxX, maxY: integer;
  ffs: TFloodFillStack;
  ffm: TFloodFillMask;
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then Exit;
  maxX := Width -1;
  maxY := Height -1;
  //because the image is stored inverted ...
  y := maxY - y;

  ffs := TFloodFillStack.create(maxY);
  try
    xl := x; xr := x;
    ffm.Reset(Width, Height, x,y, PixelBase, compareFunc);
    ffm.SetCurrentY(y);
    ffm.IsMatch(x);

    while (xl > 0) and ffm.IsMatch(xl -1) do dec(xl);
    while (xr < maxX) and ffm.IsMatch(xr +1) do inc(xr);
    ffs.Push(xl, xr, y, -1); //down
    ffs.Push(xl, xr, y, 1);  //ip
    while not ffs.IsEmpty do
    begin
      ffs.Pop(xl, xr, y, direction);
      ffm.SetCurrentY(y);
      xr2 := xl;
      //check left ...
      if ffm.IsMatch(xl) then
      begin
        while (xl > 0) and ffm.IsMatch(xl-1) do dec(xl);
        if xl <= xr2 -2 then
          ffs.Push(xl, xr2-2, y, -direction);
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl,xr2, y, direction);
        if xr2 >= xr +2 then
          ffs.Push(xr+2, xr2, y, -direction);
        xl := xr2 +2;
      end;
      //check right ...
      while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      while (xl <= xr) do
      begin
        xr2 := xl;
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, direction);
        if xr2 >= xr +2 then
        begin
          ffs.Push(xr+2,xr2,y, -direction);
          break;
        end;
        inc(xl, 2);
        while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      end;
    end;
  finally
    ffs.Free;
  end;

  for i := 0 to High(ffm.mask) do
    if ffm.mask[i] <> 0 then fPixels[i] := newColor;
end;

//------------------------------------------------------------------------------
// TFloodFillStack methods
//------------------------------------------------------------------------------

constructor TFloodFillStack.Create(maxY: integer);
begin
  self.maxY := maxY;
end;
//------------------------------------------------------------------------------

destructor TFloodFillStack.Destroy;
var
  ffr: PFloodFillRec;
begin
  while assigned(first) do
  begin
    ffr := first;
    first := first.next;
    dispose(ffr);
  end;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Push(xLeft, xRight, y, direction: integer);
var
  ffr: PFloodFillRec;
begin
  if ((y = 0) and (direction = -1)) or
    ((y = maxY) and (direction = 1)) then Exit;
  new(ffr);
  ffr.xLeft     := xLeft;
  ffr.xRight    := xRight;
  ffr.y         := y;
  ffr.direction := direction;
  ffr.next := first;
  first := ffr;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Pop(out xLeft, xRight, y, direction: integer);
var
  ffr: PFloodFillRec;
begin
  xLeft     := first.xLeft;
  xRight    := first.xRight;
  direction := first.direction;
  y         := first.y + direction;
  ffr := first;
  first := first.next;
  dispose(ffr);
end;
//------------------------------------------------------------------------------

function TFloodFillStack.IsEmpty: Boolean;
begin
  result := not assigned(first);
end;

//------------------------------------------------------------------------------
// TFloodFillMask methods
//------------------------------------------------------------------------------

procedure TFloodFillMask.Reset(Width, Height, x, y: integer;
  pixelBase: PColor32; compareFunc: TCompareFunction);
begin
   //create a mask and fill it with zeros ...
   setLength(mask, Width * Height);
   FillChar(mask[0], Width * Height, 0);

   Self.width := width;
   Self.height := height;
   colorsBase := PColor32Array(pixelBase);
   Self.compareColor := colorsBase[x + y * Width];
   Self.compareFunc := compareFunc;
   //Self.colorsRow and Self.maskRow are left undefined here
end;
//------------------------------------------------------------------------------

procedure TFloodFillMask.SetCurrentY(y: integer);
begin
  colorsRow := @colorsBase[y * width];
  maskRow := @mask[y * width];
end;
//------------------------------------------------------------------------------

function TFloodFillMask.IsMatch(x: integer): Boolean;
begin
  if assigned(compareFunc) then
    result := (maskRow[x] = 0) and compareFunc(colorsRow[x], compareColor) else
    result := (maskRow[x] = 0) and (colorsRow[x] = compareColor);
  if result then maskRow[x] := 1;
end;

//------------------------------------------------------------------------------
// TWeightedColor record
//------------------------------------------------------------------------------

procedure TWeightedColor.Reset;
begin
  fAddCount := 0;
  fAlphaTot := 0;
  fColorTotR := 0;
  fColorTotG := 0;
  fColorTotB := 0;
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.AddWeight(weight: integer);
begin
  inc(fAddCount, weight);
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(c: TColor32; weight: integer);
var
  a: integer;
  argb: TARGB absolute c;
begin
  inc(fAddCount, abs(weight));
  a := weight * argb.A;
  if a = 0 then Exit;
  inc(fAlphaTot, a);
  inc(fColorTotB, (a * argb.B));
  inc(fColorTotG, (a * argb.G));
  inc(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Subtract(c: TColor32; weight: integer);
var
  a: integer;
  argb: TARGB absolute c;
begin
  dec(fAddCount, weight);
  a := weight * argb.A;
  if a = 0 then Exit;
  dec(fAlphaTot, a);
  dec(fColorTotB, (a * argb.B));
  dec(fColorTotG, (a * argb.G));
  dec(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

function TWeightedColor.GetColor: TColor32;
var
  a: byte;
  halfAlphaTot: integer;
  argb: TARGB absolute result;
begin
  result := clNone32;
  if (fAlphaTot = 0) or (fAddCount = 0) then Exit;
  a := DivRound(fAlphaTot, fAddCount);
  if (a = 0) then Exit;
  argb.A := a;
  halfAlphaTot := fAlphaTot div 2;
  //nb: alpha weighting is applied to colors when added
  //so we now need to div by fAlphaTot (with rounding) here ...
  argb.R := ClampByte((fColorTotR + halfAlphaTot) div fAlphaTot);
  argb.G := ClampByte((fColorTotG + halfAlphaTot) div fAlphaTot);
  argb.B := ClampByte((fColorTotB + halfAlphaTot) div fAlphaTot);
end;

//------------------------------------------------------------------------------
// Initialization functions
//------------------------------------------------------------------------------

procedure MakeBlendTables;
var
  i,j: Integer;
begin
  for j := 0 to 255 do MulTable[0, j] := 0;
  for i := 0 to 255 do MulTable[i, 0] := 0;
  for j := 0 to 255 do DivTable[0, j] := 0;
  for i := 0 to 255 do DivTable[i, 0] := 0;
  for i := 1 to 255 do
    for j := 1 to 255 do
    begin
      MulTable[i, j] := Round(i * j * div255);
      if i >= j then
        DivTable[i, j] := 255 else
        DivTable[i, j] := Round(i * $FF / j);
    end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  Image32ExtClassList := TStringList.Create;
  MakeBlendTables;

finalization
  Image32ExtClassList.Free;

end.
