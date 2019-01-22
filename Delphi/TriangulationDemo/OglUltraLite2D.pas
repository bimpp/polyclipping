unit OglUltraLite2D;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0 (beta)                                                      *
* Date      :  22 January 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  An ultra light OpenGL library to draw 2D text and graphics.     *
*              Requires OpenGL version 3.0 or higher and the Clipper Library   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, OpenGL, Math, Bitmap32,
  ClipperCore, ClipperEx, ClipperTri, ClipperText, ClipperOffset;

type
  TGlyphInfo = class
  private
    fBounds: TRect64;
    function GetBounds: TRect64;
  public
    c        : Char;
    gm       : TGlyphMetrics;
    paths    : TPaths;
    constructor Create(c: Char);
    property Bounds: TRect64 read GetBounds;
  end;

  TOglFontLib = class
  private
    fLogFont: TLogFont;
    fFont: HFont;
    fScaleMultiple: integer;
    fGlyphList: TStringList;
    procedure AddCharRange(startChar, endChar: Char);
    function Add(c: Char): TGlyphInfo;
    procedure GetStringInfo(const str: string;
      out paths: TArrayOfPaths; out gma: TGlyphMetricsArray);
    function GetStringImageInternal(const paths: TPaths;
      scaleX, scaleY: double;
      fontColor, backgroundColor: TColor32;
      linewidth: single): TBitmap32;
  public
    constructor Create(const lf: TLogFont; scaleMultiple: integer = 4); overload;
    destructor Destroy; override;

    function GetStringImage(const str: string;
      fontColor: TColor32;
      scaleX: double = 1.0; scaleY: double = 1.0): TBitmap32;
    function GetStringImageLCD(const str: string;
      fontColor, backgroundColor: TColor32;
      scaleX: double = 1.0; scaleY: double = 1.0): TBitmap32;
    function GetStringImageOutLine(const str: string;
      lineWidth: single; fontColor: TColor32;
      scaleX: double = 1.0; scaleY: double = 1.0): TBitmap32;

    function GetRawPaths(const str: string): TPaths;
    function GetRawPathsD(const str: string): TPathsD;

    property Font: HFont read fFont;
    property LogFont: TLogFont read fLogFont;
  end;

{$IFNDEF UNICODE}
  PGLenum = ^GLenum;
const
  GL_RGBA8 = $8058;
{$ENDIF}
var
  ScreenPixelsY: integer = 96;

  glDrawBuffers:
    procedure(n: GLsizei; const bufs: PGLenum); stdcall;
  //open GL ver 3.0 extensions
  wglGetProcAddress:
    function(ProcName: PAnsiChar): Pointer; stdcall;
  glGenFramebuffers:
    procedure (n: GLsizei; framebuffers: PGLuint); stdcall;
  glBindFramebuffer:
    procedure(target: GLenum; framebuffer: GLuint); stdcall;
  glDeleteFramebuffers:
    procedure(n: GLsizei; const varramebuffers: PGLuint); stdcall;
  glFramebufferTexture2D:
    procedure(target: GLenum; attachment: GLenum; textarget:
    GLenum; texture: GLuint; level: GLint); stdcall;
  glClearBufferiv:
    procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLint); stdcall;
  glClearBufferfv:
    procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLfloat); stdcall;
  glClearBufferuiv:
    procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLuint); stdcall;

{$IFNDEF UNICODE}
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); stdcall;
  glBindTexture: procedure(target: GLenum; texture: GLuint); stdcall;
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); stdcall;
{$ENDIF}

const
  GL_BGRA = $80E1;
  GL_FRAMEBUFFER = $8D40;
  GL_COLOR_ATTACHMENT0 = $8CE0;

  function CreateOglContext(window: HWND): HGLRC;
  procedure DeleteOglContext(glrc: HGLRC);
  procedure ResetOglViewport(clientwidth, clientheight: integer);

  procedure oglColor(color: TColor32); {$IFDEF INLINING} inline; {$ENDIF}
  procedure oglClearColor(color: TColor32); {$IFDEF INLINING} inline; {$ENDIF}

  //Line: can be relatively slow with wider line widths ...
  procedure Line(const paths: TPaths; const offset, scale: TPointD;
    width: single; color: TColor32; isOpen: Boolean = false);
  procedure Fill(const triangles: TPaths;
    const offset, scale: TPointD; color: TColor32);
var
  defaultLogfont : TLogfont;
  oglExtensionsInstalled : Boolean = false;

implementation

const
  fixed     : Int64 = $10000;
  fixedDiv2 : double = $8000;
  invFixed  : double = 1/$10000;
var
  oglLibHandle           : Cardinal = 0;
  lineWidthRange : array [0..1] of GLfloat;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function oglGetProcAddr(const procName: PAnsiChar): Pointer; overload;
begin
  result := GetProcAddress(oglLibHandle, procName);
  if not assigned(result) then
    result := wglGetProcAddress(procName);
end;
//------------------------------------------------------------------------------

function oglGetProcAddr(const procName: PAnsiChar;
  out proc: Pointer): Boolean; overload;
begin
  proc := GetProcAddress(oglLibHandle, procName);
  if not assigned(proc) then proc := wglGetProcAddress(procName);
  result := assigned(proc);
end;
//------------------------------------------------------------------------------

//InitOglLibrary: loads a tiny subset of the full OpenGL library.
function InitOglLibrary: Boolean;
var
  p: PAnsiChar;
begin
  if oglLibHandle = 0 then
  begin
    result := false;
    oglLibHandle := LoadLibrary(opengl32);
    if oglLibHandle = 0 then Exit;

    //Requires OpenGL ver 3+ installed ...
    p := glGetString(GL_VERSION);
    if p^ < '3' then Exit;

    wglGetProcAddress := GetProcAddress(oglLibHandle, 'wglGetProcAddress');
    if Addr(wglGetProcAddress) = nil then Exit;
    if not oglGetProcAddr('glDrawBuffers', @glDrawBuffers) then Exit;
    if not oglGetProcAddr('glGenFramebuffers', @glGenFramebuffers) then Exit;
    if not oglGetProcAddr('glBindFramebuffer', @glBindFramebuffer) then Exit;
    if not oglGetProcAddr('glDeleteFramebuffers', @glDeleteFramebuffers) then Exit;
    if not oglGetProcAddr('glFramebufferTexture2D', @glFramebufferTexture2D) then Exit;
    if not oglGetProcAddr('glClearBufferiv', @glClearBufferiv) then Exit;
    if not oglGetProcAddr('glClearBufferfv', @glClearBufferfv) then Exit;
    if not oglGetProcAddr('glClearBufferuiv', @glClearBufferuiv) then Exit;
{$IFNDEF UNICODE}
    glGenTextures := oglGetProcAddr('glGenTextures');
    glBindTexture := oglGetProcAddr('glBindTexture');
    glDeleteTextures := oglGetProcAddr('glDeleteTextures');
{$ENDIF}

    glGetFloatv(GL_LINE_WIDTH_RANGE, @lineWidthRange);
  end;
  result := true;
end;
//------------------------------------------------------------------------------

function CreateOglContext(window: HWND): HGLRC;
var
  chosenPixelFormat: integer;
  dc: HDC;
  pfd: TPIXELFORMATDESCRIPTOR;
begin
  result := 0;
  dc := GetDC(window);
  try
    fillChar(pfd, sizeof(pfd), 0);
    pfd.nSize := sizeof(TPIXELFORMATDESCRIPTOR);
    pfd.nVersion := 1;   // always 1
    pfd.dwFlags := PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_DRAW_TO_WINDOW;
    pfd.iPixelType := PFD_TYPE_RGBA;
    pfd.cColorBits := 32;
    pfd.cDepthBits := 24;
    pfd.cStencilBits := 8;
    chosenPixelFormat := ChoosePixelFormat(dc, @pfd);
    if (chosenPixelFormat = 0) or
      not SetPixelFormat(dc, chosenPixelFormat, @pfd) then Exit;

    result := wglCreateContext(dc);
    wglMakeCurrent(dc, result);
    oglExtensionsInstalled :=
      InitOglLibrary; //nb: must be called AFTER wglMakeCurrent()
  finally
    ReleaseDC(window, dc);
  end;
end;
//------------------------------------------------------------------------------

procedure DeleteOglContext(glrc: HGLRC);
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(glrc);
end;
//------------------------------------------------------------------------------

procedure ResetOglViewport(clientwidth, clientheight: integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (clientwidth = 0) or (clientheight = 0) then Exit;
  glOrtho(0, clientwidth, clientheight, 0, 0, 1);
  glViewport(0, 0, clientwidth, clientheight);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//-----------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

procedure oglColor(color: TColor32);
var
  c: TARGB absolute color;
begin
  with c do glColor4ub(r, g, b, a);
end;
//------------------------------------------------------------------------------

procedure oglClearColor(color: TColor32);
var
  c: TARGB absolute color;
begin
  with c do glClearColor(r/255, g/255, b/255, a/255);
end;
//------------------------------------------------------------------------------

procedure AppendReverse(var path: TPath);
var
  i, len: integer;
begin
  len := length(path);
  if len < 2 then Exit;
  setLength(path, len * 2 -1);
  for i := 0 to len -2 do
    path[len+i] := path[len-i -2];
end;
//------------------------------------------------------------------------------

procedure Line(const paths: TPaths; const offset, scale: TPointD;
  width: single; color: TColor32; isOpen: Boolean = false);
var
  i, j: integer;
  paths2: TPaths;
begin
  if width <= lineWidthRange[0] then Exit
  else if width <= lineWidthRange[1] then
  begin
    oglColor(color);
    glLineWidth(width);
    for i := 0 to high(paths) do
    begin
      if isOpen then
        glBegin(GL_LINE_STRIP) else
        glBegin(GL_LINE_LOOP);
      for j := 0 to high(paths[i]) do
        with paths[i][j] do
          glVertex2f(x * scale.X + offset.X, y * scale.Y + offset.Y);
      glEnd();
    end;
  end else
  begin
    if isOpen then
    begin
    paths2 := paths;
    for i := 0 to high(paths2) do
      AppendReverse(paths2[i]);
    end else
    begin
      paths2 := ReversePaths(paths);
      AppendPaths(paths2, paths);
    end;
    paths2 := ClipperOffsetPaths(paths2, width/2, jtRound, etPolygon);
    paths2 := Union(paths2, frEvenOdd);
    paths2 := Triangulate(paths2);
    Fill(paths2, offset, scale, color);
  end;
end;
//------------------------------------------------------------------------------

procedure Fill(const triangles: TPaths;
  const offset, scale: TPointD; color: TColor32);
var
  i, j: integer;
begin
  oglColor(color);
  for i := 0 to high(triangles) do
  begin
    glBegin(GL_TRIANGLES);
    for j := 0 to high(triangles[i]) do
      with triangles[i][j] do
        glVertex2f(x * scale.X + offset.X, y * scale.Y + offset.Y);
    glEnd();
  end;
end;
//------------------------------------------------------------------------------

procedure ApplyLcd(drawBmp: TBitmap32;
  fontColor, bkColor: TColor32);
var
  h,w: integer;
  a: cardinal;
  src, dst: PARGB;
  fgColor: TARGB absolute fontColor;
  bgColor: TARGB absolute bkColor;
begin
  //create R, G & B color intensities based on pixel alpha values at
  //virtual red, green & blue color offsets ...
  for h := 0 to drawBmp.Height -1 do
  begin
    src := PARGB(@drawBmp.Pixels[h * drawBmp.Width]);
    dst := src;
    for w := 0 to drawBmp.Width -1 do
    begin
      dst.R := src.A;
      inc(src);
      dst.G   := src.A;
      inc(src);
      dst.B := src.A;
      inc(src);
      a := (dst.R + dst.G + dst.B);
      if a > 255 then dst.A := 255 else dst.A := (a * a) shr 8;
      //dst.A := 255; alternative, but too much color bleeding
      inc(dst);
    end;
  end;

  //now blend the font color onto the background using the
  //color intensities in the color map above ...
  for h := 0 to drawBmp.Height -1 do
  begin
    src := PARGB(@drawBmp.Pixels[h * drawBmp.Width]);
    for w := 0 to drawBmp.Width div 3 -1 do
    begin
      src.R := (bgColor.R shl 16 +
        (fgColor.R - bgColor.R) * (src.R * src.A)) shr 16;
      src.G := (bgColor.G shl 16 +
        (fgColor.G - bgColor.G) * (src.G * src.A)) shr 16;
      src.B := (bgColor.B shl 16 +
        (fgColor.B - bgColor.B) * (src.B * src.A)) shr 16;
      src.A := 255;
      inc(src);
    end;
  end;

  //finally remove the right 2/3 of the image ...
  drawBmp.Crop(Rect(0,0, drawBmp.Width div 3, drawBmp.Height));
end;

//------------------------------------------------------------------------------
// TOglFontLib class
//------------------------------------------------------------------------------

constructor TGlyphInfo.Create(c: Char);
begin
  fBounds := nullRect;
  self.c := c;
end;
//-----------------------------------------------------------------------------

function TGlyphInfo.GetBounds: TRect64;
begin
  if fBounds.IsEmpty and (length(paths) > 0) then
    fBounds := ClipperCore.GetBounds(paths);
  result := fBounds;
end;

//-----------------------------------------------------------------------------
// TOglFontLib class
//------------------------------------------------------------------------------

function FindFmt(idx: integer): string; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := Format('%4.4x', [idx]);
end;
//------------------------------------------------------------------------------

function FindFmt(ch: Char): string; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := FindFmt(Ord(ch));
end;
//------------------------------------------------------------------------------

constructor TOglFontLib.Create(const lf: TLogFont; scaleMultiple: integer);
begin
  //At least with smaller font heights, acceptable drawing requires vector
  //fonts to be rasterized at multiples of their final resolutions. This allows
  //the generated images to be scaled back to their intended resolutions with
  //decent antialiasing. Of course to render really high quality vector fonts,
  //specialized font software is needed to utilize complex 'hinting' that's
  //embedded inside the font files.
  fScaleMultiple := max(1, scaleMultiple);

  fLogFont := lf;
  //scale up the height before creating font ...
  fLogFont.lfHeight := lf.lfHeight * fScaleMultiple;
  fFont := CreateFontIndirect(fLogFont);
  //now save the original height ...
  fLogFont.lfHeight := fLogFont.lfHeight div fScaleMultiple;
  fGlyphList := TStringList.Create;
  AddCharRange(#32, #126);
  fGlyphList.Sorted := true;
end;
//------------------------------------------------------------------------------

destructor TOglFontLib.Destroy;
var
  i: integer;
begin
  if fFont <> 0 then DeleteObject(fFont);
  for i := 0 to fGlyphList.Count -1 do
    fGlyphList.Objects[i].Free;
  fGlyphList.Free;
end;
//------------------------------------------------------------------------------

procedure TOglFontLib.AddCharRange(startChar, endChar: Char);
var
  i, len: integer;
  glyphInfo: TGlyphInfo;
  pathsArray: TArrayOfPaths;
  gma: TGlyphMetricsArray;
  s: string;
begin
  len := Ord(endChar) - Ord(startChar) +1;
  setLength(s, len);
  for i := 1 to len do
  begin
    s[i] := startChar;
    inc(startChar);
  end;
  pathsArray := GetCharInfos(s, fFont, gma);
  for i := 1 to len do
  begin
    glyphInfo := TGlyphInfo.Create(s[i]);
    glyphInfo.paths := pathsArray[i-1];
    glyphInfo.gm := gma[i-1];
    fGlyphList.AddObject(FindFmt(s[i]), glyphInfo);
  end;
end;
//------------------------------------------------------------------------------

function TOglFontLib.Add(c: Char): TGlyphInfo;
var
  idx: integer;
begin
  if fGlyphList.Find(FindFmt(c), idx) then
  begin
    result := TGlyphInfo(fGlyphList.Objects[idx]);
  end else
  begin
    result := TGlyphInfo.Create(c);
    result.paths := GetCharInfo(c, fFont, result.gm);
    fGlyphList.AddObject(FindFmt(c), result);
  end;
end;
//------------------------------------------------------------------------------

procedure TOglFontLib.GetStringInfo(const str: string;
  out paths: TArrayOfPaths; out gma: TGlyphMetricsArray);
var
  i,idx,len: integer;
  glyphInfo: TGlyphInfo;
begin
  paths := nil;
  len := length(str);
  setLength(paths, len);
  setLength(gma, len);
  for i := 1 to len do
  begin
    if fGlyphList.Find(FindFmt(str[i]), idx) then
      glyphInfo := TGlyphInfo(fGlyphList.Objects[idx]) else
      glyphInfo := Add(str[i]);
    paths[i-1] := glyphInfo.paths;
    gma[i-1] := glyphInfo.gm;
  end;
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetStringImageInternal(const paths: TPaths;
  scaleX, scaleY: double; fontColor, backgroundColor: TColor32;
  linewidth: single): TBitmap32;
var
  triangles: TPaths;
  textures: GLuint;
  fbos: GLuint;
  savedView: TRect;
  rec: TRect64;
  lcdScaling: integer;
begin
  if scaleX = 0 then scaleX := 1;
  if scaleY = 0 then scaleY := 1;

  rec := GetBounds(paths);
  //convert rec from FIXED to Int64 values (vertices will be scaled below)
  rec := FixedToRect64(rec);
  rec := InflateRect(rec, 2,1);
  if backgroundColor <> clNone32 then
  begin
    lcdScaling := 3;
    inc(rec.Right, (rec.Right - rec.Left) * 2);
  end else
  begin
    lcdScaling := 1;
    if linewidth > 0 then
      rec := InflateRect(rec, Ceil(linewidth/2), Ceil(linewidth/2));
  end;

  glGetIntegerv(GL_VIEWPORT, @savedView);
  ResetOglViewport(rec.Width,rec.Height);

  //set up the texture and frame buffers ...
  glGenTextures(1, @textures);
  glBindTexture(GL_TEXTURE_2D, textures);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, rec.Width, rec.Height,
    0, GL_BGRA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glGenFramebuffers(1, @fbos);
  glBindFramebuffer(GL_FRAMEBUFFER, fbos);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
    GL_TEXTURE_2D, textures, 0);

  oglClearColor($00000000);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if linewidth <= 0 then //polygon fill
  begin
    triangles := union(paths, frEvenOdd); //is occasionally necessary
    triangles := Triangulate(triangles);
    //draw to the texture buffer scaling from FIXED to float values ...
    Fill(triangles, PointD(-rec.Left, -rec.Top),
      PointD(invFixed * lcdScaling, invFixed), fontcolor);
  end else
  begin
    Line(paths, PointD(-rec.Left, -rec.Top),
      PointD(invFixed, invFixed), linewidth, fontcolor, false);
  end;

  //create the TBitmap32 result and copy the drawn image to it ...
  result := TBitmap32.Create(rec.Width, rec.Height);
  //result.EnableAntiAliase := false; //best left enabled :)
  glReadPixels(0,0, rec.Width, rec.Height,
    GL_BGRA, GL_UNSIGNED_BYTE, result.Pixels);

  Result.Resize(Round(Result.Width * scaleX), Round(Result.Height * scaleY));
  if backgroundColor <> clNone32 then
    ApplyLcd(Result, fontColor, backgroundColor);

  //clean up ...
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDeleteFramebuffers(1, @fbos);
  glDeleteTextures(1, @textures);
  ResetOglViewport(savedView.Right - savedView.Left,
    savedView.Bottom - savedView.Top);
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetStringImage(const str: string; fontColor: TColor32;
  scaleX, scaleY: double): TBitmap32;
var
  i,len, x,y: integer;
  pathsArray: TArrayOfPaths;
  paths: TPaths;
  gma: TGlyphMetricsArray;
begin
  len := length(str);
  if (len = 0) or not oglExtensionsInstalled then
  begin
    result := TBitmap32.Create;
    Exit;
  end;
  GetStringInfo(str, pathsArray, gma);

  x := gma[0].gmCellIncX;
  y := -gma[0].gmCellIncY;
  for i := 1 to len -1 do
  begin
    pathsArray[i] := OffsetPaths(pathsArray[i], x * fixed, y * fixed);
    inc(x, gma[i].gmCellIncX);
    dec(y, gma[i].gmCellIncY); //nb: dec not inc
  end;
  paths := ArrayOfPathsToPaths(pathsArray);

  if scaleX = 0 then
    scaleX := 1/fScaleMultiple else
    scaleX :=  scaleX/fScaleMultiple;
  if scaleY = 0 then
    scaleY := 1/fScaleMultiple else
    scaleY :=  scaleY/fScaleMultiple;

  result := GetStringImageInternal(paths,
    scaleX, scaleY, fontColor, clNone32, 0);
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetStringImageLCD(const str: string;
  fontColor, backgroundColor: TColor32;
  scaleX, scaleY: double): TBitmap32;
var
  i,len, x,y: integer;
  pathsArray: TArrayOfPaths;
  paths: TPaths;
  gma: TGlyphMetricsArray;
begin
  len := length(str);
  if (len = 0) or not oglExtensionsInstalled then
  begin
    result := TBitmap32.Create;
    Exit;
  end;

  GetStringInfo(str, pathsArray, gma);

  x := gma[0].gmCellIncX;
  y := -gma[0].gmCellIncY;
  for i := 1 to len -1 do
  begin
    pathsArray[i] := OffsetPaths(pathsArray[i], x * fixed, y * fixed);
    inc(x, gma[i].gmCellIncX);
    dec(y, gma[i].gmCellIncY); //nb: dec not inc
  end;
  paths := ArrayOfPathsToPaths(pathsArray);

  if scaleX = 0 then
    scaleX := 1/fScaleMultiple else
    scaleX :=  scaleX/fScaleMultiple;
  if scaleY = 0 then
    scaleY := 1/fScaleMultiple else
    scaleY :=  scaleY/fScaleMultiple;

  result := GetStringImageInternal(paths, scaleX, scaleY,
    fontColor, backgroundColor, 0);
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetStringImageOutLine(const str: string;
  lineWidth: single; fontColor: TColor32;
  scaleX: double; scaleY: double): TBitmap32;
var
  i,len, x,y: integer;
  pathsArray: TArrayOfPaths;
  paths, paths2: TPaths;
  gma: TGlyphMetricsArray;
  scaleAv: double;
begin
  len := length(str);
  if (len = 0) or not oglExtensionsInstalled
    or (lineWidth < lineWidthRange[0]) then
  begin
    result := TBitmap32.Create;
    Exit;
  end;
  GetStringInfo(str, pathsArray, gma);

  if scaleX = 0 then
    scaleX := 1/fScaleMultiple else
    scaleX :=  scaleX/fScaleMultiple;
  if scaleY = 0 then
    scaleY := 1/fScaleMultiple else
    scaleY :=  scaleY/fScaleMultiple;

  x := gma[0].gmCellIncX;
  y := -gma[0].gmCellIncY;
  for i := 1 to len -1 do
  begin
    pathsArray[i] := OffsetPaths(pathsArray[i], x * fixed,y * fixed);
    inc(x, gma[i].gmCellIncX);
    dec(y, gma[i].gmCellIncY); //nb: dec not inc
  end;
  paths := ArrayOfPathsToPaths(pathsArray);
  if lineWidth >= lineWidthRange[1] then
  begin
    paths2 := ReversePaths(paths);
    AppendPaths(paths, paths2);
    scaleAv := (scaleX + scaleY)/2;
    paths := ClipperOffsetPaths(paths,
      lineWidth * fixedDiv2 / scaleAv / 2, jtRound, etPolygon);
    result := GetStringImageInternal(paths, scaleX, scaleY,
      fontColor, clNone32, 0);
  end else
    result := GetStringImageInternal(paths,
      scaleX, scaleY, fontColor, clNone32, lineWidth);
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetRawPaths(const str: string): TPaths;
var
  paths: TArrayOfPaths;
  gma: TGlyphMetricsArray;
begin
  GetStringInfo(str, paths, gma);
  result := ArrayOfPathsToPaths(paths);
end;
//------------------------------------------------------------------------------

function TOglFontLib.GetRawPathsD(const str: string): TPathsD;
var
  paths: TPaths;
begin
  paths := GetRawPaths(str);
  result := ScalePathsD(paths, invFixed, invFixed);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure InitDefaultLogfont;
var
  dc: HDC;
begin
  dc := GetDC(0);
  ScreenPixelsY := GetDeviceCaps(dc, LOGPIXELSY);
  ReleaseDC(0, dc);

  FillChar(defaultLogfont, sizeof(defaultLogfont), 0);
  with defaultLogfont do
  begin
    lfHeight := - 12;
    lfEscapement := 0; //angle degrees * 10
    lfWeight := FW_THIN;//FW_NORMAL;
    lfCharSet := DEFAULT_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    lfFaceName := 'Segoe UI';//'Arial';
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  InitDefaultLogfont;
finalization
  if oglLibHandle <> 0 then
    FreeLibrary(oglLibHandle);

end.
