program Triangulation;


{$R main.res}
{$R winxp.res}

uses
  Windows,
  CommCtrl,
  Messages,
  OpenGL,
  ShellAPI,
  SysUtils,
  Classes,
  Math,
  ClipperCore in '..\ClipperCore.pas',
  Clipper in '..\Clipper.pas',
  ClipperEx in '..\ClipperEx.pas',
  ClipperTri in '..\ClipperTri.pas',
  ClipperText in '..\ClipperText.pas',
  ClipperOffset in '..\ClipperOffset.pas',
  Bitmap32 in 'Bitmap32.pas',
  OglUltraLite2D in 'OglUltraLite2D.pas';

{$WARN SYMBOL_PLATFORM OFF}

type
   TTextAlign = (taTopLeft, taTopRight, taBottomRight, taBottomLeft,
     taCenterTop, taCenterBottom, taLeftCenter, taRightCenter, taCenterCenter);

const
  defaultSize: TSize = (cx:1200; cy: 800);
  backgroundColor: TColor32 = $FFF7F7F5; //off-white
  max_colors = 16;
  arrowCursor = 0;
  waitCursor  = 1;
  cliptype: array[TClipType] of string =
    ('  No clipping','  Intersection','  Union','  Difference','  XOR');
  nonsense: string = 'The quick brown fox jumps over the lazy dog.';
var
  mainHdl, statusHdl, menuHdl, customHdl: HWND;
  glrc: HGLRC;                              //handle to OpenGL rendering context
  szTitle: array [0 .. 256] of CHAR;        //The title bar text
  szWindowClass: array [0 .. 256] of CHAR;  //the main window class name
  cursors: array [0..1] of HCURSOR;
  subj, clip, solution, subjTri, clipTri, solutionTri: TPaths;
  clientwidth, clientheight: WORD;
  margin: integer;        //minimum margin around polygons
  roundto: integer = 10;  //rounds polygon coordinates to nearest 10 units.
  colors: array [0..max_colors-1] of Cardinal;  //for multi-color solutions

  simpleFontlib, fancyFontlib: TOglFontLib;     //see OglUltraLite2D

  //lcdText: Enables sub-pixel font rendering.
  // * Enabled: text will appear slightly crisper but with some color bleading
  // * Disabled: text will appear less crisp and also slightly bolder.
  // * Only suitable for lcd displays (with suitable sub-pixel geometry)
  lcdText: Boolean         = true;//false;//

  //multiColorBrush: when enabled, clipping solutions will be displayed in
  //multiple colors so triangulation is clearly visible.
  multiColorBrush: Boolean = false;

  edgeCount: integer     = 10;
  fill_rule: TFillRule   = frEvenOdd;
  clip_type: TClipType   = ctIntersection;
  show_polygons: Boolean = true;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//DPIScale: useful to adjust text (and image) size on high res. displays
function DPIScale(val: single): integer;
begin
  result := Round( val * ScreenPixelsY / 96);
end;
//------------------------------------------------------------------------------

function Union(const paths: TPaths; fr: TFillRule): TPaths;
begin
  with TClipperEx.Create do
  try
    AddPaths(paths, ptSubject);
    Execute(ctUnion, result, fr);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function GradientColor(color1, color2: TColor32; frac: single): TColor32;
var
  b1,g1,r1,b2,g2,r2: byte;
begin
  if frac >= 1 then result := color2
  else if frac <= 0 then result := color1
  else
  begin
    b1 := (color1 shr 16) and $FF;
    g1 := (color1 shr 8) and $FF;
    r1 := (color1 and $FF);

    b2 := (color2 shr 16) and $FF;
    g2 := (color2 shr 8) and $FF;
    r2 := (color2 and $FF);

    b1 := trunc(b1*(1-frac) + b2*frac);
    g1 := trunc(g1*(1-frac) + g2*frac);
    r1 := trunc(r1*(1-frac) + r2*frac);
    result := (b1 shl 16) or (g1 shl 8) or r1;
  end;
end;
//------------------------------------------------------------------------------

function GradientRainbowColor(fraction: single; alpha: byte): TColor32;
const
  clrs: array [0..5] of TColor32 =
    ($FF0000, $FFFF00, $00FF00, $00FFFF, $0000FF, $FF00FF);
var
  f: integer;
begin
  if (fraction <= 0) or (fraction >= 1) then
    result := clrs[0] or (alpha shl 24)
  else
  begin
    fraction := fraction * 6;
    f := trunc(fraction);
    if f = 5 then
      result := (alpha shl 24) or GradientColor(clrs[5], clrs[0], fraction - f)
    else
      result := (alpha shl 24) or GradientColor(clrs[f], clrs[f+1], fraction - f);
  end;
end;
//------------------------------------------------------------------------------

function MakeRandomPath(defaultWidth, defaultHeight, count: Integer;
  RoundTo: Integer = 10; margin: Integer = 10): TPath;
var
  i: Integer;
begin
  setlength(Result, count);
  for i := 0 to count -1 do with Result[i] do
  begin
    X := ((Random(defaultWidth - 2 * margin) + margin) div RoundTo) * RoundTo;
    Y := ((Random(defaultHeight - 2 * margin) + margin) div RoundTo) * RoundTo;
  end;
end;
//==============================================================================

procedure FillMultiColor(const triangles: TPaths; offset, scale: TPointD);
var
  i, j, color_idx: integer;
begin
  color_idx := 0;
  for i := 0 to high(triangles) do
  begin
    oglColor(colors[color_idx]);
    color_idx := (color_idx + 1) mod max_colors;
    glBegin(GL_TRIANGLES);
    for j := 0 to high(triangles[i]) do
      with triangles[i][j] do
        glVertex2f(x * scale.X + offset.X, y * scale.Y + offset.Y);
    glEnd();
  end;
end;
//------------------------------------------------------------------------------

procedure DrawPolygons;
var
  rec, rec2: TRect64;
  scale, offset: TPointD;
  dx: integer;
begin
  rec := GetBounds(subj);
  rec2 := GetBounds(clip);
  rec := UnionRects(rec, rec2);
  if rec.Right = rec.Left then Exit;

  //prepare to scale and center polygons in window ...
  scale.X := min((clientwidth - margin *2) / (rec.Right - rec.Left),
    (clientheight - margin *2) / (rec.Bottom - rec.Top));
  scale.Y := scale.X;

  offset.X := Round((clientwidth -
    (rec.Right-rec.Left) * scale.X)/2 - rec.Left * scale.X);
  offset.Y := Round((clientheight -
    (rec.Bottom-rec.Top) * scale.Y)/2 - rec.Top * scale.Y);

  if clip_type <> ctUnion then
  begin
    //draw the subject polygons ...
    if assigned(subj) then
    begin
      Line(subj, offset, scale, 0.3, $40000033, false);
      Fill(subjTri, offset, scale, $100000FF);
    end;

    //draw the clip polygons ...
    if assigned(clip) then
    begin
      Line(clip, offset, scale, 0.3, $40333300, false);
      Fill(clipTri, offset, scale, $20AAAA00);
    end;
  end;

  //draw the polygons in the clip solution ...
  if assigned(solution) then
  begin

    //draw a soft '3D' shadow under the solution ...
    dx := ceil(2.5 / scale.X);
    solutionTri := OffsetPaths(solutionTri, dx, dx);
    Fill(solutionTri, offset, scale, $40000000);
    solutionTri := OffsetPaths(solutionTri, -dx,-dx);

    if multiColorBrush then
    begin
      //multiColorBrush renders each triangle in a different color ...
      FillMultiColor(solutionTri, offset, scale);
      Line(solutionTri, offset, scale, 0.3, $20000000);
    end else
    begin
      //just draw the solution polygons in a monochrome green ...
      Fill(solutionTri, offset, scale, $FF00FF00);
    end;
    //finally, line the non-triangulated solution ...
    Line(solution, offset, scale, 0.8, $AA000000);
  end;
end;
//------------------------------------------------------------------------------

function DrawSimpleText(posX, posY: integer; const text: string;
  textAlign: TTextAlign; color: TColor32; scaleX, scaleY: single): TRectD;
var
  textures: GLuint;
  bmp: TBitmap32;
  centerOff: TPointD;
begin
  //1. Get the text image ...
  if lcdText then
    bmp := simpleFontLib.GetStringImageLCD(text,
      color, backgroundColor, scaleX, scaleY)
  else
    bmp := simpleFontLib.GetStringImage(text, color, scaleX, scaleY);

  //2. position where to draw the text image ...
  centerOff := PointD(bmp.Width /2, bmp.Height /2);
  case textAlign of
    taTopLeft:
      result := RectD(posX, posY, posX + bmp.Width, posY + bmp.Height);
    taTopRight:
      result := RectD(posX - bmp.Width, posY, posX, posY + bmp.Height);
    taBottomRight:
      result := RectD(posX - bmp.Width, posY - bmp.Height, posX, posY);
    taBottomLeft:
      result := RectD(posX, posY - bmp.Height, posX + bmp.Width, posY);
    taCenterTop:
      result := RectD(posX-centerOff.X, posY,
        posX+centerOff.X, posY + bmp.Height);
    taCenterBottom:
      result := RectD(posX-centerOff.X, posY - bmp.Height,
        posX+centerOff.X, posY);
    taLeftCenter:
      result := RectD(posX, posY-centerOff.Y,
        posX+bmp.Width, posY+centerOff.Y);
    taRightCenter:
      result := RectD(posX-bmp.Width, posY-centerOff.Y,
        posX, posY+centerOff.Y);
    taCenterCenter:
      result := RectD(posX-centerOff.X, posY-centerOff.Y,
        posX+centerOff.X, posY+centerOff.Y);
  end;

  //3. create a texture of the text image ...
  glGenTextures(1, @textures);
  glBindTexture(GL_TEXTURE_2D, textures);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, bmp.Width, bmp.Height, 0,
    GL_BGRA, GL_UNSIGNED_BYTE, bmp.Pixels);

  //either set the text color here ...
  //  oglColor($FF0000FF);
  //  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
  //or preserve texture color (essential with lcdText enabled) ...
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

  //4. draw the text image ...
  glEnable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
    glTexCoord2i(0,1); glVertex2f(Result.Left, Result.Top);
    glTexCoord2i(1,1); glVertex2f(Result.Right, Result.Top);
    glTexCoord2i(1,0); glVertex2f(Result.Right, Result.Bottom);
    glTexCoord2i(0,0); glVertex2f(Result.Left, Result.Bottom);
  glEnd;

  //5. clean up ...
  bmp.Free;
  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDeleteTextures(1, @textures);
end;
//------------------------------------------------------------------------------

procedure DrawOutlineText;
var
  i, linewidth, lwDiv3: integer;
  offset: TPoint;
  textures: array [0..1] of GLuint;
  bmp: TBitmap32;
  bmp2: TBitmap32;
  s: string;
  c: TColor32;
begin
  linewidth := DPIScale(16);
  lwDiv3 := linewidth div 3;

  //1. make up 5 random characters ...
  setlength(s, 5);
  for i := 1 to 5 do s[i] := Char(Random(94)+33);
  c := colors[Random(max_colors)] or $FF000000;

  //2. get their raster images (both solid and outline) using fancyFontlib ...
  bmp := fancyFontLib.GetStringImage(s, MakeLighter(c,75));
  bmp2 := fancyFontLib.GetStringImageOutLine(s, linewidth, MakeDarker(c,65));

  //3. prepare to center these text images (without scaling) ...
  offset.X := Round((clientwidth - bmp2.Width)/2);
  offset.Y := Round((clientheight - bmp2.Height)/2);

  //COPY THESE IMAGES TO TEXTURES  AND DRAW THEM ...

  //4. create 'textures' of the 2 text images ...
  glGenTextures(2, @textures[0]);
  //the solid text is copied into textures[0] ...
  glBindTexture(GL_TEXTURE_2D, textures[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Bmp.Width, Bmp.Height, 0,
    GL_BGRA, GL_UNSIGNED_BYTE, bmp.Pixels);
  //and the outline text is copied into textures[1] ...
  glBindTexture(GL_TEXTURE_2D, textures[1]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Bmp2.Width, Bmp2.Height, 0,
    GL_BGRA, GL_UNSIGNED_BYTE, bmp2.Pixels);


  glEnable(GL_TEXTURE_2D);
  //glBindTexture(GL_TEXTURE_2D, textures[1]); //still current :)

  //5. draw the outline text image as shadow (semi-transparent gray ) ...
  oglColor($44000000);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
  glBegin(GL_QUADS);
    glTexCoord2i(0,1); glVertex2i(offset.x, offset.y);
    glTexCoord2i(1,1); glVertex2i(offset.x+Bmp2.width, offset.y);
    glTexCoord2i(1,0); glVertex2i(offset.x+Bmp2.width, offset.y+Bmp2.height);
    glTexCoord2i(0,0); glVertex2i(offset.x, offset.y+Bmp2.height);
  glEnd;

  //6. draw solid text image (original color) ...
  glBindTexture(GL_TEXTURE_2D, textures[0]);         //important
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

  glBegin(GL_QUADS);
    glTexCoord2i(0,1); glVertex2i(offset.X, offset.y);
    glTexCoord2i(1,1); glVertex2i(offset.X+Bmp.width, offset.y);
    glTexCoord2i(1,0); glVertex2i(offset.X+Bmp.width, offset.y+Bmp.height);
    glTexCoord2i(0,0); glVertex2i(offset.X, offset.Y+Bmp.height);
  glEnd;

  dec(offset.x, lwDiv3); dec(offset.y, lwDiv3);
  //7. draw text outline (offset half the linewidth) ...
  glBindTexture(GL_TEXTURE_2D, textures[1]);
  glBegin(GL_QUADS);
    glTexCoord2i(0,1); glVertex2i(offset.x,offset.y);
    glTexCoord2i(1,1); glVertex2i(offset.x+Bmp2.width, offset.y);
    glTexCoord2i(1,0); glVertex2i(offset.x+Bmp2.width, offset.y+Bmp2.height);
    glTexCoord2i(0,0); glVertex2i(offset.x,offset.y+Bmp2.height);
  glEnd;

  //8. clean up ...
  bmp.Free;
  bmp2.Free;

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDeleteTextures(2, @textures);
end;
//------------------------------------------------------------------------------

procedure Draw(dc: HDC);
var
  rec: TRect;
begin
  oglClearColor(backgroundColor);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glLineWidth(1.0);
  if not oglExtensionsInstalled then Exit;

  if show_polygons then
    DrawPolygons
  else
  begin
    lcdText := true;
    DrawSimpleText(clientwidth - 20, clientheight -20, nonsense,
      taBottomRight, $FF000033, 1,1);
    lcdText := false;
    DrawSimpleText(clientwidth div 2, clientHeight div 2,
      'just some background text to demonstrate transparency',
      taCenterCenter, $AA000000, 1.5, 5);
    DrawOutlineText;
  end;
end;
//------------------------------------------------------------------------------

function UpdateSample: Boolean;
begin
  result := false;
  if not oglExtensionsInstalled then Exit;
  SetWindowText(statusHdl, PChar(cliptype[clip_type]));
  with TClipperEx.Create do
  try
    AddPaths(subj, ptSubject);
    AddPaths(clip, ptClip);
    result := Execute(clip_type, solution, fill_rule);
  finally
    free;
  end;
  if not result then Exit;

  //now triangulate ...
  subjTri := Union(subj, fill_rule);
  subjTri := Triangulate(subjTri);
  clipTri := Union(clip, fill_rule);
  clipTri := Triangulate(clipTri);
  solutionTri := Triangulate(solution);
end;
//------------------------------------------------------------------------------

procedure MakeNewPolygons;
begin
  if show_polygons then
  begin
    SetCursor(cursors[waitCursor]);
    setLength(subj, 1); setLength(clip, 1);
    subj[0] := MakeRandomPath(defaultSize.cx, defaultSize.cy,
      edgeCount, roundto, margin);
    clip[0] := MakeRandomPath(defaultSize.cx, defaultSize.cy,
      edgeCount, roundto, margin);
    UpdateSample;
  end;
  InvalidateRect(customHdl, nil, true);
end;
//------------------------------------------------------------------------------

procedure RedrawPolygons;
begin
  if show_polygons then UpdateSample;
  InvalidateRect(customHdl, nil, true);
end;
//------------------------------------------------------------------------------

function AboutProc(dlgHdl: HWND; message: cardinal;
  wParm: WPARAM; lParm: LPARAM): integer; stdcall;
var
  recDesktop, recDialog: TRECT;
begin
  result := integer(false);
    case (message) of
    WM_INITDIALOG:
      begin
        //center About dialog ...
        GetWindowRect(dlgHdl, recDialog);
        recDialog.right := recDialog.right - recDialog.left;
        recDialog.bottom := recDialog.bottom - recDialog.top;
        SystemParametersInfo(SPI_GETWORKAREA, 0, @recDesktop, 0);
        SetWindowPos(dlgHdl, HWND_TOP,
          (recDesktop.right - recDialog.right) div 2,
          (recDesktop.bottom - recDialog.bottom) div 2,
          0, 0, SWP_NOSIZE or SWP_NOREDRAW or SWP_NOACTIVATE);
        result := 1;
      end;
    WM_COMMAND:
        if (LOWORD(wParm) = IDOK) or (LOWORD(wParm) = IDCANCEL) then
        begin
          EndDialog(dlgHdl, LOWORD(wParm));
          result := 1;
        end;
    end;
end;
//-----------------------------------------------------------------------------

function ClipTypeFromMenuId(menuId: integer): TClipType;
begin
  result := TClipType((menuId-300) div 10);
end;
//-----------------------------------------------------------------------------

procedure SetClipType(menuId: integer);
var
  wasChecked: Boolean;
  i: integer;
begin

  wasChecked :=
    (GetMenuState(menuHdl, menuId, 0) and MF_CHECKED) = MF_CHECKED;
  for i := 0 to 4 do
    CheckMenuItem(menuHdl, i*10 + 300, MF_UNCHECKED);

  if not wasChecked then
  begin
    CheckMenuItem(menuHdl, menuId, MF_CHECKED);
    clip_type := ClipTypeFromMenuId(menuId);
  end else
    clip_type := ctNone;

  RedrawPolygons;
end;
//-----------------------------------------------------------------------------

function FillRuleFromMenuId(menuId: integer): TFillRule;
begin
  result := TFillRule((menuId-200) div 10);
end;
//-----------------------------------------------------------------------------

procedure SetFillRule(menuId: integer);
begin
  CheckMenuItem(menuHdl, 200, MF_UNCHECKED);
  CheckMenuItem(menuHdl, 210, MF_UNCHECKED);
  CheckMenuItem(menuHdl, menuId, MF_CHECKED);
  fill_rule := FillRuleFromMenuId(menuId);
  RedrawPolygons;
end;
//-----------------------------------------------------------------------------

procedure SetDrawOption(menuId: integer);
var
  enableOption: cardinal;
begin
  CheckMenuItem(menuHdl, 801, MF_UNCHECKED);
  CheckMenuItem(menuHdl, 802, MF_UNCHECKED);
  CheckMenuItem(menuHdl, menuId, MF_CHECKED);
  show_polygons := menuId = 801;
  InvalidateRect(customHdl, nil, true);

  //if the display option is text then there are a whole lot
  //of menu items that are irrelevant and potentially confusing ...
  if show_polygons then
    enableOption := MF_ENABLED else
    enableOption := MF_DISABLED;
  EnableMenuItem(menuHdl, 109, enableOption);
  EnableMenuItem(menuHdl, 110, enableOption);
  EnableMenuItem(menuHdl, 120, enableOption);
  EnableMenuItem(menuHdl, 130, enableOption);
  EnableMenuItem(menuHdl, 140, enableOption);
  EnableMenuItem(menuHdl, 150, enableOption);
  EnableMenuItem(menuHdl, 200, enableOption);
  EnableMenuItem(menuHdl, 210, enableOption);
  EnableMenuItem(menuHdl, 310, enableOption);
  EnableMenuItem(menuHdl, 320, enableOption);
  EnableMenuItem(menuHdl, 330, enableOption);
  EnableMenuItem(menuHdl, 340, enableOption);
  EnableMenuItem(menuHdl, 400, enableOption);
end;
//-----------------------------------------------------------------------------

procedure SetEdgeCount(count: integer);
var
  i: integer;
begin
  for i := 1 to 5 do CheckMenuItem(menuHdl, i*10 +100, MF_UNCHECKED);
  CheckMenuItem(menuHdl, count +100, MF_CHECKED);
  edgeCount := count;
  MakeNewPolygons;
end;
//-----------------------------------------------------------------------------

function IsChecked(menuId: integer): Boolean;
begin
  result := GetMenuState(menuHdl, menuId, MF_BYCOMMAND) and MF_CHECKED <> 0;
end;
//-----------------------------------------------------------------------------

procedure SetMultiColor;
begin
  multiColorBrush := not multiColorBrush;
  if multiColorBrush then
    CheckMenuItem(menuHdl, 400, MF_CHECKED) else
    CheckMenuItem(menuHdl, 400, MF_UNCHECKED);
  InvalidateRect(mainHdl, nil, true);
end;
//-----------------------------------------------------------------------------

function MainProc(hdl: HWND; message: UINT;
  wparm: WPARAM; lparm: LPARAM): LRESULT; stdcall;
var
  wmId: WORD;
  rec: TRECT;
begin
  result := 0;
  case (message) of
    WM_SIZE:
      begin
        SetWindowPos(statusHdl, 0, 0,0,0,0, SWP_NOZORDER);
        GetWindowRect(statusHdl, rec);
        SetWindowPos(customHdl,0, 0,0,LOWORD(lparm),
          HIWORD(lparm) - (rec.Bottom-rec.Top), SWP_NOZORDER);
        margin := clientheight div 20;
      end;
    WM_COMMAND:
      begin
        Result := 0;
        wmId := LOWORD(wParm);
        case (wmId) of
          500: MakeNewPolygons;        //Enter key
          101: DestroyWindow(hdl);     //Exit key
          102: DialogBox(HInstance, MAKEINTRESOURCE(100), hdl, @AboutProc);

          //Edge Counts ...
          110,120,130,140,150: SetEdgeCount(wmId-100);

          //Filling Rules ...
          200, 210: SetFillRule(wmId);

          //Clip Types ...
          300,310,320,330,340: SetClipType(wmId);

          //Multicolor filling ...
          400: SetMultiColor;

          801, 802: SetDrawOption(wmId);

          else
            result := DefWindowProc(hdl, message, wParm, lParm);
        end;
      end;
    WM_DESTROY: PostQuitMessage(0);

    else result := DefWindowProc(hdl, message, wParm, lParm);
  end;
end;

//------------------------------------------------------------------------------

function CustomProc(hdl: HWND; message: UINT;
  wparm: WPARAM; lparm: LPARAM): LRESULT; stdcall;
var
  ps: PAINTSTRUCT;
  dc: HDC;
  rec: TRECT;
begin
  case message of
    WM_SIZE:
      begin
        clientwidth := LOWORD(lparm);
        clientheight := HIWORD(lparm);
        ResetOglViewport(clientwidth, clientheight); //OglUltraLite2D
        Result := 0;
      end;
    WM_LBUTTONDOWN:
      begin
        MakeNewPolygons;
        Result := 0;
      end;
    WM_ERASEBKGND: result := 1;
    WM_PAINT:
    begin
      dc := BeginPaint(hdl, ps);
      GetClientRect(hdl, rec);
      try
        Draw(dc);
      except
      end;
      SwapBuffers(dc);
      EndPaint(hdl, ps);
      SetCursor(cursors[arrowCursor]);
      result := 0;
    end;

    else result := CallWindowProc(pointer(GetWindowLong(hdl, GWL_USERDATA)),
        hdl, message, wparm, lparm);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

type
  bytes4 = packed array [0..3] of byte;
var
  i: integer;
  wcex: WNDCLASSEX;
  accelTblHdl: HACCEL;
  msg: TMSG;
  rec: TRect;
  lf: TLogfont;
  simpleTextFontHeight: integer;
const
  noExtensions: string = ' Error: This demo requires OpenGL version 3 or above.';
begin
  Randomize;
  szTitle := 'Delphi OpenGL Demo';
  szWindowClass := 'Delphi_OpenGL_Demo';
  wcex.cbSize := sizeof(WNDCLASSEX);
  wcex.style          := CS_HREDRAW or CS_VREDRAW;
  wcex.lpfnWndProc    := @MainProc;
  wcex.cbClsExtra     := 0;
  wcex.cbWndExtra     := 0;
  wcex.hInstance      := hInstance;
  wcex.hIcon          := LoadIcon(hInstance, MAKEINTRESOURCE(100));
  wcex.hCursor        := LoadCursor(0, IDC_ARROW);
  wcex.hbrBackground  := (COLOR_WINDOW+1);
  wcex.lpszMenuName   := MAKEINTRESOURCE(100);
  wcex.lpszClassName  := szWindowClass;
  wcex.hIconSm        := 0;
  RegisterClassEx(wcex);

  cursors[arrowCursor] := LoadCursor(0, IDC_ARROW);
  cursors[waitCursor] := LoadCursor(0, IDC_WAIT);

  for i := 0 to max_colors -1 do
    colors[i] := GradientRainbowColor(i / max_colors, $AA);
  Randomize;
  InitCommonControls; //for statusbar

  // Perform application initialization:
  SystemParametersInfo(SPI_GETWORKAREA, 0, @rec, 0);
  mainHdl := CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
    ((rec.Right-rec.Left) - defaultSize.cx) div 2,
    ((rec.Bottom-rec.Top) - defaultSize.cy) div 2,
    defaultSize.cx, defaultSize.cy, 0, 0, hInstance, nil);

  menuHdl := GetMenu(mainHdl);
  multiColorBrush := IsChecked(400);

  customHdl := CreateWindowEx(0, 'BUTTON', '',
    BS_OWNERDRAW or WS_CHILD or WS_VISIBLE,
    0, 0, 10, 10, mainHdl, Cardinal(-1), hInstance, nil);
  //subclass the custom 'button' control ...
  i := SetWindowLong(customHdl, GWL_WNDPROC, integer(@CustomProc));
  SetWindowLong(customHdl, GWL_USERDATA, i); //store the old WndProc

  glrc := CreateOglContext(customHdl); //OglUltraLite2D

  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

  simpleTextFontHeight := DPIScale( -11);

  lf               := defaultLogfont;
  lf.lfHeight      := simpleTextFontHeight;
  lf.lfEscapement  := 0;
  simpleFontlib := TOglFontLib.Create(lf);

  lf               := defaultLogfont;
  lf.lfHeight      := DPIScale( -196 );
  lf.lfEscapement  := 200;     //rotated 20 degrees
  lf.lfFaceName    := 'Comic Sans MS';
  //for large fonts, we need minimal if any (render) scaling ...
  fancyFontlib := TOglFontLib.Create(lf, 2);

  statusHdl := CreateWindowEx(0, STATUSCLASSNAME, ' ',
    SBARS_SIZEGRIP or WS_CHILD or WS_VISIBLE,
    0, 0, 0, 0, mainHdl, Cardinal(-1), hInstance, nil);

  if not oglExtensionsInstalled then
    SetWindowText(statusHdl, PChar(noExtensions));

  SetEdgeCount(10);
  SetFillRule(210);   //non-zero
  SetClipType(320);   //union
  SetDrawOption(801); //show polygons

  MakeNewPolygons;

  ShowWindow(mainHdl, CmdShow);
  UpdateWindow(mainHdl);

  accelTblHdl := LoadAccelerators(hInstance, MAKEINTRESOURCE(100));
  while (GetMessage(msg, 0, 0, 0)) do
  begin
    if TranslateAccelerator(msg.hwnd, accelTblHdl, msg) = 0 then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;

  //clean up ...
  DeleteOglContext(glrc); //OglUltraLite2D
  simpleFontlib.Free;
  fancyFontlib.Free;
  AnimateWindow(mainHdl, 500, AW_HIDE or AW_BLEND);
end.


