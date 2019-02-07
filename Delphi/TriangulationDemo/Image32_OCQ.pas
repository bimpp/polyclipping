unit Image32_OCQ;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  7 February 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Color reduction for TImage32                                    *
*           :  Uses Octree Color Quantization & Floyd / Steinberg Dithering    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses Windows, SysUtils, Classes, Math, Image32;

//ReduceColors: Reduces the image's color count & returns the color palette.
//Dithering is recommended especially for images with fewer than 256 colors.
function ReduceColors(image: TImage32; MaxColors: integer;
  UseDithering: Boolean = true): TArrayOfColor32;


implementation

type

  //Octree Color Quantization:
  //https://web.archive.org/web/20140605161956/ -->
  // <-- http://www.microsoft.com/msj/archive/S3F1.aspx

  TOctNode = class;
  TOctNodes8 = array[0 .. 7] of TOctNode;

  TOctNode = class
    protected
      IsLeaf     : Boolean;
      Level      : integer;
      Count      : integer;
      Next       : TOctNode;
      Childs     : TOctNodes8;
      TotalR     : integer;
      TotalG     : integer;
      TotalB     : integer;
      procedure  Add(color: TColor32);
      procedure  Get(var color: TColor32);
      procedure  GetNearest(var color: TColor32);
    public
      constructor Create(aLevel: byte);
      destructor Destroy; override;
  end;

  TOctree = class
    protected
      Leaves     : integer;
      MaxColors  : integer;
      Top        : TOctNode;
      Reducible8 : TOctNodes8;
      procedure  Reduce;
      procedure  Delete(var node: TOctNode);
    public
      constructor Create(aMaxColors: integer);
      destructor  Destroy; override;
      procedure  Add(color: TColor32);
      procedure  GetNearest(var color: TColor32);
      function   GetPalette: TArrayOfColor32;
  end;

  PARGBArray = ^TARGBArray;
  TARGBArray = array [0 .. $FFFFFF -1] of TARGB;

const
  NullOctNodes8 : TOctNodes8 = (nil, nil, nil, nil, nil, nil, nil, nil);
  palette16: array [0 .. 15] of TColor32 = (
    $FFFFFFFF, $FF05F3FC, $FF0264FF, $FF0608DD,
    $FF8408F2, $FFA50046, $FFD40000, $FFEAAB02,
    $FF14B71F, $FF116400, $FF052C56, $FF3A7190,
    $FFC0C0C0, $FF808080, $FF404040, $FF000000);

//------------------------------------------------------------------------------
// Miscellaneous Octree functions
//------------------------------------------------------------------------------

function GetIndex(color: TColor32; level: byte): byte;
const
  mask: array[0..7] of Byte= ($80, $40, $20, $10, $8, $4, $2, $1);
var
  argb: TARGB absolute color;
  shift: integer;
begin
  shift := 7 - level;
  Result:=
    ((argb.R   and mask[level]) shr (shift - 2)) or
    ((argb.G and mask[level]) shr (shift - 1)) or
    ((argb.B  and mask[level]) shr shift);
end;

//------------------------------------------------------------------------------
// TOctNode methods
//------------------------------------------------------------------------------

constructor TOctNode.Create(aLevel: byte);
begin
  Level   := aLevel;
  IsLeaf  := Level = 8;
  Next    := nil;
  Childs  := NullOctNodes8;
  TotalR  := 0;
  TotalG  := 0;
  TotalB  := 0;
  Count   := 0;
end;
//------------------------------------------------------------------------------

destructor TOctNode.Destroy;
var
  i: integer;
begin
  for i:= 0 to 7 do Childs[i].Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TOctNode.Add(color: TColor32);
var
  argb: TARGB absolute color;
begin
  Inc (TotalR, argb.R);
  Inc (TotalG, argb.G);
  Inc (TotalB, argb.B);
  Inc (Count);
end;
//------------------------------------------------------------------------------

procedure TOctNode.GetNearest(var color: TColor32);
var
  i, j: integer;
begin
  if not IsLeaf then
  begin
    i := GetIndex(color, level);
    if not assigned(Childs[i]) then
    begin
      //we'll only ever get here when we're matching a color that wasn't in the
      //pre-reduced image (eg when dithering an image after color reduction).

      //I've tried picking the child with the closer color match but the extra
      //complexity and computational effort seems wasted given that there's
      //no discernable improvement in image reproduction compared to simply
      //assigning the first discovered child node ...

      for j := 7 downto 0 do //ie ?? perfer more rather than fewer colors
        if assigned(Childs[j]) then
          Childs[j].GetNearest(color);
    end
    else
      Childs[i].GetNearest(color);
  end
  else
    Get(color);
end;
//------------------------------------------------------------------------------

procedure TOctNode.Get(var color: TColor32);
var
  argb: TARGB absolute color;
begin
  if Count > 0 then
  begin
    argb.R := TotalR div Count;
    argb.G := TotalG div Count;
    argb.B := TotalB div Count;
    argb.A := 255;
  end;
end;

//------------------------------------------------------------------------------
// TOctree methods
//------------------------------------------------------------------------------

constructor TOctree.Create(aMaxColors: integer);
begin
  self.MaxColors := Max(8, aMaxColors); //breaks with < 8 maxColors
  Leaves := 0;
  Top := TOctNode.Create(0);
  Reducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

destructor TOctree.Destroy;
begin
  Delete(Top);
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TOctree.Reduce;
var
  i, childCnt: integer;
  node: TOctNode;
begin
  //find the lowest level with a reducible node ...
  i:= 7;
  while (i > 0) and not Assigned(Reducible8[i]) do Dec(i);

  //now reduce the most recently added node at level 'i' ...
  node           := Reducible8[i];
  Reducible8[i]  := node.Next;
  node.IsLeaf    := True;
  node.TotalR    := 0;
  node.TotalG    := 0;
  node.TotalB    := 0;
  node.Count     := 0;
  childCnt       := 0;

  //now merge the leaves into the parent node ...
  for i:= 0 to 7 do
    if Assigned (node.Childs[i]) then
    begin
      Inc (node.TotalR, node.Childs[i].TotalR);
      Inc (node.TotalG, node.Childs[i].TotalG);
      Inc (node.TotalB, node.Childs[i].TotalB);
      Inc (node.Count, node.Childs[i].Count);
      node.Childs[i].Free;
      node.Childs[i]:= nil;
      inc(childCnt);
    end;
  Dec(Leaves, childCnt -1);
end;
//------------------------------------------------------------------------------

procedure TOctree.Add(color: TColor32);
var
  argb: TARGB absolute color;

 procedure AddColor(var node: TOctNode; level: byte);
 begin
   if not Assigned(node) then
   begin
     node:= TOctNode.Create(level +1);
     if node.IsLeaf then
     begin
       Inc(Leaves);
     end else
     begin
       node.Next  := Reducible8[node.level];
       Reducible8[node.level] := node;
     end;
   end;

   if node.IsLeaf then
     node.Add(color) else
     AddColor(node.Childs[GetIndex(color, node.level)], node.level);
 end;

begin
  AddColor(Top, 0);
  while (Leaves > MaxColors) do Reduce;
end;
//------------------------------------------------------------------------------

procedure TOctree.GetNearest(var color: TColor32);
begin
  Top.GetNearest(color);
end;
//------------------------------------------------------------------------------

procedure TOctree.Delete(var node: TOctNode);
var
  i: integer;
begin
  for i := Low (node.Childs) to High (node.Childs) do
    if Assigned(node.Childs[i]) then
      Delete(node.Childs[i]);
  FreeAndNil(node);
end;
//------------------------------------------------------------------------------

function TOctree.GetPalette: TArrayOfColor32;
var
  count: integer;

  procedure FillPalette(Node: TOctNode);
  var
    i: integer;
  begin
    if Node.IsLeaf then
    begin
      Node.Get(Result[Count]);
      Inc(Count);
    end else
    begin
      for i := 0 to 7 do
        if assigned(Node.Childs[i]) then
          FillPalette(Node.Childs[i]);
    end;
  end;

begin
  SetLength(result, Leaves);
  count := 0;
  FillPalette(Top);
end;

//------------------------------------------------------------------------------
// Floyd / Steinberg Dithering -
// see https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
//------------------------------------------------------------------------------

var
  Mul1Div16Table: array [-255 .. 255] of integer;
  Mul3Div16Table: array [-255 .. 255] of integer;
  Mul5Div16Table: array [-255 .. 255] of integer;
  Mul7Div16Table: array [-255 .. 255] of integer;

procedure Monochrome(var color: TColor32);
var
  c: TARGB absolute color;
begin
  c.R := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
  if c.R > 127 then c.R := 255 else c.R := 0;
  c.G := c.R; c.B := c.R; c.A := 255;
end;
//------------------------------------------------------------------------------

procedure Dither(image: TImage32; octree: TOctree);
var
  preReducedColor      : TARGB;
  X, Y          : Integer;
  currLine      : PARGBArray;
  NextLine      : PARGBArray;
  Color         : TARGB;
  qeR,qeG, qeB  : integer;

  function ClampByte(val: integer): Byte;
  begin
    if val < 0 then Result:= 0
    else if val >= 255 then Result:= 255
    else Result:= val;
  end;

begin
  NextLine := nil;
  currLine := @image.pixels[0];
  for Y := 0 to image.Height-1 do
  begin
    if Y < image.Height-1 then
      NextLine := @image.pixels[(Y+1) * image.Width];
    for X := 0 to image.Width-1 do
    begin
      preReducedColor := currLine[X];

      //now get the reduced color ...
      if assigned(octree) then
        octree.GetNearest(currLine[X].Color) else
        Monochrome(currLine[X].Color);

      qeR := preReducedColor.R - currLine[X].R;
      qeG := preReducedColor.G - currLine[X].G;
      qeB := preReducedColor.B - currLine[X].B;

      if X < image.Width-1 then
      begin
        currLine[x+1].R := ClampByte(currLine[x+1].R + Mul7Div16Table[qeR]);
        currLine[x+1].G := ClampByte(currLine[x+1].G + Mul7Div16Table[qeG]);
        currLine[x+1].B := ClampByte(currLine[x+1].B + Mul7Div16Table[qeB]);
      end;

      if Y < image.Height -1 then
      begin
        if X < image.Width-1 then
        begin
          NextLine[x+1].R := ClampByte(NextLine[x+1].R + Mul1Div16Table[qeR]);
          NextLine[x+1].G := ClampByte(NextLine[x+1].G + Mul1Div16Table[qeG]);
          NextLine[x+1].B := ClampByte(NextLine[x+1].B + Mul1Div16Table[qeB]);
        end;

        if X > 0 then
        begin
          NextLine[x-1].R := ClampByte(NextLine[x-1].R + Mul3Div16Table[qeR]);
          NextLine[x-1].G := ClampByte(NextLine[x-1].G + Mul3Div16Table[qeG]);
          NextLine[x-1].B := ClampByte(NextLine[x-1].B + Mul3Div16Table[qeB]);
        end;

        NextLine[x].R := ClampByte(NextLine[x].R + Mul5Div16Table[qeR]);
        NextLine[x].G := ClampByte(NextLine[x].G + Mul5Div16Table[qeG]);
        NextLine[x].B := ClampByte(NextLine[x].B + Mul5Div16Table[qeB]);
      end;
    end;
    currLine := NextLine;
  end;
end;

//------------------------------------------------------------------------------
// ReduceColors ...
//------------------------------------------------------------------------------

function ReduceColors(image: TImage32; MaxColors: integer;
  UseDithering: Boolean): TArrayOfColor32;
var
  i, count: integer;
  pc: PARGB;
  octree: TOctree;
begin
  //reduce colors less than 8 to monochrome (we can't use Octree)
  if MaxColors < 16 then
  begin
    setLength(Result, 2);
    result[0] := clBlack32;
    result[1] := clWhite32;

    image.Grayscale;
    if UseDithering then
    begin
      Dither(image, nil);
    end else
    begin
      pc := PARGB(image.PixelBase);
      for i := 0 to image.Width * image.Height - 1 do
      begin
        if pc.B > 127 then
          pc.Color := clWhite32 else
          pc.Color := clBlack32;
        inc(pc);
      end;
    end;
    Exit;
  end;

  MaxColors := Min(256, MaxColors);
  setLength(result, MaxColors);
  octree := TOctree.Create(MaxColors);
  try
    //color adaptive palettes seem to do poorly with very small palettes
    //so it's generally better to use a standard palette ...
    if MaxColors <= 16 then
    begin
      for i := 0 to high(palette16) do
        octree.Add(palette16[i]);
    end else
    begin
      pc := PARGB(image.PixelBase);
      for i := 0 to image.Width * image.Height - 1 do
      begin
        octree.Add(pc.Color);
        inc(pc);
      end;
    end;
    result :=  octree.GetPalette;

    if UseDithering then
    begin
      Dither(image, octree);
    end else
    begin
      pc := PARGB(image.PixelBase);
      for i := 0 to image.Width * image.Height - 1 do
      begin
        octree.GetNearest(pc.Color);
        inc(pc);
      end;
    end;

  finally
    octree.Free;
  end;
end;

//------------------------------------------------------------------------------
// Initialization functions
//------------------------------------------------------------------------------

procedure MakeDitherTables;
var
  i: Integer;
begin
  for i := -255 to 255 do
  begin
    Mul1Div16Table[i] := Round(i / 16);
    Mul3Div16Table[i] := Round(i * 3 / 16);
    Mul5Div16Table[i] := Round(i * 5 / 16);
    Mul7Div16Table[i] := Round(i * 7 / 16);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeDitherTables;

end.

