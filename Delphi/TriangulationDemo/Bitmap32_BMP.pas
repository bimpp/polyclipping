unit Bitmap32_BMP;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  24 January 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  BMP file format extension for TBitmap32                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Windows, Math, Bitmap32;

type

  //TBitmap32Ext_BMP.LoadFromFile() loads correctly all 'good' BMP images
  //in Jason Summers' test suite - see http://entropymine.com/jason/bmpsuite/
  //For notes on RLE bitmap compression, see ...
  //https://docs.microsoft.com/en-us/windows/desktop/gdi/bitmap-compression

  TBitmap32Ext_BMP = class(TBitmap32Ext)
    class function SaveToFile(const filename: string;
      bmp32: TBitmap32): Boolean; override;
    class procedure SaveToStream(stream: TStream; bmp32: TBitmap32);
    class function LoadFromFile(const filename: string;
      bmp32: TBitmap32): Boolean; override;
    class function LoadFromStream(stream: TStream;
      bmp32: TBitmap32): Boolean;
  end;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

class function TBitmap32Ext_BMP.SaveToFile(const filename: string;
  bmp32: TBitmap32): Boolean;
var
  stream: TFilestream;
begin
  result := not bmp32.IsEmpty;
  if not result then Exit;
  stream := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(stream, bmp32);
  finally
    stream.Free;
  end;
end;
//------------------------------------------------------------------------------

class procedure TBitmap32Ext_BMP.SaveToStream(stream: TStream;
  bmp32: TBitmap32);
var
  BH: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
begin
  FillChar(BH, sizeof(BH), #0);
  BH.bfType := $4D42;
  BH.bfOffBits := sizeof(BI) + sizeof(BH);
  BH.bfSize := BH.bfOffBits + Cardinal(Length(bmp32.Pixels)) * sizeof(TColor32);
  FillChar(BI, sizeof(BI), #0);
  BI.biSize := sizeof(BI);
  BI.biWidth := bmp32.Width;
  BI.biHeight := bmp32.Height;
  BI.biPlanes := 1;
  BI.biBitCount := 32;
  BI.biSizeImage := bmp32.Width * bmp32.Height * sizeof(TColor32);
  BI.biCompression := BI_RGB;

  stream.Write(BH, sizeof(BH));
  stream.Write(BI, sizeof(BI));
  stream.Write(bmp32.Pixels[0], Length(bmp32.Pixels) * sizeof(TColor32));
end;
//------------------------------------------------------------------------------

function GetPaletteFromStream(stream: TStream; count, size: integer): TColor32Array;
var
  i: integer;
  c: TARGB;
begin
  setLength(Result, count);
  for i := 0 to count -1 do
  begin
    stream.Read(c, size);
    with c do result[i] := $FF000000 +  R shl 16 + G shl 8 + B;
  end;
end;
//------------------------------------------------------------------------------

function GetPixelsFromStreamUsingBitfields(stream: TStream; width, height,
  bpp: integer; bitfields: TTriColor32): TColor32Array;
var
  i,j,bytesPerRow, bytesPerPix: integer;
  shift, size: TTriColor32;
  buffer: PByte;
  dstPixel: PColor32;
  b: PCardinal;
begin

  //from the bitfields masks, get R, G & B color offsets and sizes
  for i := 0 to 2 do
  begin
    size[i] := 0;
    shift[i] := 0;
    for j := 0 to 31 do
      if (size[i] > 0) then
      begin
        if bitfields[i] and (1 shl j) > 0 then inc(size[i])
        else break;
      end
      else if bitfields[i] and (1 shl j) > 0 then
      begin
        shift[i] := j;
        size[i] := 1;
      end;
  end;

  //colorXBit.R = (buffer^ and bitfields[0]) shr shift[0]
  //So to convert colorXBit.R to color32bit.R ...
  //color32bit.R = colorXBit.R * 255 div (1 shl size[0] -1)

  //convert size[x] to color channel scaling factor ...
  for i := 0 to 2 do size[i] := (1 shl size[i]) - 1;
  //now ... color32bit.R = colorXBit.R * 255 div size[0]

  bytesPerPix := bpp div 8;
  bytesPerRow := ((31 + bpp * width) div 32) * 4;
  setLength(Result, width * height);
  GetMem(buffer, bytesPerRow);
  try
    for i := 0 to height -1 do
    begin
      stream.Read(buffer^, bytesPerRow);
      b := PCardinal(buffer);
      dstPixel := @result[i * width];
      for j := 0 to width -1 do
      begin
        dstPixel^ := $FF000000 +
          (((b^ and bitfields[0]) shr shift[0]) * 255 div size[0]) shl 16 +
          (((b^ and bitfields[1]) shr shift[1]) * 255 div size[1]) shl 8 +
          (((b^ and bitfields[2]) shr shift[2]) * 255 div size[2]);
        inc(dstPixel);
        inc(PByte(b), bytesPerPix);
      end;
    end;
  finally
    FreeMem(buffer);
  end;
end;
//------------------------------------------------------------------------------

function GetPixelsFromStreamUsingPalette(stream: TStream;
  width, height, bpp: integer;
  const palette: TColor32Array): TColor32Array;
var
  i,j,bytesPerRow, palHigh, pxCnt: integer;
  buffer, b: PByte;
  dstPixel: PColor32;
  c, shift: byte;
begin
  shift := 8 - bpp;
  bytesPerRow := ((31 + bpp * width) div 32) * 4;
  setLength(Result, width * height);
  palHigh := High(palette);
  GetMem(buffer, bytesPerRow);
  try
    for i := 0 to height -1 do
    begin
      stream.Read(buffer^, bytesPerRow);
      b := buffer;
      dstPixel := @result[i * width];
      pxCnt := 0;
      for j := 0 to width -1 do
      begin
        pxCnt := (pxCnt + bpp) mod 8;
        c := b^ shr shift;
        if c > palHigh then dstPixel^ := clNone32
        else dstPixel^ := palette[c];
        if  pxCnt = 0 then inc(b)
        else b^ := b^ shl bpp;
        inc(dstPixel);
      end;
    end;
  finally
    FreeMem(buffer);
  end;
end;
//------------------------------------------------------------------------------

function GetByte(var ptr: PByte): Byte; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := ptr^;
  inc(ptr);
end;
//------------------------------------------------------------------------------

function GetByte(var ptr: PByte; is4Bit, isSecond: Boolean): Byte; overload;
begin
  if not is4Bit then
  begin
    result := ptr^;
    inc(ptr);
  end
  else if isSecond then
  begin
    result := ptr^ and $F;
    inc(ptr)
  end else
    result := ptr^ shr 4;
end;
//------------------------------------------------------------------------------

function GetPixelsFromStreamUsingPalAndRleCompression(stream: TStream;
  width, height, bpp: integer;
  const palette: TColor32Array): TColor32Array;
var
  i,j,k, cnt, idx, buffLen, w, delta: integer;
  dst: PColor32;
  byte1, byte2: byte;
  buffer: Pointer;
  b: PByte;
  is4Bit: boolean;
const
  COMMAND_BYTE = 0;
  DELTA_MODE = 2;
begin
  setLength(Result, width * height);
  buffLen := stream.Size -  stream.Position;
  getMem(buffer, buffLen);
  stream.Read(buffer^, buffLen);
  is4Bit := bpp = 4;
  b := buffer;
  try
    for i := 0 to height -1 do
    begin
      dst := @result[i * width];
      w := 0; idx := 0;
      while w < width do
      begin
        byte1 := GetByte(b);
        byte2 := GetByte(b);
        if byte1 = COMMAND_BYTE then
        begin
          if byte2 < 2 then Exit          //error
          else if byte2 = DELTA_MODE then
          begin
            cnt := GetByte(b);
            delta := GetByte(b);
            if delta > 0 then Exit;       //Y-delta never seen & not supported
            for k := 1 to cnt do
            begin
              dst^ := palette[idx];
              inc(w);
              inc(dst);
            end;
          end
          else                            //'absolute mode'
          begin
            cnt := byte2;
            for k := 1 to cnt do
            begin
              idx := GetByte(b, is4Bit, not Odd(k));
              dst^ := palette[idx];
              inc(w);
              inc(dst);
            end;
            if is4Bit and Odd(cnt) then inc(b);
            if Cardinal(b) mod 2 = 1 then
              inc(b);                     //ie must be WORD aligned
          end;
        end else                          //'encoded mode'
        begin
          cnt := byte1;
          if is4Bit then
          begin
            for j := 1 to cnt do
            begin
              if Odd(j) then
                idx := byte2 shr 4 else
                idx := byte2 and $F;
              dst^ := palette[idx];
              inc(w);
              inc(dst);
            end;
          end else
          begin
            idx := byte2;
            for j := 1 to cnt do
            begin
              dst^ := palette[idx];
              inc(w);
              inc(dst);
            end;
          end;
        end;
      end;
      byte1 := GetByte(b);
      byte2 := GetByte(b);
      if (byte1 <> 0) or (byte2 <> 0) then Exit;
    end;
  finally
    FreeMem(buffer);
  end;
end;
//------------------------------------------------------------------------------

function ValidateBitFields(const bitFields: TTriColor32): boolean;
begin
  //make sure each color channel has a mask and that they don't overlap ...
  result := (bitFields[0] <> 0) and (bitFields[1] <> 0) and
    (bitFields[2] <> 0) and (bitFields[0] and bitFields[1] = 0) and
    (bitFields[0] and bitFields[2] = 0) and (bitFields[1] and bitFields[2] = 0);
end;
//------------------------------------------------------------------------------

class function TBitmap32Ext_BMP.LoadFromFile(const filename: string;
  bmp32: TBitmap32): Boolean;
var
  fileStream: TFileStream;
begin
  result := false;
  if not FileExists(filename) then Exit;
  fileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(fileStream, bmp32);
  finally
    fileStream.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TBitmap32Ext_BMP.LoadFromStream(stream: TStream;
  bmp32: TBitmap32): Boolean;
var
  palEntrySize: integer;
  BH: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  tmp, pal: TColor32Array;
  bitfields: TTriColor32;
  isTopDown, validBitFields: boolean;
begin
  result := false;
  with stream do
  begin
    if Size < sizeof(BH) + sizeof(BI) then Exit;
    Read(BH, sizeof(BH));
    Read(BI, sizeof(BI));

    palEntrySize := 4;
    if BI.biSize = sizeof(BITMAPCOREHEADER) then
    begin
      BI.biBitCount     := PBitmapCoreHeader(@BI).bcBitCount;
      BI.biHeight       := PBitmapCoreHeader(@BI).bcHeight;
      BI.biWidth        := PBitmapCoreHeader(@BI).bcWidth;
      BI.biCompression  := 0;
      BI.biClrUsed      := 0;
      palEntrySize      := 3;
    end;

    if (BH.bfType <> $4D42) or (BI.biSize < sizeof(BITMAPCOREHEADER)) or
      (BI.biCompression > BI_BITFIELDS) then Exit;

    isTopDown := BI.biHeight < 0;
    BI.biHeight := abs(BI.biHeight);

    validBitFields := false;
    if ((BI.biCompression and BI_BITFIELDS) = BI_BITFIELDS) then
    begin
      stream.Position := 54;
      stream.Read(bitfields[0], Sizeof(TTriColor32));
      validBitFields := ValidateBitFields(bitfields);
      if stream.Position < sizeof(BH) + BI.biSize then
        stream.Position := sizeof(BH) + BI.biSize;
    end else
    begin
      stream.Position := sizeof(BH) + BI.biSize;
    end;

    if not validBitFields then
    begin
      if BI.biBitCount = 24 then
      begin
        bitfields[0] := $FF shl 16;
        bitfields[1] := $FF shl 8;
        bitfields[2] := $FF;
        validBitFields := true;
      end
      else if BI.biBitCount = 16 then
      begin
        bitfields[0] := $1F shl 10;
        bitfields[1] := $1F shl 5;
        bitfields[2] := $1F;
        validBitFields := true;
      end;
    end;

    if (BI.biClrUsed = 0) and (BI.biBitCount < 16) then
      BI.biClrUsed := Trunc(Power(2, BI.biBitCount));
    if BI.biClrUsed > 0 then
      pal := GetPaletteFromStream(stream, BI.biClrUsed, palEntrySize);

    tmp := nil;
    result := true;
    bmp32.SetSize(BI.biWidth, BI.biHeight, false);

    if validBitFields then
      tmp := GetPixelsFromStreamUsingBitfields(
        stream, bmp32.Width, bmp32.Height, BI.biBitCount, bitfields)

    else if (BI.biBitCount = 32) then
      Read(bmp32.Pixels[0], BI.biWidth * BI.biHeight * sizeof(TColor32))

    else if (BI.biCompression = BI_RLE8) or (BI.biCompression = BI_RLE4) then
      tmp := GetPixelsFromStreamUsingPalAndRleCompression(
        stream, bmp32.Width, bmp32.Height, BI.biBitCount, pal)

    else tmp := GetPixelsFromStreamUsingPalette(
      stream, bmp32.Width, bmp32.Height, BI.biBitCount, pal);

    if assigned(tmp) and (length(tmp) = length(bmp32.Pixels)) then
      move(tmp[0], bmp32.Pixels[0], length(tmp) * sizeof(TColor32));

    if isTopDown then bmp32.FlipVertical;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TBitmap32.RegisterExtension('bmp', TBitmap32Ext_BMP);

end.
