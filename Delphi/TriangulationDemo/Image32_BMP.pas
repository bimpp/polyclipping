unit Image32_BMP;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  7 February 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  BMP file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Windows, Math, Image32;

type

  //TImage32Ext_BMP.LoadFromFile() loads correctly all 'good' BMP images
  //in Jason Summers' test suite - see http://entropymine.com/jason/bmpsuite/
  //For notes on RLE bitmap compression, see ...
  //https://docs.microsoft.com/en-us/windows/desktop/gdi/bitmap-compression

  TImage32Ext_BMP = class(TImage32Ext)
  public
    class function LoadFromFile(const filename: string;
      img32: TImage32): Boolean; override;
    class function LoadFromStream(stream: TStream;
      img32: TImage32): Boolean;
    class function SaveToFile(const filename: string;
      img32: TImage32): Boolean; override;
    class procedure SaveToStream(stream: TStream; img32: TImage32);
  end;

implementation

type
  PTriColor32 = ^TTriColor32;
  TTriColor32 = array [0..2] of TColor32;
  TArrayOfByte = array of Byte;

//------------------------------------------------------------------------------
// Loading (reading) BMP images from file ...
//------------------------------------------------------------------------------

function StreamReadPalette(stream: TStream;
  count, size: integer): TArrayOfColor32;
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

function StreamReadImageWithBitfields(stream: TStream; width, height,
  bpp: integer; bitfields: TTriColor32): TArrayOfColor32;
var
  i,j,bytesPerRow, bytesPerPix: integer;
  shift, size: TTriColor32;
  buffer: PByte;
  dstPixel: PColor32;
  b: PCardinal;
begin

  //from the 3 bitfields, get each bit mask offset (shift) and bit mask size
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
  //and the largest possible value for colorXBit.R = (1 shl size[i]) - 1
  //so convert size[x] to the maximum possible value for colorXBit.R ...
  for i := 0 to 2 do size[i] := (1 shl size[i]) - 1;

  //Now to convert colorXBit.R to color32bit.R ...
  //color32bit.R = colorXBit.R * 255 div size[0]

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
          ((((b^ and bitfields[0]) shr shift[0]) * 255 div size[0]) shl 16 +
          (((b^ and bitfields[1]) shr shift[1]) * 255 div size[1]) shl 8 +
          ((b^ and bitfields[2]) shr shift[2]) * 255 div size[2]);
        inc(dstPixel);
        inc(PByte(b), bytesPerPix);
      end;
    end;
  finally
    FreeMem(buffer);
  end;
end;
//------------------------------------------------------------------------------

function StreamReadImageWithPalette(stream: TStream;
  width, height, bpp: integer;
  const palette: TArrayOfColor32): TArrayOfColor32;
var
  i,j, bytesPerRow, palHigh, pxCnt: integer;
  buffer: TArrayOfByte;
  b: PByte;
  dstPixel: PColor32;
  c, shift: byte;
begin
  shift := 8 - bpp;
  bytesPerRow := ((31 + bpp * width) div 32) * 4;
  setLength(Result, width * height);
  palHigh := High(palette);
  SetLength(buffer, bytesPerRow);
  for i := 0 to height -1 do
  begin
    stream.Read(buffer[0], bytesPerRow);
    b := @buffer[0];
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

function StreamReadImageWithRLECompression(stream: TStream;
  width, height, bpp: integer;
  const palette: TArrayOfColor32): TArrayOfColor32;
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
  is4Bit := bpp = 4;
  setLength(Result, width * height);
  buffLen := stream.Size - stream.Position;
  getMem(buffer, buffLen);
  try
    stream.Read(buffer^, buffLen);
    b := buffer;
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
              if w = width then break;
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
              if w = width then break;
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

function IsValidBitFields(const bitFields: TTriColor32): boolean;
begin
  //make sure each color channel has a mask and that they don't overlap ...
  result := (bitFields[0] <> 0) and (bitFields[1] <> 0) and
    (bitFields[2] <> 0) and (bitFields[0] and bitFields[1] = 0) and
    (bitFields[0] and bitFields[2] = 0) and (bitFields[1] and bitFields[2] = 0);
end;
//------------------------------------------------------------------------------

class function TImage32Ext_BMP.LoadFromFile(const filename: string;
  img32: TImage32): Boolean;
var
  fileStream: TFileStream;
begin
  result := false;
  if not FileExists(filename) then Exit;
  fileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(fileStream, img32);
  finally
    fileStream.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImage32Ext_BMP.LoadFromStream(stream: TStream;
  img32: TImage32): Boolean;
var
  palEntrySize: integer;
  BH: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  tmp, pal: TArrayOfColor32;
  bitfields: TTriColor32;
  isTopDown, hasValidBitFields: boolean;
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

    if ((BI.biCompression and BI_BITFIELDS) = BI_BITFIELDS) then
    begin
      stream.Position := 54;
      stream.Read(bitfields[0], Sizeof(TTriColor32));
      hasValidBitFields := IsValidBitFields(bitfields);
      if stream.Position < sizeof(BH) + BI.biSize then
        stream.Position := sizeof(BH) + BI.biSize;
    end else
    begin
      hasValidBitFields := false;
      stream.Position := sizeof(BH) + BI.biSize;
    end;

    if not hasValidBitFields then
    begin
      if BI.biBitCount = 24 then
      begin
        bitfields[0] := $FF shl 16;
        bitfields[1] := $FF shl 8;
        bitfields[2] := $FF;
        hasValidBitFields := true;
      end
      else if BI.biBitCount = 16 then
      begin
        bitfields[0] := $1F shl 10;
        bitfields[1] := $1F shl 5;
        bitfields[2] := $1F;
        hasValidBitFields := true;
      end;
    end;

    if (BI.biClrUsed = 0) and (BI.biBitCount < 16) then
      BI.biClrUsed := Trunc(Power(2, BI.biBitCount));
    if BI.biClrUsed > 0 then
      pal := StreamReadPalette(stream, BI.biClrUsed, palEntrySize);

    tmp := nil;
    result := true;
    img32.SetSize(BI.biWidth, BI.biHeight);

    if hasValidBitFields then
      tmp := StreamReadImageWithBitfields(
        stream, img32.Width, img32.Height, BI.biBitCount, bitfields)

    else if (BI.biBitCount = 32) then
      Read(img32.Pixels[0], BI.biWidth * BI.biHeight * sizeof(TColor32))

    else if (BI.biCompression = BI_RLE8) or (BI.biCompression = BI_RLE4) then
      tmp := StreamReadImageWithRLECompression(
        stream, img32.Width, img32.Height, BI.biBitCount, pal)

    else tmp := StreamReadImageWithPalette(
      stream, img32.Width, img32.Height, BI.biBitCount, pal);

    if assigned(tmp) and (length(tmp) = length(img32.Pixels)) then
      move(tmp[0], img32.Pixels[0], length(tmp) * sizeof(TColor32));

    if isTopDown then img32.FlipVertical;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) BMP images to file ...
//------------------------------------------------------------------------------

function MakeBitfields: TTriColor32;
begin
  result[0] := $FF0000;
  result[1] := $FF00;
  result[2] := $FF;
end;
//------------------------------------------------------------------------------

function GetRowSize(bitCount, imageWidth: integer): integer;
begin
  result := ((31 + BitCount * imageWidth) div 32) * 4;
end;
//------------------------------------------------------------------------------

function IndexOf(sortedList: TList;
  findVal: TColor32; out listPos: integer): Boolean;
var
  min, max, d: integer;
begin
  d := 0; listPos := 0;
  min := 0; max := sortedList.Count -1;
  while (max >= min) do
  begin
    listPos := (min + max) div 2;
    //integer casts are OK here because the high bits will always be clear
    d := Integer(findVal) - Integer(sortedList[listPos]);
    if d = 0 then
    begin
      Result := true;
      Exit;
    end
    else if d > 0 then min := listPos +1
    else max := listPos -1;
  end;
  if (d > 0) then inc(listPos);
  Result := false;
end;
//------------------------------------------------------------------------------

procedure GetPaletteColors(img32: TImage32; palList: TList);
var
  i, idx: integer;
  pc: PColor32;
begin
  if img32.IsEmpty then Exit;
  palList.Clear;
  palList.Capacity := 256;
  pc := img32.PixelBase;
  for i := 0 to img32.Width * img32.Height -1 do
  begin
    if not IndexOf(palList, pc^ and $FFFFFF, idx) then
    begin
      if palList.Count >= 256 then
      begin
        //there are too many colors so ...
        palList.Clear;
        Break;
      end;
      palList.Insert(idx, Pointer(pc^ and $FFFFFF));
    end;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure StreamWriteLoBitImage(img32: TImage32; palList: TList;
  BitCount: integer; stream: TStream);
var
  i, j, k, pxlPerByte, rowSize, delta, shiftSize, totalBytes: integer;
  buffer: TArrayOfByte;
  pSrc: PColor32;
  pDst: PByte;
begin
  pxlPerByte := 8 div BitCount;
  rowSize := GetRowSize(BitCount, img32.Width);
  delta := rowSize - img32.Width div pxlPerByte;
  //delphi doesn't handle modulo of negatives as expected so ...
  shiftSize := img32.Width mod pxlPerByte;
  if shiftSize > 0 then shiftSize := pxlPerByte - shiftSize;
  totalBytes := rowSize * img32.Height;
  SetLength(buffer, totalBytes);
  fillChar(buffer[0], totalBytes, 0);
  pSrc := img32.PixelBase;
  pDst := @buffer[0];
  for i := 1 to img32.Height do
  begin
    k := 0;
    for j := 1 to img32.Width do
    begin
      k := k shl BitCount + palList.IndexOf(Pointer(pSrc^ and $FFFFFF));
      if (j mod pxlPerByte = 0) then
      begin
        pDst^ := k;
        inc(pDst);
        k := 0;
      end;
      inc(pSrc);
    end;
    if shiftSize > 0 then pDst^ := k shl shiftSize;
    inc(pDst, delta);
  end;
  stream.Write(buffer[0], totalBytes);
end;
//------------------------------------------------------------------------------

procedure StreamWrite24BitImage(img32: TImage32; stream: TStream);
var
  i,j, delta, rowSize, totalBytes: integer;
  buffer: TArrayOfByte;
  pc: PColor32;
  pb: PByte;
begin
  rowSize := GetRowSize(24, img32.Width);
  delta := rowSize - (img32.Width *3);
  totalBytes := rowSize * img32.Height;
  setLength(buffer, totalBytes);
  fillChar(buffer[0], totalBytes, 0);
  pb := @buffer[0];
  pc := img32.PixelBase;
  for i := 0 to img32.Height -1 do
  begin
    for j := 0 to img32.Width -1 do
    begin
      Move(pc^, pb^, 3); //ie skipping the alpha byte
      inc(pc); inc(pb, 3);
    end;
    inc(pb, delta);
  end;
  stream.Write(buffer[0], totalBytes); //much faster to do this once
end;
//------------------------------------------------------------------------------

class procedure TImage32Ext_BMP.SaveToStream(stream: TStream;
  img32: TImage32);
var
  BH: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
  i, palCnt, BitCount, rowSize, imageSize: integer;
  UsesAlpha: Boolean;
  palList: TList;
  pals: TArrayOfColor32;
begin
  palList := TList.Create;
  try
    UsesAlpha := img32.UsesAlphaChannel;
    if not UsesAlpha then
    begin
      GetPaletteColors(img32, palList);
      palCnt := palList.Count;
      if palCnt = 0 then BitCount := 24
      else if palCnt > 16 then BitCount := 8
      else if palCnt > 2 then BitCount := 4
      else BitCount := 1;
    end else
    begin
      BitCount := 32;
      palCnt := 0;
    end;

    rowSize := GetRowSize(BitCount, img32.Width);
    imageSize := rowSize * img32.Height;

    FillChar(BH, sizeof(BH), #0);
    BH.bfType := $4D42;
    FillChar(BI, sizeof(BI), #0);
    BI.biSize := sizeof(BI);
    BI.biWidth := img32.Width;
    BI.biHeight := img32.Height;
    BI.biPlanes := 1;
    BI.biBitCount := BitCount;
    BI.biClrUsed := palCnt;
    BI.biCompression := BI_RGB;
    case BitCount of
    1,4,8:
      begin
        BH.bfOffBits := sizeof(BI) + sizeof(BH) + palCnt * 4;
        BH.bfSize := BH.bfOffBits + Cardinal(imageSize);
        BI.biSizeImage := imageSize;
        stream.Write(BH, sizeof(BH));
        stream.Write(BI, sizeof(BI));
        SetLength(pals, palCnt);
        for i := 0 to palCnt -1 do
          pals[i] := TColor32(palList[i]);
        stream.Write(pals[0], palCnt * 4);
        StreamWriteLoBitImage(img32, palList, BitCount, stream);
      end;
    24:
      begin
        BH.bfOffBits := sizeof(BI) + sizeof(BH) + SizeOf(TTriColor32);
        BH.bfSize := BH.bfOffBits + Cardinal(imageSize);
        BI.biSizeImage := imageSize;
        BI.biCompression := BI_BITFIELDS;
        stream.Write(BH, sizeof(BH));
        stream.Write(BI, sizeof(BI));
        stream.Write(MakeBitfields, SizeOf(TTriColor32));
        StreamWrite24BitImage(img32, stream);
      end
    else
      begin
        BH.bfOffBits := sizeof(BI) + sizeof(BH);
        BH.bfSize := BH.bfOffBits + Cardinal(imageSize);
        BI.biSizeImage := imageSize;
        stream.Write(BH, sizeof(BH));
        stream.Write(BI, sizeof(BI));
        stream.Write(img32.Pixels[0], Length(img32.Pixels) * sizeof(TColor32));
      end;
    end;
  finally
    palList.free;
  end;
end;
//------------------------------------------------------------------------------

class function TImage32Ext_BMP.SaveToFile(const filename: string;
  img32: TImage32): Boolean;
var
  stream: TFilestream;
begin
  result := not img32.IsEmpty;
  if not result then Exit;
  stream := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(stream, img32);
  finally
    stream.Free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterExtension('bmp', TImage32Ext_BMP);

end.
