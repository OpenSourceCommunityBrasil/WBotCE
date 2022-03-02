{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit WBotCE_UtilsImg;

{$mode objfpc}{$H+}

interface
uses
  Messages, SysUtils, Variants, Classes, IniFiles, db, StdCtrls, ExtCtrls,
  {$IfNDef FPC}
    jpeg, Winapi.Windows, Vcl.Graphics
  {$else}
    graphics, LCLType, LCL, LCLClasses
  {$EndIf};

  function TestIsBMP(AStream: TStream): Boolean; overload;
  function TestIsBMP(const AFileName: TFileName): Boolean; overload;
  function TestIsIcon(AStream: TStream): Boolean; overload;
  function TestIsIcon(const AFileName: TFileName): Boolean; overload;
  function TestIsJPEG(const AFileName: TFileName): Boolean; overload;
  function TestIsJPEG(AStream: TStream): Boolean; overload;
  function TestIsPNG(AStream: TStream): Boolean; overload;
  function TestIsPNG(const AFileName: TFileName): Boolean; overload;

implementation

{$HINTS OFF}
function TestIsBMP(AStream: TStream): Boolean;
var
  VSignature: array[0..1] of Char;
  VReadSize: Integer;
  VOldPosition: Int64;
begin
  VOldPosition := AStream.Position;
  try
    VReadSize := AStream.Read(VSignature, SizeOf(VSignature));
    Result := (VReadSize = 2) and (VSignature[0] = 'B') and (VSignature[1] = 'M');
  finally
    AStream.Position := VOldPosition;
  end;
end;
{$HINTS ON}

function TestIsBMP(const AFileName: TFileName): Boolean;
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TestIsBMP(VFile);
  finally
    VFile.Free;
  end;
end;

{$HINTS OFF}
function TestIsIcon(AStream: TStream): Boolean;
const
  CIconSignature: array [0..3] of Char = #0#0#1#0;
var
  VSignature: array[0..3] of Char;
  VReadSize: Integer;
  VOldPosition: Int64;
begin
  VOldPosition := AStream.Position;
  try
    VReadSize := AStream.Read(VSignature, SizeOf(VSignature));
    Result := (VReadSize = SizeOf(VSignature)) and
      CompareMem(@VSignature, @CIconSignature, 4);
  finally
    AStream.Position := VOldPosition;
  end;
end;
{$HINTS ON}

function TestIsIcon(const AFileName: TFileName): Boolean;
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TestIsIcon(VFile);
  finally
    VFile.Free;
  end;
end;

function TestIsJPEG(AStream: TStream): Boolean;
var
  VSOI: Word;
  VOldPosition: Int64;
  {$IfNDef FPC}
    BmpFH: TBitmapFileHeader;
  {$EndIf}
begin
  VOldPosition := AStream.Position;
  try
    VSOI := 0;
    AStream.Read(VSOI, SizeOf(VSOI));
    {$IfNDef FPC}
      Result := (BmpFH.bfType = $D8FF);
    {$ELSE}
      Result := VSOI = NtoLE($D8FF);
    {$EndIf}
  finally
    AStream.Position := VOldPosition;
  end;
end;

function TestIsJPEG(const AFileName: TFileName): Boolean;
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TestIsJPEG(VFile);
  finally
    VFile.Free;
  end;
end;

{$HINTS OFF}
function TestIsPNG(AStream: TStream): Boolean;
const
  CSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
var
  VReadSize: Integer;
  VOldPosition: Int64;
  VSigCheck: array[0..7] of Byte;
begin
  VOldPosition := AStream.Position;
  try
    AStream.Read(VSigCheck, SizeOf(VSigCheck));
    Result := False;
    for VReadSize := 0 to 7 do
      if VSigCheck[VReadSize] <> CSignature[VReadSize] then
        Exit;
    Result := True;
  finally
    AStream.Position := VOldPosition;
  end;
end;
{$HINTS ON}

function TestIsPNG(const AFileName: TFileName): Boolean;
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TestIsPNG(VFile);
  finally
    VFile.Free;
  end;
end;

end.

