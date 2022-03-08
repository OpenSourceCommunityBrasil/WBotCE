{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit WBotce_Utils;

{$i wbotce.inc}

interface

uses
  Classes, SysUtils, TypInfo, fpjson, jsonparser, jsonscanner, base64, LResources,
  // WbotCE
  WBotce_Model;

function StringToFile(const AString: string; const AFileName: TFileName;
  const ASafely: boolean = True): boolean;

function FileToString(const AFileName: TFileName;
  const ASafely: boolean = True): string;

function JSONParse(const AString: TJSONStringType;
  const ASafely: boolean = True): TJSONData;

procedure JSONToObject(const AJSON: TJSONStringType;
  const AObject: TObject); overload;

procedure JSONToObject(const AJSON: TJSONData;
  const AObject: TObject); overload;

function ObjectToJSON(const AObject: TObject): TJSONData;

procedure Base64ToStream(const AString: string; const AStream: TStream;
  const AMode: TBase64DecodingMode = bdmMIME);

function StreamToBase64(const AStream: TStream): string;

function ResourceToString(const AResourceName: string): string;

function ReplaceVAR(const AScript, AVar, ANewValue: string): string;

function NormalizeString(const AValue: string): string;

function StringToActionType(const AValue: string): TActionType;

function ActionTypeToString(const AValue: TActionType): string;

function OnlyNumber(const AValue: String): String;

function CharIsNum(const C: Char): Boolean;

implementation

function StringToFile(const AString: string; const AFileName: TFileName;
  const ASafely: boolean): boolean;
var
  VStream: TFileStream;
begin
  Result := False;
  try
    VStream := TFileStream.Create(AFileName, fmCreate);
    try
      VStream.Write(Pointer(AString)^, Length(AString));
    finally
      FreeAndNil(VStream);
    end;
    Result := True;
  except
    if (not (ASafely)) then
    begin
      raise;
    end;
  end;
end;

function FileToString(const AFileName: TFileName;
  const ASafely: boolean): string;
var
  VStream: TFileStream;
  VLength: NativeInt;
begin          
  Result := EmptyStr;
  if (FileExists(AFileName)) then
  begin
    try
      VStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      try
        VLength := VStream.Size;
        if (VLength > 0) then
        begin
          SetLength(Result, VLength);
          VStream.Read(Pointer(Result)^, VLength);
        end;
      finally
        FreeAndNil(VStream);
      end;
    except
      if (not (ASafely)) then
      begin
        raise;
      end;
    end;
  end;
end;

function JSONParse(const AString: TJSONStringType;
  const ASafely: boolean): TJSONData;
var
  VParser: TJSONParser;
begin
  Result := nil;
  VParser := TJSONParser.Create(AString, [joUTF8]);
  try
    try
      Result := VParser.Parse;
    except
      FreeAndNil(Result);
      if (not (ASafely)) then
      begin
        raise;
      end;
    end;
  finally
    FreeAndNil(VParser);
  end;
end;

procedure JSONToStrings(const AJSON: TJSONArray; const AStrings: TStrings);
var
  VIndex: NativeInt;
  VJSON: TJSONData;
begin
  if (Assigned(AStrings)) and (Assigned(AJSON)) then
  begin
    AStrings.Clear;
    for VIndex := 0 to (AJSON.Count - 1) do
    begin
      VJSON := AJSON[VIndex];
      if (Assigned(VJSON)) and (VJSON.JSONType = jtString) then
      begin
        AStrings.Add(VJSON.AsString);
      end;
    end;
  end;
end;

procedure JSONToModelList(const AJSON: TJSONArray;
  const AModelList: TModelList);
var
  VIndex: NativeInt;
  VJSON: TJSONData;
  VModel: TModel;
  VModelClass: TModelClass;
begin
  if (Assigned(AModelList)) and (Assigned(AJSON)) then
  begin
    AModelList.Clear;
    VModelClass := AModelList.ItemClass;
    if (Assigned(VModelClass)) then
    begin
      for VIndex := 0 to (AJSON.Count - 1) do
      begin
        VJSON := AJSON[VIndex];
        if (Assigned(VJSON)) and (VJSON.JSONType = jtObject) then
        begin
          VModel := VModelClass.Create; 
          JSONToObject(VJSON, VModel);
          AModelList.Add(VModel);
        end;
      end;
    end;
  end;
end;

procedure JSONToObject(const AJSON: TJSONStringType; const AObject: TObject);
var
  VJSON: TJSONData;
begin
  if (Assigned(AObject)) then
  begin
    VJSON := JSONParse(AJSON);
    if (Assigned(VJSON)) then
    begin
      try
        if (VJSON.JSONType = jtArray) then
        begin
          if (AObject.InheritsFrom(TStrings)) then
          begin
            JSONToStrings(VJSON as TJSONArray, AObject as TStrings);
          end
          else
          if (AObject.InheritsFrom(TModelList)) then
          begin
            JSONToModelList(VJSON as TJSONArray, AObject as TModelList);
          end;
        end
        else
        if (VJSON.JSONType = jtString) then
        begin
          JSONToObject(VJSON.AsString, AObject);
        end
        else
        begin
          JSONToObject(VJSON, AObject);
        end;
      finally
        FreeAndNil(VJSON);
      end;
    end;
  end;
end;

procedure JSONToObject(const AJSON: TJSONData; const AObject: TObject);
var
  VProp: PPropInfo;
  VProps: PPropList;
  VPropsCount: NativeInt;
  VIndex: NativeInt;
  VObject: TObject;
  VJSON: TJSONData;
  VJSONIndex: NativeInt;
begin
  if (Assigned(AObject)) and
    (Assigned(AJSON)) and (AJSON.JSONType = jtObject) then
  begin
    try
      VPropsCount := GetPropList(AObject, VProps);
      for VIndex := 0 to (VPropsCount - 1) do
      begin
        VProp := VProps^[VIndex];
        if (VProp <> nil) and (VProp^.GetProc <> nil) then
        begin
          VJSONIndex := (AJSON as TJSONObject).IndexOfName(VProp^.Name, True);
          if (VJSONIndex > -1) then
          begin
            VJSON := (AJSON as TJSONObject).Items[VJSONIndex];
            if (Assigned(VJSON)) and (VJSON.JSONType <> jtNull) then
            begin
              case VProp^.PropType^.Kind of
                tkAString:
                begin
                  if (VProp^.SetProc <> nil) and
                    (VJSON.JSONType in [jtBoolean, jtNumber, jtString]) then
                  begin
                    SetStrProp(AObject, VProp, VJSON.AsString);
                  end
                  else
                  if (VJSON.JSONType in [jtArray, jtObject]) then
                  begin
                    SetStrProp(AObject, VProp,
                      VJSON.FormatJSON(AsCompressedJSON));
                  end;
                end;
                tkFloat:
                begin               
                  if (VProp^.SetProc <> nil) and
                    (VJSON.JSONType in [jtBoolean, jtNumber]) then
                  begin
                    SetFloatProp(AObject, VProp, VJSON.AsFloat);
                  end;
                end;
                tkInteger:
                begin           
                  if (VProp^.SetProc <> nil) and
                    (VJSON.JSONType in [jtBoolean, jtNumber]) then
                  begin
                    SetOrdProp(AObject, VProp, VJSON.AsInteger);
                  end;
                end;
                tkInt64,
                tkQWord:
                begin 
                  if (VProp^.SetProc <> nil) and
                    (VJSON.JSONType in [jtBoolean, jtNumber]) then
                  begin
                    SetInt64Prop(AObject, VProp, VJSON.AsInt64);
                  end;
                end;
                tkBool:
                begin
                  if (VJSON.JSONType in [jtBoolean, jtNumber]) then
                  begin
                    SetOrdProp(AObject, VProp, Ord(VJSON.AsBoolean));
                  end;
                end;
                tkClass:
                begin
                  VObject := GetObjectProp(AObject, VProp);
                  if (Assigned(VObject)) then
                  begin
                    if (VJSON.JSONType = jtArray) then
                    begin
                      if (VObject.InheritsFrom(TStrings)) then
                      begin
                        JSONToStrings(VJSON as TJSONArray, VObject as TStrings);
                      end
                      else
                      if (VObject is TModelList) then
                      begin
                        JSONToModelList(VJSON as TJSONArray,
                          VObject as TModelList);
                      end;
                    end
                    else
                    if (VJSON.JSONType = jtString) then
                    begin
                      JSONToObject(VJSON.AsString, VObject);
                    end
                    else
                    begin
                      JSONToObject(VJSON, VObject);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeMemAndNil(VProps);
    end;
  end;
end;

function StringsToJSON(const AStrings: TStrings): TJSONArray;
var
  VIndex: NativeInt;
begin
  Result := TJSONArray.Create([]);
  for VIndex := 0 to (AStrings.Count - 1) do
  begin
    Result.Add(AStrings[VIndex]);
  end;
end;

function ModelListToJSON(const AModelList: TModelList): TJSONArray;
var
  VIndex: NativeInt;
begin
  Result := TJSONArray.Create([]);
  for VIndex := 0 to (AModelList.Count - 1) do
  begin
    Result.Add(ObjectToJSON(AModelList[VIndex]));
  end;
end;

function ObjectToJSON(const AObject: TObject): TJSONData;
var
  VProp: PPropInfo;
  VProps: PPropList;
  VPropsCount: NativeInt;
  VIndex: NativeInt;
  VObject: TObject;
begin
  Result := TJSONObject.Create([]);
  if (Assigned(AObject)) then
  begin
    try
      VPropsCount := GetPropList(AObject, VProps);
      for VIndex := 0 to (VPropsCount - 1) do
      begin
        VProp := VProps^[VIndex];
        if (VProp <> nil) and (VProp^.GetProc <> nil) then
        begin
          case VProp^.PropType^.Kind of
            tkAString:
              (Result as TJSONObject).Add(VProp^.Name,
                GetStrProp(AObject, VProp));
            tkFloat:
              (Result as TJSONObject).Add(VProp^.Name,
                GetFloatProp(AObject, VProp));
            tkInteger:
              (Result as TJSONObject).Add(VProp^.Name,
                GetOrdProp(AObject, VProp));
            tkInt64,
            tkQWord:
              (Result as TJSONObject).Add(VProp^.Name,
                GetInt64Prop(AObject, VProp));
            tkBool:
              (Result as TJSONObject).Add(VProp^.Name,
                GetOrdProp(AObject, VProp) <> 0);
            tkClass:
            begin
              VObject := GetObjectProp(AObject, VProp);
              if (Assigned(VObject)) then
              begin
                if (VObject.InheritsFrom(TStrings)) then
                begin
                  (Result as TJSONObject).Add(VProp^.Name,
                    StringsToJSON(VObject as TStrings));
                end
                else
                if (VObject.InheritsFrom(TModelList)) then
                begin
                  (Result as TJSONObject).Add(VProp^.Name,
                    ModelListToJSON(VObject as TModelList));
                end
                else
                begin
                  (Result as TJSONObject).Add(VProp^.Name,
                    ObjectToJSON(VObject));
                end;
              end;
            end;
          end;
        end;
      end
    finally
      FreeMemAndNil(VProps);
    end;
  end;
end;

procedure Base64ToStream(const AString: string; const AStream: TStream;
  const AMode: TBase64DecodingMode);
var
  VDecoder: TBase64DecodingStream;
  VStream: TStringStream;
begin
  if (AString <> EmptyStr) then
  begin
    VStream := TStringStream.Create(AString);
    try
      VDecoder := TBase64DecodingStream.Create(VStream, AMode);
      try
        AStream.CopyFrom(VDecoder, VDecoder.Size);
      finally
        VDecoder.Free;
      end;
    finally
      VStream.Free;
    end;
  end;
end;

function StreamToBase64(const AStream: TStream): string;
var
  VStream: TStringStream;
  VEncoding: TBase64EncodingStream;
begin
  Result := EmptyStr;
  if (Assigned(AStream)) and (AStream.Size > 0) then
  begin
    AStream.Position := 0;
    VStream := TStringStream.Create(EmptyStr);
    try
      VEncoding := TBase64EncodingStream.Create(VStream);
      try
        VEncoding.CopyFrom(AStream, AStream.Size);
      finally
        VEncoding.Free;
      end;
      Result := VStream.DataString;
    finally
      VStream.Free;
    end;
  end;
end;

function ResourceToString(const AResourceName: string): string;
var                                 
  VStream: TStringStream;
  VResource: TLazarusResourceStream;
begin
  Result := EmptyStr;
  if (AResourceName <> EmptyStr) then
  begin                                    
    VStream := TStringStream.Create(EmptyStr);
    try
      try
        VResource := TLazarusResourceStream.Create(AResourceName, nil);
        VResource.SaveToStream(VStream);
        Result := VStream.DataString;
      except
      end;
    finally
      FreeAndNil(VStream);
      FreeAndNil(VResource);
    end;
  end;
end;

function ReplaceVAR(const AScript, AVar, ANewValue: string): string;
var
  VVar: string;
begin
  VVar := AVar;
  Result := VVar;
  if (Length(VVar) > 2) then
  begin
    Result := StringReplace(AScript, Trim(VVar), Trim(ANewValue),
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function NormalizeString(const AValue: string): string;
begin
  Result := StringReplace(AValue, LineEnding, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #$A, '', [rfReplaceAll]);
end;

function StringToActionType(const AValue: string): TActionType;
var
  VType: TActionType;
  VName: string;
begin
  Result := atNone;
  for VType in TActionType do
  begin
    VName := GetEnumName(TypeInfo(TActionType), Ord(VType));
    if (LowerCase(VName) = LowerCase('at' + AValue)) then
    begin
      Result := VType;
      Break;
    end;
  end;
end;

function ActionTypeToString(const AValue: TActionType): string;
var
  VName: string;
begin
  VName := GetEnumName(TypeInfo(TActionType), Ord(AValue));
  Result := Copy(VName, 3, Length(VName) - 2);
end;

{-----------------------------------------------------------------------------
  *** Extraido de Acbrutil ***
  Retorna uma String apenas com os char Numericos contidos em <Value>
 ---------------------------------------------------------------------------- }
function OnlyNumber(const AValue: String): String;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result   := '' ;
  LenValue := Length( AValue ) ;
  For I := 1 to LenValue  do
  begin
     if CharIsNum( AValue[I] ) then
        Result := Result + AValue[I];
  end;
end;

{-----------------------------------------------------------------------------
   *** Extraido de Acbrutil ***
  Retorna <True> se <C> é Númerico
 ---------------------------------------------------------------------------- }
function CharIsNum(const C: Char): Boolean;
begin
  Result := CharInSet( C, ['0'..'9'] ) ;
end;

end.
