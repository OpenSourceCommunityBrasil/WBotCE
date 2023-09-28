{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit wbotce_core;

{$i wbotce.inc}

interface

uses
  Classes, SysUtils, StrUtils, LResources,
  //WBot
  WBotce_Model, WBotce_Form;

type            
  TRequestChatEvent = procedure(const ASender: TObject;
    const AChats: TResponseChat) of object;
  TRequestContactsEvent = procedure(const ASender: TObject;
    const AContacts: TResponseContact) of object;  
  TRequestGroupContactsEvent = procedure(const ASender: TObject;
    const AContacts: TResponseGroupContacts) of object;
  TRequestGroupsEvent = procedure(const ASender: TObject;
    const AGroups: TResponseGroups) of object;
  TRequestCheckIsValidNumberEvent = procedure(const ASender: TObject;
    const APhone: String; AIsValid: Boolean) of object;

  { TWBotCE }

  TWBotCE = class(TComponent)
  private
    FMyNumber: String;
    FBrowser: Boolean;
    FLowBatteryLevel: NativeInt;
    FMonitorBattery: boolean;
    FMonitorUnreadMsgs: boolean;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnLowBatteryLevel: TNotifyEvent;
    FOnMyNumber: TNotifyEvent;
    FOnNotification: TNotificationEvent; 
    FOnRequestChat: TRequestChatEvent;
    FOnRequestContact: TRequestContactsEvent;
    FOnRequestGroupContacts: TRequestGroupContactsEvent;
    FOnRequestCheckIsValidNumber: TRequestCheckIsValidNumberEvent;
    FForm: TWBotceForm;
    FOnRequestGroups: TRequestGroupsEvent;
    FVersion: string;
    function GetAuthenticated: boolean;
    function GetConected: boolean;
    procedure SetUnreadMsgs(const AValue: boolean);
  protected
    property Console: TWBotceForm read FForm;
    procedure InternalError(const ASender: TObject; const AError: string;
      const AAdditionalInformation: string);  
    procedure InternalNotification(const ASender: TObject;
      const AAction: TActionType; const AData: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect(const ALogout: boolean = False);
    procedure CheckIsValidNumber(APhone: string);
    procedure GetAllContacts;
    procedure GetAllGroupContacts(AGroupId: String);
    procedure GetAllGroups;
    procedure GetBatteryLevel;
    procedure GetMyNumber;
    procedure GetUnreadMessages;
    procedure GroupAddParticipant(AGroupId, APhone: String);
    procedure GroupRemoveParticipant(AGroupId, APhone: String);
    procedure ReadMsg(const APhone: String);
    procedure SendContact(const APhone, AContact: string);
    procedure SendFile(const APhone, ACaption, AFileName: string); overload;
    procedure SendFile(const APhone, ACaption, AFileName: string; AStream: TStream); overload;
    procedure SendMsg(const APhone, AMsg: string);
    procedure SendButtons(const APhone, AMsg, AButtons, AFooter: string);
    procedure ClearChat(const APhoneOrGroupId: string);
  public
    Property  MyNumber : String Read FMyNumber;
    property Authenticated: boolean read GetAuthenticated;
    property Conected: boolean read GetConected;
    // Events
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnNotification: TNotificationEvent
      read FOnNotification write FOnNotification;
  published
    property Browser: Boolean read FBrowser write FBrowser default True;
    property LowBatteryLevel: NativeInt
      read FLowBatteryLevel write FLowBatteryLevel default 10;
    property MonitorBattery: boolean
      read FMonitorBattery write FMonitorBattery default True;
    property MonitorUnreadMsgs: boolean
      read FMonitorUnreadMsgs write SetUnreadMsgs default True;
    property Version: string read FVersion;
    // Events
    property OnConnected: TNotifyEvent
      read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent
      read FOnDisconnected write FOnDisconnected; 
    property OnLowBatteryLevel: TNotifyEvent
      read FOnLowBatteryLevel write FOnLowBatteryLevel;
    property OnMyNumber: TNotifyEvent
      read FOnMyNumber write FOnMyNumber;
    property OnRequestChat: TRequestChatEvent
      read FOnRequestChat write FOnRequestChat;
    property OnRequestContact: TRequestContactsEvent
      read FOnRequestContact write FOnRequestContact;
    property OnRequestGroupContacts: TRequestGroupContactsEvent
      read FOnRequestGroupContacts write FOnRequestGroupContacts;
    property OnRequestGroups: TRequestGroupsEvent
      read FOnRequestGroups write FOnRequestGroups;
    property OnRequestCheckIsValidNumber: TRequestCheckIsValidNumberEvent
      read FOnRequestCheckIsValidNumber write FOnRequestCheckIsValidNumber;
  end;

procedure Register;

implementation

uses
  WBotce_Const, WBotce_Utils;

procedure Register;
begin        
  {$i wbotce.lrs}
  RegisterComponents('WBotCE', [TWBotCE]);
end;

{ TWBotCE }

function TWBotCE.GetAuthenticated: boolean;
begin        
  if (csDesigning in ComponentState) then
  begin
    Exit;
  end;
  Result := FForm.Authenticated;
end;

function TWBotCE.GetConected: boolean;
begin
  if (csDesigning in ComponentState) then
  begin
    Exit;
  end;
  Result := FForm.Conected;
end;

procedure TWBotCE.SetUnreadMsgs(const AValue: boolean);
begin
  if (FMonitorUnreadMsgs<>AValue) then
  begin
    FMonitorUnreadMsgs:=AValue;    
    if (not(csDesigning in ComponentState)) then
    begin
      FForm.MonitorUnreadMsgs:=FMonitorUnreadMsgs;
    end;
  end;
end;

procedure TWBotCE.InternalError(const ASender: TObject; const AError: string;
  const AAdditionalInformation: string);
begin
  if (Assigned(FOnError)) then
  begin
    FOnError(ASender, AError, AAdditionalInformation);
  end;
end;

procedure TWBotCE.InternalNotification(const ASender: TObject;
  const AAction: TActionType; const AData: string);
var                              
  VResponseChat: TResponseChat;
  VResponseContact: TResponseContact;   
  VResponseGroups: TResponseGroups;
  VResponseGroupContacts: TResponseGroupContacts;
  VResponseBattery: TResponseBattery;
  VResponseMyNumber: TResponseMyNumber;
  VResponseCheckIsValidNumber: TResponseCheckIsValidNumber;
begin

  if (Assigned(FOnNotification)) then
  begin
    FOnNotification(ASender, AAction, AData);
  end;

  case AAction of
    atConnected:
    begin
      if (Assigned(FOnConnected)) then
      begin
        FOnConnected(Self);
      end;
    end;

    atDisconnected:
    begin
      if (Assigned(FOnDisconnected)) then
      begin
        FOnDisconnected(Self);
      end;
    end;

    atGetBatteryLevel:
    begin
      if (Assigned(FOnLowBatteryLevel)) then
      begin
        VResponseBattery := TResponseBattery.Create;
        try
          {$IfDef wbot_debug}
          WriteLn('ResponseBattery: '+AData);
          {$EndIf}
          VResponseBattery.LoadJSON(AData);
          if (VResponseBattery.Result <> 'undefined') and (VResponseBattery.Result <> '') then
          begin
            if (StrToIntDef(VResponseBattery.Result, 0) <= FLowBatteryLevel) then
            begin
              FOnLowBatteryLevel(Self);
            end;
          end;
        finally
          FreeAndNil(VResponseBattery);
        end;
      end;
    end;

    atGetAllContacts:
    begin
      if (Assigned(FOnRequestContact)) then
      begin
        VResponseContact := TResponseContact.Create;
        try
          VResponseContact.LoadJSON(AData);
          FOnRequestContact(Self, VResponseContact);
        finally
          FreeAndNil(VResponseContact);
        end;
      end;
    end;

    atGetAllGroupContacts:
    begin
      if (Assigned(FOnRequestGroupContacts)) then
      begin
        VResponseGroupContacts := TResponseGroupContacts.Create;
        try
          VResponseGroupContacts.LoadJSON(AData);
          FOnRequestGroupContacts(Self, VResponseGroupContacts);
        finally
          FreeAndNil(VResponseGroupContacts);
        end;
      end;
    end;

    atGetAllGroups:
    begin   
      if (Assigned(FOnRequestGroups)) then
      begin
        VResponseGroups := TResponseGroups.Create;
        try
          VResponseGroups.LoadJSON(AData);
          FOnRequestGroups(Self, VResponseGroups);
        finally
          FreeAndNil(VResponseGroups);
        end;
      end;
    end;

    atGetAllChats,
    atGetUnreadMessages:
    begin
      if (Assigned(FOnRequestChat)) then
      begin
        VResponseChat := TResponseChat.Create;
        try
          VResponseChat.LoadJSON(AData);
          FOnRequestChat(Self, VResponseChat);
        finally
          FreeAndNil(VResponseChat);
        end;
      end;
    end;

    atGetMyNumber:
    begin
      VResponseMyNumber := TResponseMyNumber.Create;
      try
        VResponseMyNumber.LoadJSON(AData);
        FMyNumber:= OnlyNumber(VResponseMyNumber.Result);
        FForm.MyNumber:= FMyNumber;
        if (Assigned(FOnMyNumber)) then
          FOnMyNumber(Self);
      finally
        FreeAndNil(VResponseMyNumber);
      end;
    end;

    atNewCheckIsValidNumber:
    begin
      if (Assigned(FOnRequestCheckIsValidNumber)) then
      begin
        VResponseCheckIsValidNumber := TResponseCheckIsValidNumber.Create;
        try
          VResponseCheckIsValidNumber.LoadJSON(AData);
          FOnRequestCheckIsValidNumber(Self, VResponseCheckIsValidNumber.Result.Id , VResponseCheckIsValidNumber.Result.Valid);
        finally
          FreeAndNil(VResponseCheckIsValidNumber);
        end;
      end;
    end;
  end;
end;

constructor TWBotCE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);    
  if (not(csDesigning in ComponentState)) then
  begin
    FForm := TWBotCEForm.Create(Self);  
    FForm.OnError := @InternalError;
    FForm.OnNotification := @InternalNotification;
    FForm.MyNumber:= '';
  end;
  FBrowser := True;
  FLowBatteryLevel := 10;
  FMonitorBattery := True;
  FMonitorUnreadMsgs:= True;
  FVersion := WBOTCE_VERSION;
end;

destructor TWBotCE.Destroy;
begin
  inherited Destroy;
end;

procedure TWBotCE.Connect;
begin
  FForm.Browser := FBrowser;
  FForm.MonitorBattery := FMonitorBattery;
  FForm.MonitorUnreadMsgs := FMonitorUnreadMsgs;
  FForm.Connect;
end;

procedure TWBotCE.Disconnect(const ALogout: boolean);
begin
  FForm.Disconnect(ALogout);
end;

procedure TWBotCE.CheckIsValidNumber(APhone: string);
begin
  FForm.CheckIsValidNumber(APhone);
end;

procedure TWBotCE.GetAllContacts;
begin
  FForm.GetAllContacts;
end;

procedure TWBotCE.GetAllGroupContacts(AGroupId: String);
begin
  FForm.GetAllGroupContacts(AGroupId);
end;

procedure TWBotCE.GetAllGroups;
begin                 
  FForm.GetAllGroups;
end;

procedure TWBotCE.GetBatteryLevel;
begin               
  FForm.GetBatteryLevel;
end;

procedure TWBotCE.GetMyNumber;
begin
  FForm.GetMyNumber;
end;

procedure TWBotCE.GetUnreadMessages;
begin                  
  FForm.GetUnreadMessages;
end;      

procedure TWBotCE.ReadMsg(const APhone: String);
begin
  {$ifdef wbot_debug}
    WriteLn('Debug: '+APhone);
  {$endif}
  FForm.ReadMsg(APhone);
end;

procedure TWBotCE.SendContact(const APhone, AContact: string);
begin                      
  // TODO: Check phone structure
  FForm.SendContact(APhone, AContact);
end;

procedure TWBotCE.SendFile(const APhone, ACaption, AFileName: string);
var
  VStream: TStream;
begin
  // TODO: Check phone structure
  if (not(FileExists(AFileName))) then
  begin
    InternalError(Self, Format(EXCEPT_FILE_NOFOUND, [AFileName]), '');
    Exit;
  end;
  VStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SendFile(APhone, ACaption, AFileName, VStream);
  finally
    FreeAndNil(VStream);
  end;
end;


procedure TWBotCE.SendFile(const APhone, ACaption, AFileName: string; AStream: TStream);
var
  VFileBase64: string;
  VFileName: string;
  VFileExt: string;
  VMsg: string;
  VLog: TStringList;
begin
  try
    VLog:= TStringList.Create;
    VFileBase64 := StreamToBase64(AStream);
    VFileName := ExtractFileName(AFileName);
    VFileExt := Copy(LowerCase(ExtractFileExt(AFileName)), 2, 3);

    {$IfDef wbot_debug}
    WriteLn('File ext: '+VFileExt);
    {$EndIf}

    if AnsiIndexStr(VFileExt, ['mp3', 'ogg']) > -1 then
      VMsg := 'data:audio/'
    else
    if AnsiIndexStr(VFileExt, ['avi', 'mpeg']) > -1 then
      VMsg := 'data:video/'
    else
    if (AnsiIndexStr(VFileExt, ['jpg', 'jpeg', 'tif', 'ico', 'bmp', 'png', 'raw']) > -1) then
      VMsg := 'data:image/'
    else
      VMsg := 'data:application/';

    VMsg := VMsg + VFileExt + ';base64,' + VFileBase64;

    // Send
    {$IfDef wbot_debug}
    VLog.Add(VMsg);
    VLog.SaveToFile('LogFile.Txt');
    WriteLn(VMsg);
    {$EndIf}
    FForm.SendMsgBase64(APhone, VMsg, VFileName, ACaption);
  finally
    FreeAndNil(VLog);
  end;
end;

procedure TWBotCE.SendMsg(const APhone, AMsg: string);
begin
  // TODO: Check phone structure
  FForm.SendMsg(APhone, AMsg);
end;

procedure TWBotCE.SendButtons(const APhone, AMsg, AButtons, AFooter: string);
begin
  // TODO: Check phone structure
  FForm.SendButtons(APhone, AMsg, AButtons, AFooter);
end;

procedure TWBotCE.ClearChat(const APhoneOrGroupId: string);
begin
  // TODO: Check phone structure
  FForm.ClearChat(APhoneOrGroupId);
end;

procedure TWBotCE.GroupAddParticipant(AGroupId, APhone: String);
begin
  // TODO: Check phone structure
  FForm.GroupAddParticipant(AGroupId, APhone);
end;

procedure TWBotCE.GroupRemoveParticipant(AGroupId, APhone: String);
begin
  // TODO: Check phone structure
  FForm.GroupRemoveParticipant(AGroupId, APhone);
end;

end.

