{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit WBotce_Form;

{$i wbotce.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  LResources,
  // CEF
  uCEFWindowParent, uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFApplication,
  // WBotCE
  WBotce_Const, WBotce_Utils, WBotce_Model;

type

  { TWBotCEForm }

  TWBotCEForm = class(TForm)
    ChromiumWindow: TCEFWindowParent;
    Chromium: TChromium;
    QrCodeImg: TImage;
    StatusBar: TStatusBar;
    TimerConnect: TTimer;
    TimerMonitoring: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);  
    procedure FormShow(Sender: TObject);  
    procedure OnTimerConnect(Sender: TObject);
    procedure OnTimerMonitoring(Sender: TObject);
    procedure ChromiumConsoleMessage(Sender: TObject;
      const {%H-}ABrowser: ICefBrowser; {%H-}ALevel: TCefLogSeverity;
      const AMessage, {%H-}ASource: ustring; {%H-}ALine: Integer;
      out {%H-}AResult: Boolean);
    procedure ChromiumTitleChange(Sender: TObject;
      const {%H-}ABrowser: ICefBrowser; const {%H-}ATitle: ustring);
  private
    FJsScript : TStringList;
    FApplicationPath: String;
    FMyNumber: String;
    FAuthenticated: boolean;
    FConected: boolean;
    FBrowser: Boolean;
    FStep: NativeInt;
    FZoom: NativeInt;
    FLastQrCode: string; 
    FMonitorBattery: boolean;
    FMonitorUnreadMsgs: boolean;
    FOnError: TErrorEvent;
    FOnNotification: TNotificationEvent;
  private
    procedure SetZoom(const AValue: NativeInt);
  protected
    procedure CheckCEFApp;
    procedure InternalNotification(const AAction: TActionType;
      const AData: string = '');
    procedure InternalError(const AError: string;
      const AAdditionalInformation: string);
    procedure ExecuteScript(const AScript: string;
      const ADirect: boolean = False);
  protected
    procedure ProcessQrCode(const AData: string);  
    procedure Logout;
  public   
    procedure Connect;
    procedure Disconnect(const ALogout: boolean = False);
  public
    procedure CheckIsValidNumber(APhone: string);
    procedure GetQrCode;
    procedure GetBatteryLevel; 
    procedure GetUnreadMessages;  
    procedure GetAllContacts;
    procedure GetAllGroupContacts(AGroupId: String);
    procedure GetAllGroups;
    procedure GetMyNumber;
    procedure GroupAddParticipant(const AGroupId, ANumber: string);
    procedure GroupRemoveParticipant(const AGroupId, ANumber: string);
    procedure ReadMsg(const ANumber: String);
    procedure SendContact(const ANumber, AContact: string);
    procedure SendMsg(const ANumber, AMsg: string);
    procedure SendButtons(const ANumber, AMsg, AButtons, AFooter: string);
    procedure SendMsgBase64(const ANumber, AMsg, AFileName, ACaption: string);
  public
    property MyNumber: String read FMyNumber write FMyNumber;
    property Conected: boolean read FConected;
    property Authenticated: boolean read FAuthenticated;
    property Browser: Boolean read FBrowser write FBrowser;
    property MonitorBattery: boolean
      read FMonitorBattery write FMonitorBattery;
    property MonitorUnreadMsgs: boolean
      read FMonitorUnreadMsgs write FMonitorUnreadMsgs;   
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnNotification: TNotificationEvent
      read FOnNotification write FOnNotification;
  end;

var
  WBotCEForm: TWBotCEForm;

implementation

{$R *.lfm}

{ TWBotCEForm }

procedure TWBotCEForm.FormCreate(Sender: TObject);
begin
  FApplicationPath:= ExtractFilePath(Application.ExeName);
  FJsScript := TStringList.Create;
  Chromium.DefaultUrl := WBOTCE_WHATSAPP;
  FConected := False;
  FAuthenticated := False;
  FBrowser := True;
  FStep := 0;
  FZoom := 1;
  FLastQrCode := EmptyStr;
  FMonitorBattery := True;
  FMonitorUnreadMsgs := True;
  FOnError := nil;
  FOnNotification:= nil;
end;

procedure TWBotCEForm.FormDestroy(Sender: TObject);
begin
  InternalNotification(atDestroy);
  FreeAndNil(FJsScript);
end;          

procedure TWBotCEForm.FormShow(Sender: TObject);
begin
  Caption := WBOTCE_NAME + ' - ' + WBOTCE_VERSION;
  StatusBar.SimpleText := EmptyStr;
  ChromiumWindow.Visible := False;
  QrCodeImg.Visible := False;
  if (FBrowser) then
  begin                      
    ChromiumWindow.Visible := True;
  end
  else
  begin                            
    QrCodeImg.Visible := True;
    StatusBar.SimpleText := QRCODE_LOADING;
  end;
end;

procedure TWBotCEForm.OnTimerConnect(Sender: TObject);
var
  VStatus: boolean;
  VScript: string;
begin
  TimerConnect.Enabled := False;
  VStatus := True;
  try
    if (FAuthenticated) then
    begin
      // Inject script     
      {$i wbotce.lrs}
      if FileExists(FApplicationPath+'wbotce.js') then
      begin
        FJsScript.LoadFromFile(FApplicationPath+'wbotce.js');
        VScript:= FJsScript.Text;
      end
      else VScript := ResourceToString('wbotce');
      {$IfDef wbotce_debug}
      WriteLn('wbotce script ', VScript);
      {$EndIf}
      ExecuteScript(VScript, True);
      Sleep(50);
      if (Not(FBrowser)) then
      begin
        Hide;
      end;
      VStatus := False;
      InternalNotification(atInitialized);
    end
    else
    begin
      GetQrCode;
    end;
  finally
    TimerConnect.Enabled := VStatus;
  end;
end;

procedure TWBotCEForm.OnTimerMonitoring(Sender: TObject);
begin
  TimerMonitoring.Enabled := False;
  try
    if (FAuthenticated) then
    begin
      if (FMonitorBattery) then
      begin
        GetBatteryLevel;
      end;
      if (FMonitorUnreadMsgs) then
      begin
        GetUnreadMessages;
      end;
      if (FMyNumber = '') then
      begin
        GetMyNumber;
      end;
    end;
  finally
    TimerMonitoring.Enabled := True;
  end;
end;

procedure TWBotCEForm.ChromiumConsoleMessage(Sender: TObject;
  const ABrowser: ICefBrowser; ALevel: TCefLogSeverity;
  const AMessage, ASource: ustring; ALine: Integer; out AResult: Boolean);
var
  VConsole: TResponseConsole;
  VAction: TActionType;
begin
  {$IfDef wbotce_debug}  
  WriteLn(AMessage);
  {$EndIf}
  // Check JSON
  if (Copy(TrimLeft(AMessage), 1, 2) <> '{"') then
  begin
    Exit;
  end;
  VConsole := TResponseConsole.Create;
  try  
    VConsole.LoadJSON(string(AMessage));
    VAction := VConsole.ActionType;
    // Redirect
    case VAction of
      atNone:
      begin
        InternalError(EXCEPT_JS_UNKNOWN, string(AMessage));
      end;  
      atGetQrCode:
      begin
        ProcessQrCode(VConsole.SaveJSON);
        InternalNotification(VAction);
      end;
      else
      begin
        InternalNotification(VAction, VConsole.Result);
      end;
    end;
  finally
    FreeAndNil(VConsole);
  end;
end;

procedure TWBotCEForm.ChromiumTitleChange(Sender: TObject;
  const ABrowser: ICefBrowser; const ATitle: ustring);
begin
  Inc(FStep);
  if (FStep > 3) and (FStep < 10) and (not(FAuthenticated)) then
  begin
    // Established connection   
    FAuthenticated := True;
    InternalNotification(atConnected);
  end;
  if (FStep <= 3) then
  begin
    SetZoom(-2);
  end;
end;    

procedure TWBotCEForm.SetZoom(const AValue: NativeInt);
var
  VIndex: NativeInt;
  VZoom: NativeInt;
begin
  if (FZoom <> AValue) then
  begin
    FZoom := AValue;
    Chromium.ResetZoomStep;
    VZoom := (FZoom * (-1));
    for VIndex := 0 to (VZoom - 1) do
    begin
      Chromium.DecZoomStep;
    end;
  end;
end;

procedure TWBotCEForm.InternalNotification(const AAction: TActionType;
  const AData: string);
begin
  if (Assigned(FOnNotification)) then
  begin
    FOnNotification(Self, AAction, AData);
  end;
end;

procedure TWBotCEForm.InternalError(const AError: string;
  const AAdditionalInformation: string);
begin
  if (Assigned(FOnError)) then
  begin
    FOnError(Self, AError, AAdditionalInformation);
  end;
end;

procedure TWBotCEForm.ExecuteScript(const AScript: string;
  const ADirect: boolean);
begin
  CheckCEFApp;  
  if (not (FConected)) and (not (ADirect))  then
  begin
    raise EWBotCE.Create(EXCEPT_CEF_CONNECT);
  end;
  Chromium.Browser.MainFrame.ExecuteJavaScript(UnicodeString(AScript),
    UnicodeString('about:blank'), 0);
end;

procedure TWBotCEForm.ProcessQrCode(const AData: string);
var
  VQrCode: TResponseQrCode;
begin
  if (FLastQrCode <> AData) then
  begin                  
    FLastQrCode := AData;
    VQrCode := TResponseQrCode.Create;
    try
      VQrCode.LoadJSON(FLastQrCode);
      QrCodeImg.Picture.Assign(VQrCode.Result.QrCodeImage);
      if (Assigned(QrCodeImg.Picture.Graphic))  and
        (not(QrCodeImg.Picture.Graphic.Empty)) then
      begin
        StatusBar.SimpleText := QRCODE_SUCCESS;
      end;
    finally
      FreeAndNil(VQrCode);
    end;
  end;
end;   

procedure TWBotCEForm.Logout;
begin
  ExecuteScript(CMD_LOGOUT);
end;

procedure TWBotCEForm.CheckCEFApp;
begin
  if (not(Assigned(GlobalCEFApp))) or
    (GlobalCEFApp.Status <> asInitialized) then
  begin
    raise EWBotCE.Create(EXCEPT_CEF_APP);
  end;
end;

procedure TWBotCEForm.Connect;
var
  VStart: NativeUInt;
begin
  CheckCEFApp;
  try
    if (not(FConected)) then
    begin
      InternalNotification(atConnecting);
      VStart := GetTickCount64;
      FConected := Chromium.CreateBrowser(ChromiumWindow);
      repeat
        FConected := (Chromium.Initialized);
        if (not (FConected)) then
        begin
          Sleep(10);
          Application.ProcessMessages;
          if ((GetTickCount64 - VStart) > 20000) then
          begin
            Break;
          end;
        end;
      until FConected;
    end;
  finally                                
    TimerConnect.Enabled := FConected;
    TimerMonitoring.Enabled := FConected;
    if (FConected) then
    begin
      Chromium.OnConsoleMessage := @ChromiumConsoleMessage;
      Chromium.OnTitleChange := @ChromiumTitleChange;
      Chromium.Reload;
      Show;
    end
    else
    begin
      InternalNotification(atDisconnected);
      raise EWBotCE.Create(EXCEPT_CEF_CONNECT);
    end;
  end;
end;

procedure TWBotCEForm.Disconnect(const ALogout: boolean);
begin
  if (not(FConected)) then
  begin
    Exit;
  end;
  try
    if (ALogout) then
    begin
      Logout;
      Sleep(50);
    end;
    TimerConnect.Enabled := False;
    TimerMonitoring.Enabled := False;
    Chromium.StopLoad;
    //Chromium.CloseBrowser(True);
    Sleep(600);
    FStep := 0;     
    FLastQrCode := EmptyStr; 
    FAuthenticated:= False;
    FConected := False;     
    Hide;
    InternalNotification(atDisconnected);
  except
  end;
end;

procedure TWBotCEForm.CheckIsValidNumber(APhone: string);
var
  VScript: string;
begin
  VScript := CMD_CHECK_NUMBER_STATUS;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', APhone);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.GetQrCode;
begin
  if (not(FBrowser)) then
  begin
    ExecuteScript(CMD_GET_QRCODE);
  end;
end;

procedure TWBotCEForm.GetBatteryLevel;
begin                                  
  ExecuteScript(CMD_GET_BATTERY_LEVEL);
end;

procedure TWBotCEForm.GetUnreadMessages;
begin
  ExecuteScript(CMD_GET_UNREAD_MESSAGES);
end;

procedure TWBotCEForm.GetAllContacts;
begin
  ExecuteScript(CMD_GET_ALL_CONTACTS);
end;

procedure TWBotCEForm.GetAllGroupContacts(AGroupId: String);
var
  VScript: string;
begin
  VScript := CMD_GET_ALL_GROUP_CONTACTS;
  VScript := ReplaceVAR(VScript, '<#GROUP_ID#>', AGroupId);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.GetAllGroups;
begin
  ExecuteScript(CMD_GET_ALL_GROUPS);
end;

procedure TWBotCEForm.GetMyNumber;
begin
  ExecuteScript(CMD_GET_MYNUMBER);
end;

procedure TWBotCEForm.ReadMsg(const ANumber: String);
var
  VScript: string;
begin
  VScript := CMD_READ_MSG;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', ANumber);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.SendContact(const ANumber, AContact: string);
var
  VScript: string;
begin
  VScript := CMD_SEND_CONTACT;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', ANumber);
  VScript := ReplaceVAR(VScript, '<#CONTACT#>', AContact);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.SendMsg(const ANumber, AMsg: string);
var
  VScript: string;
begin
  VScript := CMD_SEND_MSG;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', ANumber);
  VScript := ReplaceVAR(VScript, '<#MSG#>', AMsg);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.SendButtons(const ANumber, AMsg, AButtons, AFooter: string
  );
var
  VScript: string;
begin
  VScript := CMD_SEND_BUTTONS;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', ANumber);
  VScript := ReplaceVAR(VScript, '<#MSG#>', AMsg);
  VScript := ReplaceVAR(VScript, '<#BUTTONS#>', AButtons);
  VScript := ReplaceVAR(VScript, '<#FOOTER#>', AFooter);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.SendMsgBase64(const ANumber, AMsg, AFileName,
  ACaption: string);    
var
  VScript: string;
begin
  VScript := CMD_SEND_MSG_BASE64;
  VScript := ReplaceVAR(VScript, '<#PHONE#>', ANumber);
  VScript := ReplaceVAR(VScript, '<#MSG#>', AMsg); 
  VScript := ReplaceVAR(VScript, '<#FILENAME#>', AFileName);
  VScript := ReplaceVAR(VScript, '<#CAPTION#>', ACaption);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.GroupAddParticipant(const AGroupId, ANumber: string);
var
  VScript: string;
begin
  VScript := CMD_GROUP_ADD_PARTICIPANT;
  VScript := ReplaceVAR(VScript, '<#GROUP_ID#>', AGroupId);
  VScript := ReplaceVAR(VScript, '<#PARTICIPANT_NUMBER#>', ANumber);
  ExecuteScript(VScript);
end;

procedure TWBotCEForm.GroupRemoveParticipant(const AGroupId, ANumber: string);
var
  VScript: string;
begin
  VScript := CMD_GROUP_REMOVE_PARTICIPANT;
  VScript := ReplaceVAR(VScript, '<#GROUP_ID#>', AGroupId);
  VScript := ReplaceVAR(VScript, '<#PARTICIPANT_NUMBER#>', ANumber);
  ExecuteScript(VScript);
end;

end.
