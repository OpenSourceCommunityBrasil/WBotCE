unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, StrUtils, WBotce_Core, WBotce_Model, WBotce_Utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnAdicionarContato: TButton;
    Button2: TButton;
    ButtonCarregaContatos: TButton;
    ButtonCarregaGrupos: TButton;
    ButtonConecta: TButton;
    ButtonEnviarMsgFile: TButton;
    ButtonEnviarMsgFileGrupo: TButton;
    ButtonLimparChatGrupo: TButton;
    ButtonEnviarMsgText: TButton;
    ButtonEnviarMsgButton: TButton;
    ButtonEnviarMsgTextGrupo: TButton;
    ButtonValidarNumero: TButton;
    ButtonLimparChat: TButton;
    ChkRespostaAutomatica: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    EditGrupo: TEdit;
    EdtAddContato: TEdit;
    EditNumero: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelStatus: TLabel;
    LabelCtName: TLabel;
    LabelCtId: TLabel;
    ListBoxContatos: TListBox;
    ListBoxGrupoContatos: TListBox;
    ListBoxGrupos: TListBox;
    MemoLog: TMemo;
    MemoMsgTxt: TMemo;
    MemoMsgTxtGrupo: TMemo;
    MemoRecebida: TMemo;
    MemoEnviada: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControlMsg: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    WBotCE1: TWBotCE;
    procedure BtnAdicionarContatoClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonCarregaGruposClick(Sender: TObject);
    procedure ButtonCarregaContatosClick(Sender: TObject);
    procedure ButtonConectaClick(Sender: TObject);
    procedure ButtonEnviarMsgButtonClick(Sender: TObject);
    procedure ButtonEnviarMsgFileClick(Sender: TObject);
    procedure ButtonEnviarMsgFileGrupoClick(Sender: TObject);
    procedure ButtonEnviarMsgTextGrupoClick(Sender: TObject);
    procedure ButtonEnviarMsgTextClick(Sender: TObject);
    procedure ButtonLimparChatClick(Sender: TObject);
    procedure ButtonLimparChatGrupoClick(Sender: TObject);
    procedure ButtonValidarNumeroClick(Sender: TObject);
    procedure ChkRespostaAutomaticaChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxContatosDblClick(Sender: TObject);
    procedure ListBoxGruposDblClick(Sender: TObject);
    procedure WBotCE1Connected(Sender: TObject);
    procedure WBotCE1Disconnected(Sender: TObject);
    procedure WBotCE1LowBatteryLevel(Sender: TObject);
    procedure WBotCE1MyNumber(Sender: TObject);
    procedure WBotCE1RequestChat(const Sender: TObject;
      const AChats: TResponseChat);
    procedure WBotCE1RequestCheckIsValidNumber(const ASender: TObject;
      const ANumber: String; AResult: Boolean);
    procedure WBotCE1RequestContact(const Sender: TObject;
      const AContacts: TResponseContact);
    procedure WBotCE1RequestGroupContacts(const ASender: TObject;
      const AContacts: TResponseGroupContacts);
    procedure WBotCE1RequestGroups(const Sender: TObject;
      const AGroups: TResponseGroups);
  private
    procedure AddLog(const AStr: String);
    procedure Limpar;
    procedure RespostaAutomatica(const AFoneId, AMsgRecebida: String);
  public
  end;

var
  Form1: TForm1;

implementation

uses DateUtils, Clipbrd;

{$R *.lfm}

function ExtractBetween(const AValue, ADelimiterA, ADelimiterB: string): string;
var
  VPositonA: NativeInt;
  VPositonB: NativeInt;
begin
  Result := EmptyStr;
  VPositonA := Pos(ADelimiterA, AValue);
  if (VPositonA > 0) then
  begin
    VPositonA := VPositonA + Length(ADelimiterA);
    VPositonB := PosEx(ADelimiterB, AValue, VPositonA);
    if (VPositonB > 0) then
    begin
      Result := Copy(AValue, VPositonA, VPositonB-VPositonA);
    end;
  end;
end;

{ TForm1 }

procedure TForm1.ButtonConectaClick(Sender: TObject);
begin
  if (WBotCE1.Conected) then
  begin
    WBotCE1.Disconnect;
    Limpar;
  end
  else
  begin
    WBotCE1.MonitorBattery := CheckBox3.Checked;
    WBotCE1.Browser := CheckBox2.Checked;
    WBotCE1.Connect;
  end;
end;

procedure TForm1.ButtonEnviarMsgButtonClick(Sender: TObject);
const
  Buttons = '[{buttonId: "id1", buttonText:{displayText: "OPTION1"}, type: 1}, {buttonId: "id2", buttonText: {displayText: "OPTION2"}, type: 1}]';
  Footer = 'Choose option';
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.SendButtons(EditNumero.Text, NormalizeString(MemoMsgTxt.Text), Buttons, Footer);
  end;
end;

procedure TForm1.ButtonEnviarMsgTextClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.SendMsg(EditNumero.Text, NormalizeString(MemoMsgTxt.Text));
  end;
end;

procedure TForm1.ButtonLimparChatClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WbotCE1.ClearChat(EditNumero.Text);
  end;
end;

procedure TForm1.ButtonLimparChatGrupoClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WbotCE1.ClearChat(EditGrupo.Text);
  end;
end;

procedure TForm1.ButtonValidarNumeroClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.CheckIsValidNumber(EditNumero.Text);
  end;
end;

procedure TForm1.ButtonEnviarMsgFileClick(Sender: TObject);
begin
  if WBotCE1.Conected and OpenDialog1.Execute then
  begin
    WBotCE1.SendFile(EditNumero.Text, NormalizeString(MemoMsgTxt.Text), OpenDialog1.FileName);
  end;
end;

procedure TForm1.ButtonEnviarMsgFileGrupoClick(Sender: TObject);
begin
  if WBotCE1.Conected and OpenDialog1.Execute then
  begin
    WBotCE1.SendFile(EditGrupo.Text, NormalizeString(MemoMsgTxtGrupo.Text), OpenDialog1.FileName);
  end;
end;

procedure TForm1.ButtonEnviarMsgTextGrupoClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.SendMsg(EditGrupo.Text, NormalizeString(MemoMsgTxtGrupo.Text));
  end;
end;

procedure TForm1.ChkRespostaAutomaticaChange(Sender: TObject);
begin
  WBotCE1.MonitorUnreadMsgs:=ChkRespostaAutomatica.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Limpar;
end;

procedure TForm1.ListBoxContatosDblClick(Sender: TObject);
begin
  EditNumero.Text:= ExtractBetween(ListBoxContatos.GetSelectedText, '(', ')');
end;

procedure TForm1.ListBoxGruposDblClick(Sender: TObject);
var
  VText: string;
  VIndex: NativeInt;
begin
  VText := ListBoxGrupos.GetSelectedText;
  VIndex := Pos('@g.us', VText);
  if (VIndex > 0) then
  begin
    EditGrupo.Text := Copy(VText, 0, Pos('@g.us', VText) + 4);

    if WBotCE1.Conected then
    begin
      WBotCE1.GetAllGroupContacts(Copy(VText, 1, VIndex + 4));
    end;
  end;
end;

procedure TForm1.ButtonCarregaContatosClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.GetAllContacts;
  end;
end;

procedure TForm1.ButtonCarregaGruposClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.GetAllGroups;
  end;
end;

procedure TForm1.BtnAdicionarContatoClick(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.GroupAddParticipant(EditGrupo.Text, EdtAddContato.Text);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if WBotCE1.Conected then
  begin
    WBotCE1.GroupRemoveParticipant(EditGrupo.Text, ListBoxGrupoContatos.GetSelectedText);
  end;
end;

procedure TForm1.WBotCE1Connected(Sender: TObject);
begin
  LabelStatus.Color:=clGreen;
  LabelStatus.Caption:='Conectado'; 
  ButtonConecta.Caption:='Desconectar';
  AddLog('Conectado');
end;

procedure TForm1.WBotCE1Disconnected(Sender: TObject);
begin
  Label6.Caption:= '';
  LabelStatus.Color:=clRed;
  LabelStatus.Caption:='Desconectado';
  ButtonConecta.Caption:='Conectar';
  AddLog('Desconectado');
end;

procedure TForm1.WBotCE1LowBatteryLevel(Sender: TObject);
begin
  AddLog('Bateria baixa...');
end;

procedure TForm1.WBotCE1MyNumber(Sender: TObject);
begin
  Label6.Caption:= 'Número Logado '+ WBotCE1.MyNumber;
end;

procedure TForm1.WBotCE1RequestChat(const Sender: TObject;
  const AChats: TResponseChat);
var
  VChat: TChat;
  VMsg: TMessage;
  Dt: TDateTime;
begin
  for VChat in AChats.Result do
  begin
    if (Assigned(VChat)) and (not(VChat.IsGroup)) then
    begin
      for VMsg in VChat.Messages do
      begin
        if (Assigned(VMsg)) and (not(VMsg.FromMe)) then
        begin
          LabelCtId.Caption:=VChat.Contact.Id;
          LabelCtName.Caption:=VChat.Contact.Name;

          Dt := VMsg.Timestamp/(SecsPerDay*1000.0);

          MemoRecebida.Text:=VMsg.Content + ' [' + DateTimeToStr(Dt) + '] - ' + VMsg.Chat.Contact.Id;

          if ChkRespostaAutomatica.Checked then
            RespostaAutomatica(VChat.Contact.Id, VMsg.Content);
        end;
      end;
    end;
  end;
end;

procedure TForm1.WBotCE1RequestCheckIsValidNumber(const ASender: TObject;
  const ANumber: String; AResult: Boolean);
begin
  if AResult then
    ShowMessage('Número válido (' + ANumber + ')' + '[' + ANumber.Substring(2).Replace('@c.us', '') + ']')
  else
    ShowMessage('Número inválido ' + ANumber);
end;

procedure TForm1.WBotCE1RequestContact(const Sender: TObject;
  const AContacts: TResponseContact);
Var
  VContact: TContact;
begin
  for VContact in AContacts.Result do
  begin
    if (Assigned(VContact)) then
    begin
      ListBoxContatos.Items.Add(VContact.Name + ' (' + VContact.Id +')');
    end;
  end;
end;

procedure TForm1.WBotCE1RequestGroupContacts(const ASender: TObject;
  const AContacts: TResponseGroupContacts);
begin
  ListBoxGrupoContatos.Items.Assign(AContacts.Result);

  Clipboard.AsText := AContacts.Result.Text;
end;

procedure TForm1.WBotCE1RequestGroups(const Sender: TObject;
  const AGroups: TResponseGroups);
begin
  ListBoxGrupos.Items.Assign(AGroups.Result);
end;

procedure TForm1.AddLog(const AStr: String);
begin
  MemoLog.Lines.Add(AStr);
  MemoLog.SelLength:=Length(MemoLog.Text);
end;

procedure TForm1.Limpar;
begin  
  PageControl1.ActivePage := TabSheet1;
  PageControlMsg.ActivePage := TabSheet2;
  LabelStatus.Color := clRed;
  LabelStatus.Font.Color := clWhite;
  LabelStatus.Font.Style := LabelStatus.Font.Style + [fsBold];
  ListBoxContatos.Clear;
  EditNumero.Text:= '';
  MemoMsgTxt.Text:= 'Olá';
  ListBoxGrupos.Clear;
  EditGrupo.Text:= '';
  MemoMsgTxtGrupo.Text:= 'Olá';
end;

procedure TForm1.RespostaAutomatica(const AFoneId, AMsgRecebida: String);
const
  CMSGDEF = 'Você esta recebendo uma mensagem automatica ao escolher a opção 1!';
  CMSGPER = 'Olá, este é um teste de mensagem automatica feito com WBot 1.0'+'\n'+
            'Digite a opção *1*.';
begin
  //Ler mensagem
  WBotCE1.ReadMsg(AFoneId);
  if AMsgRecebida <> '1' then
  begin
    MemoEnviada.Text:=CMSGPER;
    WBotCE1.SendMsg(AFoneId, CMSGPER);
  end
  else
  begin
    MemoEnviada.Text:=CMSGDEF;
    WBotCE1.SendMsg(AFoneId, CMSGDEF);
  end;
end;

end.

