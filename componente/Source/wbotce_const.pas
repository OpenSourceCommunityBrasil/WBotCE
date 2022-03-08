{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit WBotce_Const;

{$i wbotce.inc}

interface

uses
  Classes, SysUtils;

const
  WBOTCE_NAME = 'WBOTCE';  
  WBOTCE_VERSION = '0.1.1.1';   
  WBOTCE_WHATSAPP = 'https://web.whatsapp.com/';
  WBOTCE_INI = 'wbotce.ini';

  CMD_SEND_CHAT_STATE =
    'Store.WapQuery.sendChatstateComposing("<#PHONE#>");';  
  CMD_SEND_CONTACT
    = 'window.WAPI.sendContact("<#PHONE#>", "<#CONTACT#>")';
  CMD_SEND_MSG =
    'window.WAPI.sendMessageToID("<#PHONE#>","<#MSG#>")';  
  CMD_SEND_MSG_BASE64 =
    'window.WAPI.sendImage("<#MSG#>", "<#PHONE#>", "<#FILENAME#>", "<#CAPTION#>")';
  CMD_GET_QRCODE =
    'var _qrCode = document.getElementsByTagName("canvas")[0].toDataURL("image/png");' + LineEnding +
    'console.log(JSON.stringify({"name":"getQrCode","result":{_qrCode}}));';
  CMD_GET_BATTERY_LEVEL =
    'window.WAPI.getBatteryLevel();';
  CMD_GET_UNREAD_MESSAGES =
    'window.WAPI.getUnreadMessages(includeMe="True", includeNotifications="True", use_unread_count="True");';
  CMD_GET_ALL_GROUPS =
    'window.WAPI.getAllGroups();';
  CMD_GET_ALL_CONTACTS =
    'window.WAPI.getAllContacts();';
  CMD_GET_ALL_CHATS =
    'window.WAPI.getAllChats();';  
  CMD_LOGOUT =
    'localStorage.clear(); location.reload();';

  CMD_READ_MSG =
    'window.WAPI.sendSeen("<#PHONE#>")';

  CMD_GET_MYNUMBER=
    'getMyNumber();';

resourcestring
  EXCEPT_CEF_APP =
    'Component GlobalCEFApp has not been initialized in your application';
  EXCEPT_CEF_BROWSER =
    'Error creating browser in CEF';
  EXCEPT_CEF_CONNECT =
    'You are not connected to the service server';
  EXCEPT_CEF_VERSION =
    'Your CEF4 version is not compatible, please update your library at https://github.com/salvadordf/CEF4Delphi'
    + LineEnding + 'Required version: %s' + LineEnding + 'Identified version: %s';

  EXCEPT_JS_UNKNOWN = 'Unknown wbotce.js return';

  EXCEPT_FILE_NOFOUND = '%s file not found';

  QRCODE_LOADING =  'Loading QRCode...';
  QRCODE_SUCCESS = 'Point your phone now!';

implementation

end.

