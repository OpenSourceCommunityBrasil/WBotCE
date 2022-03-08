{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

unit WBotce_Model;

{$i wbotce.inc}

interface

uses
  Classes, SysUtils, Graphics, Contnrs, fpjson;

type
  TActionType = (
    atNone,
    // Response
    atGetAllChats, atGetAllContacts, atGetAllGroups, atGetBatteryLevel,
    atGetQrCode, atGetUnreadMessages, atGetMyNumber,
    // Connection
    atConnected, atConnecting, atDisconnected, atDisconnecting, atDestroy,
    atDestroying, atInitializing, atInitialized
  );

  TNotificationEvent = procedure(const ASender: TObject;
    const AAction: TActionType; const AData: string) of object;

  TErrorEvent = procedure(const ASender: TObject; const AError: string;
    const AAdditionalInformation: string) of object;  

  EWBotCE = Exception;

  { TModel }

  TModel = class
  private
    FName: string;
  public
    constructor Create;
    function ActionType: TActionType;
    procedure LoadJSON(const AJSON: string); virtual;
    function SaveJSON(const ABeautiful: boolean = False): string; virtual;
  published
    property Name: string read FName write FName;
  end;  
  TModelClass = class of TModel;

  { TModelList }

  TModelList = class(TModel)
  private
    FItems: TObjectList;
    function GetItem(const AIndex: NativeInt): TModel;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Add(const AItem: TModel);
    procedure Clear;
    function Count: NativeInt;
    function ItemClass: TModelClass; virtual; abstract;
    property Items[const AIndex: NativeInt]: TModel read GetItem; default;
  end;

  { TGModelList }

  generic TGModelList<T: class> = class(TModelList)
  private
    type
      TGModelListEnumerator = class
      private                
        FList: TGModelList;
        FPosition: NativeInt;
        function GetCurrent: T;
      public
        constructor Create(const AList: TGModelList);
        function MoveNext: boolean;
      public
        property Current: T read GetCurrent;
      end;
  private
    function GetItem(const AIndex: NativeInt): T;
  public
    procedure Add(const AItem: T);    
    function GetEnumerator: TGModelListEnumerator;
    function ItemClass: TModelClass; override;
    property Items[const AIndex: NativeInt]: T read GetItem; default;
  end;

  { TResponsel }

  TResponse = class(TModel)
  private
    FResult: string;
  published
    property Result: string read FResult write FResult;
  end;

  { TQrCode }

  TQrCode = class(TModel)
  private
    FQrCode: string;
    FQrCodeImage: TPicture;
  protected
    procedure ProcessQRCodeImage;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property QrCodeImage: TPicture read FQrCodeImage;
  published
    property _QrCode: string read FQrCode write FQrCode;
  end;

  { TResponseQrCode }

  TResponseQrCode = class(TModel)
  private
    FResult: TQrCode;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure LoadJSON(const AJSON: string); reintroduce;
  published
    property Result: TQrCode read FResult;
  end;   

  { TResponseConsole }

  TResponseConsole = class(TResponse)
  end;

  { TResponseBattery }

  TResponseBattery = class(TResponse)
  end;

  { TResponseMyNumber }

  TResponseMyNumber = class(TResponse)
  end;

  { TResponseGroups }

  TResponseGroups = class(TModel)
  private
    FResult: TStringList;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Result: TStringList read FResult;
  end;

  { TContact}

  TContact = class(TModel)
  private
    FFormattedName: string;
    FId: string;
    FIsMe: boolean;
    FIsMyContact: boolean;
    FIsPSA: boolean;
    FIsUser: boolean;
    FIsWAContact: boolean;
    FLabels: TStrings;
    FMsgs: string;
    FPushname: string;
    FSectionHeader: string;
    FStatusMute: boolean;
    FType: String;
    FVerifiedName: string;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property FormattedName: string read FFormattedName write FFormattedName;
    property SectionHeader: string read FSectionHeader write FSectionHeader;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Pushname: string read FPushname write FPushname;
    property VerifiedName: string read FVerifiedName write FVerifiedName;
    property IsUser: boolean read FIsUser write FIsUser;
    property StatusMute: boolean read FStatusMute write FStatusMute;
    property Labels: TStrings read FLabels;
    property IsMe: boolean read FIsMe write FIsMe;
    property IsMyContact: boolean read FIsMyContact write FIsMyContact;
    property IsPSA: boolean read FIsPSA write FIsPSA;
    property IsWAContact: boolean read FIsWAContact write FIsWAContact;
    property Msgs: string read FMsgs write FMsgs;
    property &type: String read FType write FType;
  end;

  { TContactList }

  TContactList = class(specialize TGModelList<TContact>)
  end;

  { TResponseContact }

  TResponseContact = class(TModel)
  private
    FResult: TContactList;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Result: TContactList read FResult;
  end;

  // Forward
  TChat = class;

  { TLastReceivedKey }

  TLastReceivedKey = class(TModel)
  private
    FFromMe: boolean;
    FId: string;
    FRemote: string;
    FSerialized: string;
  published
    property _Serialized: string read FSerialized write FSerialized;
    property FromMe: boolean read FFromMe write FFromMe;
    property Id: string read FId write FId;
    property Remote: string read FRemote write FRemote;
  end;

  { TProfilePicThumbObj }

  TProfilePicThumbObj = class(TModel)
  private
    FEurl: string;
    FId: string;
    FImg: string;
    FImgFull: string;
    FTag: string;
  published
    property EUrl: string read FEurl write FEUrl;
    property Id: string read FId write FId;
    property Img: string read FImg write FImg;
    property ImgFull: string read FImgFull write FImgFull;
    property Tag: string read FTag write FTag;
  end;

  { TSender }

  TSender = class(TModel)
  private
    FFormattedName: string;
    FId: string;
    FIsBusiness: boolean;
    FIsEnterprise: boolean;
    FIsMe: boolean;
    FIsMyContact: boolean;
    FIsPSA: boolean;
    FIsUser: boolean;
    FIsWAContact: boolean;
    FLabels: TStrings;
    FProfilePicThumbObj: TProfilePicThumbObj;
    FPushname: string;
    FStatusMute: boolean;
    FType: String;
    FVerifiedName: string;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property ProfilePicThumbObj: TProfilePicThumbObj read FProfilePicThumbObj;
    property FormattedName: string read FFormattedName write FFormattedName;
    property Id: string read FId write FId;
    property IsBusiness: boolean read FIsBusiness write FIsBusiness;
    property IsEnterprise: boolean read FIsEnterprise write FIsEnterprise;
    property IsMe: boolean read FIsMe write FIsMe;
    property IsMyContact: boolean read FIsMyContact write FIsMyContact;
    property IsPSA: boolean read FIsPSA write FIsPSA;
    property IsUser: boolean read FIsUser write FIsUser;
    property IsWAContact: boolean read FIsWAContact write FIsWAContact;
    property Labels: TStrings read FLabels;
    property Pushname: string read FPushname write FPushname;
    property StatusMute: boolean read FStatusMute write FStatusMute;
    property Name: string read FName write FName;
    property VerifiedName: string read FVerifiedName write FVerifiedName;      
    property &Type: String read FType write FType;
  end;

  { TMediaDataPreview }

  TMediaDataPreview = class(TModel)
  private
    FBody: string;
    Freleased: boolean;
    F_B64: string;
    F_inAutoreleasePool: boolean;
    F_retainCount: NativeInt;
  published
    property _RetainCount: NativeInt read F_retainCount write F_RetainCount;
    property _InAutoreleasePool: boolean
      read F_inAutoreleasePool write F_InAutoreleasePool;
    property Released: boolean read Freleased write Freleased;
    property Body: string read FBody write Fbody;
    property _B64: string read F_B64 write F_B64;
  end;

  { TMediaData_Blob }

  TMediaData_Blob = class(TModel)
  private
    F_Mimetype: string;
    F_Url: string;
  published
     property _Url: string read F_Url write F_Url;
     property _Mimetype: string read F_Mimetype write F_Mimetype;
  end;

  { TMediaDataBlob }

  TMediaDataBlob = class(TModel)
  private
    FReleased: boolean;
    F_Blob: TMediaData_Blob;
    F_InAutoreleasePool: boolean;
    F_RetainCount: NativeInt;
  public
    destructor  Destroy; override;
    procedure AfterConstruction; override;
  published
    property _RetainCount: NativeInt read F_RetainCount Write F_RetainCount;
    property _InAutoreleasePool: boolean
      read F_InAutoreleasePool write F_InAutoreleasePool;
    property Released: boolean read FReleased write Freleased;
    property _Blob: TMediaData_Blob Read F_Blob;
  end;

  { TMediaData }

  TMediaData = class(TModel)
  private
    FanimatedAsNewMsg: boolean;
    FanimationDuration: extended;
    FAspectRatio: extended;
    FFilehash: string;
    FFullHeight: NativeInt;
    FfullWidth: NativeInt;
    FMediaBlob: string;
    FMediaStage: string;
    FMimetype: string;
    FPreview: TMediaDataPreview;
    FSize: extended;
    FType: string;
    F_ListeningToSwSupport: boolean;
    F_SwStreamingSupported: boolean;
  public
     destructor  Destroy; override;
     procedure AfterConstruction; override;
  published
     property &Type: string read FType write FType;
     property MediaStage: string read FMediaStage write FMediaStage;
     property Size: extended read FSize write FSize;
     property Filehash: string read FFilehash write FFilehash;
     property Mimetype: string read FMimetype write FMimetype;
     property MediaBlob: string read FMediaBlob write FMediaBlob;
     property FullHeight: NativeInt read FFullHeight write FFullHeight;
     property FullWidth : NativeInt read FfullWidth write FFullWidth;
     property AspectRatio: extended read FAspectRatio write FaspectRatio;
     property AnimationDuration: extended
       read FanimationDuration write FanimationDuration;
     property AnimatedAsNewMsg: boolean
       read FanimatedAsNewMsg write FanimatedAsNewMsg;
     property _SwStreamingSupported: boolean
       read F_SwStreamingSupported write F_SwStreamingSupported;
     property _ListeningToSwSupport: boolean
       read F_ListeningToSwSupport write F_ListeningToSwSupport;
     property Preview: TMediaDataPreview  Read FPreview write FPreview;
  end;

  { TMessage }

  TMessage = class(TModel)
  private
    FAck: extended;
    FBody: string;
    FBroadcast: Boolean;
    FCaption: string;
    FChat: TChat;
    FChatId: string;
    FClientUrl: string;
    FContent: string;
    FDirectPath: string;
    FFilehash: string;
    FFilename: string;
    FFrom: string;
    FId: string;
    FInvis: boolean;
    FIsForwarded: boolean;
    FIsGroupMsg: boolean;
    FIsMedia: boolean;
    FIsMMS: boolean;
    FIsNewMsg: boolean;
    FIsNotification: Boolean;
    FIsPSA: boolean;
    FLabels: TStrings;
    FLat: extended;
    FLng: extended;
    FMediaData: TMediaData;
    FMediaKey: string;
    FMediaKeyTimestamp: extended;
    FMentionedJidList: TStrings;
    FMimetype: string;
    FNotifyName: string;
    FPageCount: extended;
    FQuotedMsgObj: string;
    FRecvFresh: boolean;
    FSelf: string;
    FSender: TSender;
    FSize: extended;
    FStar: boolean;
    FSubType: string;
    FT: extended;
    FTimestamp: extended;
    FTo: string;
    FType: string;
    FUploadhash: string;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Ack: extended read FAck write FAck;
    property Body: string read FBody write FBody;
    property Broadcast: Boolean read FBroadcast write FBroadcast;
    property Chat: TChat read FChat;
    property ChatId: string read FChatId write FChatId;
    property Caption: string read FCaption write FCaption;
    property Content: string read FContent write FContent;
    property From: string read FFrom write FFrom;
    property Id: string read FId write FId;
    property Invis: boolean read FInvis write FInvis;
    property IsForwarded: boolean read FIsForwarded write FIsForwarded;
    property IsGroupMsg: boolean read FIsGroupMsg write FIsGroupMsg;
    property IsMMS: boolean read FIsMMS write FIsMMS;
    property IsMedia: boolean read FIsMedia write FIsMedia;
    property IsNewMsg: boolean read FIsNewMsg write FIsNewMsg;
    property Lat: extended read FLat write FLat;
    property Lng: extended read FLng write FLng;
    property SubType: string read FSubType write FSubType;
    property IsNotification: Boolean read FIsNotification write FIsNotification;
    property IsPSA: boolean read FIsPSA write FIsPSA;
    property Labels: TStrings read FLabels;
    property MediaData: TMediaData read FMediaData;
    property MentionedJidList: TStrings read FMentionedJidList;
    property NotifyName: string read FNotifyName write FNotifyName;
    property RecvFresh: boolean read FRecvFresh write FRecvFresh;
    property Self: string read FSelf write FSelf;
    property Mimetype: string read FMimetype write FMimetype;
    property Filename: string read FFilename write Ffilename;
    property ClientUrl: string read FClientUrl write FClientUrl;
    property DirectPath: string read FDirectPath write FDirectPath;
    property Filehash: string read FFilehash write FFilehash;
    property Uploadhash: string read FUploadhash write FUploadhash;
    property Size: extended read FSize write FSize;
    property MediaKey: string read FMediaKey write FMediaKey;
    property MediaKeyTimestamp: extended
      read FMediaKeyTimestamp write FMediaKeyTimestamp;
    property PageCount: extended read FPageCount write FPageCount;
    property QuotedMsgObj: string read FQuotedMsgObj write FQuotedMsgObj;
    property Sender: TSender read FSender;
    property Star: boolean read FStar write FStar;
    property T: extended read FT write FT;
    property Timestamp: extended read FTimestamp write FTimestamp;
    property &To: string read FTo write FTo;
    property &Type: string read FType write FType;
  end;

  { TMessageList }

  TMessageList = class(specialize TGModelList<TMessage>)
  end;

  { TChatStates }

  TChatStates = class(TModel)
  private
    FTeste: string;
  published
    property Teste: string read FTeste write FTeste;
  end;

  { TChatStatesList }

  TChatStatesList = class(specialize TGModelList<TChatStates>)
  end;

  { TPresence }

  TPresence = class(TModel)
  private
    FChatstates: TChatStatesList;
    FId: string;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Chatstates: TChatStatesList read FChatstates write FChatstates;
    property Id: string read FId write FId;
  end;     

  { TParticipants }

  TParticipants = class(TModel)
  private
    FId: string;
    FIsAdmin: boolean;
    FIsSuperAdmin: boolean;
  published
    property Id: string read FId write FId;
    property IsAdmin: boolean read FIsAdmin write FIsAdmin;
    property IsSuperAdmin: boolean read FIsSuperAdmin write FIsSuperAdmin;
  end;

  { TParticipantsList }

  TParticipantsList = class(specialize TGModelList<TParticipants>)
  end;

  { TGroupMetadata }

  TGroupMetadata = class(TModel)
  private
    FCreation: extended;
    FDesc: string;
    FDescId: string;
    FDescOwner: string;
    FDescTime: extended;
    FId: string;
    FOwner: string;
    FParticipants: TParticipantsList;
    FPendingParticipants: TParticipantsList;
    FRestrict: boolean;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Creation: extended read FCreation write FCreation;
    property Desc: string read FDesc write FDesc;
    property DescId: string read FDescId write FDescId;
    property DescOwner: string read FDescOwner write FDescOwner;
    property DescTime: extended read FDescTime write FDescTime;
    property Id: string read FId write FId;
    property Owner: string read FOwner write FOwner;
    property Restrict: boolean read FRestrict   write FRestrict;
    property Participants: TParticipantsList read FParticipants;
    property PendingParticipants: TParticipantsList read FPendingParticipants;
  end;

  { TChat }

  TChat = class(TModel)
  private
    FArchive: boolean;
    FContact: TContact;
    FGroupMetadata: TGroupMetadata;
    FId: string;
    FIsAnnounceGrpRestrict: boolean;
    FIsGroup: boolean;
    FIsReadOnly: boolean;
    FKind: String;
    FKindTypeNumber: string;
    FLastReceivedKey: TLastReceivedKey;
    FMessages: TMessageList;
    FModifyTag: Extended;
    FMsgs: String;
    FMuteExpiration: Extended;
    FNotSpam: Boolean;
    FPendingMsgs: Boolean;
    FPin: Extended;
    FPresence: TPresence;
    FT: Extended;
    FUnreadCount: Extended;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property IsAnnounceGrpRestrict: boolean
      read FIsAnnounceGrpRestrict write FIsAnnounceGrpRestrict;
    property GroupMetadata: TGroupMetadata read FGroupMetadata;
    property Archive: boolean read FArchive write FArchive;
    property Contact: TContact read FContact;
    property Id: string read FId write FId;
    property IsGroup: boolean read FIsGroup write FIsGroup;
    property IsReadOnly: boolean read FIsReadOnly write FIsReadOnly;
    property kind: string read FKind write FKind;
    property KindTypeNumber: string read FKindTypeNumber write FKindTypeNumber;
    property LastReceivedKey: TLastReceivedKey read FLastReceivedKey;
    property Messages: TMessageList read FMessages write FMessages;
    property ModifyTag: Extended read FModifyTag write FModifyTag;
    property MuteExpiration: Extended read FMuteExpiration write FMuteExpiration;
    property NotSpam: Boolean read FNotSpam write FNotSpam;
    property PendingMsgs: Boolean read FPendingMsgs write FPendingMsgs;
    property Msgs: String read FMsgs Write FMsgs;
    property Pin: Extended read FPin write FPin;
    property Presence: TPresence read FPresence write FPresence;
    property T: Extended read FT write FT;
    property UnreadCount: Extended read FUnreadCount write FUnreadCount;
  end;

  { TChatList }

  TChatList = class(specialize TGModelList<TChat>)
  end;

  { TResponseChat }

  TResponseChat = class(TModel)
  private
    FResult: TChatList;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    property Result: TChatList read FResult;
  end;

implementation

uses
  // WBotCE
  WBotce_Utils;

{ TModel }

constructor TModel.Create;
begin
  inherited Create;
  FName := EmptyStr;
end;

function TModel.ActionType: TActionType;
begin
  Result := StringToActionType(FName);
end;

procedure TModel.LoadJSON(const AJSON: string);
var
  VJSON: TJSONData;
begin
  VJSON := JSONParse(AJSON);
  if (Assigned(VJSON)) then
  begin
    try
      JSONToObject(VJSON, Self);
    finally
      FreeAndNil(VJSON);
    end;
  end;
end;

function TModel.SaveJSON(const ABeautiful: boolean): string;
var
  VJSON: TJSONData;
begin
  Result := EmptyStr;
  VJSON := ObjectToJSON(Self);
  if (Assigned(VJSON)) then
  begin
    try
      if (ABeautiful) then
      begin
        Result := VJSON.FormatJSON();
      end
      else
      begin
        Result := VJSON.FormatJSON(AsCompressedJSON);
      end;
    finally
      FreeAndNil(VJSON);
    end;
  end;
end;

{ TModelList }

function TModelList.GetItem(const AIndex: NativeInt): TModel;
begin
  Result := TModel(FItems[AIndex]);
end;

destructor TModelList.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TModelList.AfterConstruction;
begin
  inherited AfterConstruction; 
  FItems := TObjectList.Create(True);
end;

procedure TModelList.Add(const AItem: TModel);
begin
  FItems.Add(AItem);
end;

procedure TModelList.Clear;
begin
  FItems.Clear;
end;

function TModelList.Count: NativeInt;
begin
  Result := FItems.Count;
end;

{ TGModelList.TGModelListEnumerator }

function TGModelList.TGModelListEnumerator.GetCurrent: T;
begin
  Result := FList[FPosition];
end;

constructor TGModelList.TGModelListEnumerator.Create(const AList: TGModelList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TGModelList.TGModelListEnumerator.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TGModelList }

function TGModelList.GetItem(const AIndex: NativeInt): T;
begin
  Result := T(inherited Items[AIndex]);
end;

procedure TGModelList.Add(const AItem: T);
begin
  inherited Add(AItem);
end;

function TGModelList.GetEnumerator: TGModelListEnumerator;
begin
  Result := TGModelListEnumerator.Create(Self);
end;

function TGModelList.ItemClass: TModelClass;
begin
  Result := T;
end;

{ TQrCode }

procedure TQrCode.ProcessQRCodeImage;
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    FQrCodeImage.Clear;
    try
      Base64ToStream(Copy(FQrCode, 23, Length(FQrCode)), VStream);
      // Minimum size of an image
      if (VStream.Size > 3000) then
      begin
        VStream.Position := 0;
        FQrCodeImage.LoadFromStream(VStream);
      end;
    except
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

destructor TQrCode.Destroy;
begin           
  FreeAndNil(FQrCodeImage);
  inherited Destroy;
end;

procedure TQrCode.AfterConstruction;
begin         
  inherited AfterConstruction;
  FQrCode := EmptyStr;
  FQrCodeImage := TPicture.Create;
end;

{ TResponseQrCode }

destructor TResponseQrCode.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;

procedure TResponseQrCode.AfterConstruction;
begin
  inherited AfterConstruction; 
  FResult := TQrCode.Create;
end;

procedure TResponseQrCode.LoadJSON(const AJSON: string);
begin
  inherited LoadJSON(AJSON);
  FResult.ProcessQRCodeImage;
end;   

{ TResponseGroups }

destructor TResponseGroups.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;

procedure TResponseGroups.AfterConstruction;
begin
  inherited AfterConstruction;
  FResult := TStringList.Create;
end;

{ TContact }

destructor TContact.Destroy;
begin
  FreeAndNil(FLabels);
  inherited Destroy;
end;

procedure TContact.AfterConstruction;
begin
  inherited AfterConstruction;
  FLabels := TStringList.Create;
end;

{ TResponseContact }

destructor TResponseContact.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;

procedure TResponseContact.AfterConstruction;
begin
  inherited AfterConstruction;
  FResult := TContactList.Create;
end;     

{ TSender }

destructor TSender.Destroy;
begin                              
  FreeAndNil(FLabels);
  FreeAndNil(FProfilePicThumbObj);
  inherited Destroy;
end;

procedure TSender.AfterConstruction;
begin
  inherited AfterConstruction;
  FLabels := TStringList.Create;
  FProfilePicThumbObj := TProfilePicThumbObj.Create;
end;   

{ TMediaDataBlob }

destructor TMediaDataBlob.Destroy;
begin            
  FreeAndNil(F_Blob);
  inherited Destroy;
end;

procedure TMediaDataBlob.AfterConstruction;
begin
  inherited AfterConstruction;
  F_Blob := TMediaData_Blob.Create;
end;

{ TMediaData }

destructor TMediaData.Destroy;
begin                  
  FreeAndNil(FPreview);
  inherited Destroy;
end;

procedure TMediaData.AfterConstruction;
begin
  inherited AfterConstruction;
  FPreview := TMediaDataPreview.Create;
end;

{ TMessage }

destructor TMessage.Destroy;
begin
  FreeAndNil(FChat);
  FreeAndNil(FLabels);
  FreeAndNil(FMentionedJidList);
  FreeAndNil(FSender);
  FreeAndNil(FMediaData);
  inherited Destroy;
end;

procedure TMessage.AfterConstruction;
begin
  inherited AfterConstruction;  
  FChat := TChat.Create;
  FLabels := TStringList.Create;
  FMentionedJidList:= TStringList.Create;
  FSender := TSender.Create;
  FMediaData := TMediaData.Create;
end;         

{ TPresence }

destructor TPresence.Destroy;
begin
  FreeAndNil(FChatstates);
  inherited Destroy;
end;

procedure TPresence.AfterConstruction;
begin
  inherited AfterConstruction;
  FChatstates := TChatStatesList.Create;
end;       

{ TGroupMetadata }

destructor TGroupMetadata.Destroy;
begin
  FreeAndNil(FParticipants);
  FreeAndNil(FPendingParticipants);
  inherited Destroy;
end;

procedure TGroupMetadata.AfterConstruction;
begin
  inherited AfterConstruction;
  FParticipants := TParticipantsList.Create; 
  FPendingParticipants := TParticipantsList.Create;
end;

{ TChat }

destructor TChat.Destroy;
begin
  FreeAndNil(FContact);
  FreeAndNil(FMessages);
  FreeAndNil(FLastReceivedKey);
  FreeAndNil(FPresence);
  FreeAndNil(FGroupMetadata);
  inherited Destroy;
end;

procedure TChat.AfterConstruction;
begin
  inherited AfterConstruction;
  FContact := TContact.Create;
  FMessages := TMessageList.Create;
  FLastReceivedKey := TLastReceivedKey.Create;
  FPresence := TPresence.Create;
  FGroupMetadata := TGroupMetadata.Create;
end;

{ TResponseChat }

destructor TResponseChat.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;

procedure TResponseChat.AfterConstruction;
begin
  inherited AfterConstruction;  
  FResult := TChatList.Create;
end;

end.

