# Documentação — wbotce.js
Este arquivo injeta uma camada **WAPI** (`window.WAPI`) sobre a biblioteca **WPP** (WhatsApp Web API) e expõe helpers/monitores para uso via **CEF** (Chromium Embedded Framework) ou console do navegador.
## Versões embutidas (header do arquivo)
- Version_JS: **3.1.4.5**
- Version_WBotCEMin: **0.1.1.2**
- Version_CEF4Min: **120.1.10**
## Conceitos importantes
- **JID**: identificador do WhatsApp (ex.: `5511999999999@c.us`, grupos `...@g.us`).
- **LID**: identificador “migrado” (WhatsApp vem usando em algumas contas). O script tenta mapear **LID ⇄ phoneNumber** via `WPP.contact.getPnLidEntry(...)`.
- Muitas funções retornam **Promise** (async). Em Delphi/CEF, normalmente você chama a função e espera o retorno, **ou** consome o retorno pelo console (quando a função emite mensagens).
## Protocolo de comunicação via console (integração CEF)
O script usa principalmente estas duas funções:

- `SetConsoleMessage(nome, resultValue)`
- `SetConsoleMessageString(nome, stringValue)`

Elas imprimem no console uma string JSON no formato:

```json
{"name":"<nome>","result":"{\"result\":<resultValue>}"}
```

ou (string direta):

```json
{"name":"<nome>","result":"<stringValue>"}
```

Na integração CEF, o host normalmente “escuta” o console e roteia pelo campo `name`.
## Eventos (WPP.on)
O arquivo registra handlers do WPP. Alguns eventos são **sempre emitidos**, outros dependem de flags `habilitaEvento_*` que você ativa chamando `startEvento_*()`.
| Evento (WPP) | Emite no console `name` | Saída | Flag necessária |
|---|---|---|---|
| `call.incoming_call` | `IncomingiCall` | console.log | — |
| `call.outgoing_call` | `OutgoingCall` | console.log | — |
| `conn.authenticated` | `isAuthenticated` | SetConsoleMessage | — |
| `conn.main_loaded` | `isLoaded` | SetConsoleMessage | — |
| `conn.main_ready` | `isReady` | SetConsoleMessage | — |
| `conn.online` | `GetEnvisOnline` | SetConsoleMessage | — |
| `chat.poll_response` | `Getpoll_response` | SetConsoleMessage | — |
| `chat.msg_ack_change` | `Getmsg_ack_change` | SetConsoleMessage | habilitaEvento_msg_ack_change |
| `chat.msg_revoke` | `Getmsg_revoke` | SetConsoleMessage | habilitaEvento_msg_revoke |
| `chat.new_message` | `Getnew_message` | SetConsoleMessage | habilitaEvento_new_message |
| `chat.new_reaction` | `Getnew_reaction` | SetConsoleMessage | habilitaEvento_new_reaction |
| `chat.msg_edited` | `Getmsg_edited` | SetConsoleMessage | — |
| `conn.logout` | `isLogout` | SetConsoleMessage | — |
| `conn.needs_update` | `GetEnvneedsUpdate` | SetConsoleMessage | — |
| `conn.require_auth` | `GetEnvrequire_auth` | SetConsoleMessage | — |
| `conn.qrcode_idle` | `GetEnvqrcode_idle` | SetConsoleMessage | — |
| `chat.presence_change` | `Getpresence_change` | SetConsoleMessage | habilitaEvento_presence_change |
| `chat.update_label` | `Getupdate_label` | SetConsoleMessage | habilitaEvento_update_label |
| `chat.active_chat` | `Getactive_chat` | SetConsoleMessage | habilitaEvento_active_chat |
| `chat.live_location_start` | `Getlive_location_start` | SetConsoleMessage | habilitaEvento_live_location_start |
| `group.participant_changed` | `Getgroup_participant_changed` | SetConsoleMessage | habilitaEvento_group_participant_changed |
| `order.payment_status` | `Getorder_payment_status` | SetConsoleMessage | habilitaEvento_order_payment_status |
| `conn.logout_reason` | `logout_reason` | SetConsoleMessage | — |

### Observações de payload
- `chat.new_message` (quando habilitado) converte a mensagem com `toJSON()` e adiciona campos úteis:
  - `msg.fromJid`, `msg.fromlid`, `msg.toJid`, `msg.tolid`
  - em grupos: `msg.authorJid`, `msg.authorLid`
  - `msg.isGroup` e `msg.formattedTitle`
- `call.incoming_call` e `call.outgoing_call` montam manualmente o JSON e imprimem via `console.log`.
## Flags de eventos
As flags começam como `false` e são ligadas por:

- `startEvento_new_message()`
- `startEvento_msg_ack_change()`
- `startEvento_msg_revoke()`
- `startEvento_new_reaction()`
- `startEvento_presence_change()`
- `startEvento_update_label()`
- `startEvento_active_chat()`
- `startEvento_live_location_start()`
- `startEvento_group_participant_changed()`
- `startEvento_order_payment_status()`
## Monitores
O script tem dois “modos” de monitoramento (legado e novo). Eles geralmente rodam com `setInterval` e retornam via console.
### Legado
- `startMonitor(dias, limit)` — inicia rotina que busca mensagens não lidas (usa `monitorUnReadMessages`).
- `stopMonitor()` — para o monitor.
- `monitorUnReadMessages(dias, limit)` — coleta mensagens não lidas (antigo).
### Novo (recomendado)
- `startMonitorNew(qtdMsg)` — inicia monitor (usa `monitorUnReadMessagesNew`).
- `stopMonitorNew()` — para o monitor.
- `monitorUnReadMessagesNew(qtdMsg)` — lista chats com não-lidas via `window.WAPI.list`.
### Monitor de “crash” WPP
- `startMonitorWPPCrash()` / `monitorWPPCrash()` — tenta detectar falhas e reportar para o host.
## Funções globais (helpers)
Abaixo, as funções declaradas fora da `window.WAPI`.
- **arrayToJson(array)**
- **check()**
- **convertImgToBase64URL(url, callback, outputFormat)**
- **getAllGroupContacts(Contacts)** | console: `GetAllGroupContacts`
- **getMyNumber()**
- **getMyNumberID()**
- **isChatMessage(message)**
- **isGroupJid(jid)**
- **localStorageGetItem(item)** | console: `getMyNumber`
- **localStorageGetItemID(item)**
- **monitorUnReadMessages()** | console: `OnChangeConnect`
- **monitorUnReadMessagesNew()** | console: `OnChangeConnect`
- **monitorWPPCrash()** | console: `WPPCrashMonitor`
- **moveElementsToParentParentElement(elementClass)**
- **removeElementsByClass(elementClass)**
- **removeMsgsProperty(array)**
- **replacer(key, value)**
- **resolveChatIdFromQuery(validarObj, fallbackWid)**
- **SetConsoleMessage(jsName, resultValue)**
- **SetConsoleMessageString(jsName, StringValue)**
- **sleep(ms)**
- **startEvento_active_chat(hab_active_chat = false)**
- **startEvento_group_participant_changed(hab_group_participant_changed = false)**
- **startEvento_live_location_start(hab_live_location_start = false)**
- **startEvento_msg_ack_change(hab_msg_ack_change = false)**
- **startEvento_msg_revoke(hab_msg_revoke = false)**
- **startEvento_new_message(hab_new_message = false)**
- **startEvento_new_reaction(hab_new_reaction = false)**
- **startEvento_order_payment_status(hab_order_payment_status = false)**
- **startEvento_presence_change(hab_presence_change = false)**
- **startEvento_update_label(hab_update_label = false)**
- **startMonitor(intervalSeconds = 0)**
- **startMonitorNew(intervalSecondsNew = 0)**
- **startMonitorWPPCrash(intervalSeconds = 0)**
- **stopMonitor()**
- **stopMonitorNew()**

### Notas
- Existem definições duplicadas de `sleep`, `check` e `resolveChatIdFromQuery` no arquivo; em JavaScript, **a última definição sobrescreve as anteriores**.
## WAPI (`window.WAPI`) — referência
A maioria das funções é um wrapper do WPP e/ou faz serialização para retornar objetos “seguros” para o host.

Legenda:
- **async**: retorna Promise
- **sync**: retorna direto
- `console: <name>`: além do retorno, também emite para o host via console

### Conexão & Estado do WhatsApp
- **getBatteryLevel(done)** — sync | aceita callback `done` | console: `getBatteryLevel`
- **getHistorySyncProgress2()** — async | console: `getHistorySyncProgress`
- **GetisOnline2()** — async | console: `GetisOnline`
- **GetneedsUpdate2()** — async | console: `GetneedsUpdate`
- **getWAVersion()** — sync | console: `getWAVersion`
- **isBeta()** — async
- **isConnected(done)** — sync | aceita callback `done` | console: `GetCheckIsConnected`
- **isLoggedIn(done)** — sync | aceita callback `done`

### Perfil/Conta
- **getMe()** — async | console: `GetMe`
- **getMessageACK2(uniqueID)** — async | console: `getMessageACK`
- **getMessageById(id, done)** — sync | aceita callback `done`
- **getMessageById2(UniqueID)** — async | console: `ErrorResponse`
- **getMessages(chatid, options)** — async | console: `getMessages`
- **getProfilePicFromId(id, done)** — sync | aceita callback `done`
- **getProfilePicFromServer(id)** — sync
- **getProfilePicSmallFromId(id, done)** — sync | aceita callback `done`
- **setMyName(newName)** — async

### Contatos & Perfil
- **CheckNumberExists(chatid)** — async | console: `CheckNumberExists`
- **checkNumberStatus(id)** — async | console: `NewCheckIsValidNumber`
- **contactBlock(id, done)** — sync | aceita callback `done`
- **contactUnblock(id, done)** — sync | aceita callback `done`
- **genLinkDeviceCodeForPhoneNumber2(chatid)** — async | console: `GetgenLinkDeviceCodeForPhoneNumber`
- **getAllContacts(done)** — async | aceita callback `done` | console: `getAllContacts`
- **getContact(id, done)** — sync | aceita callback `done`
- **getLastSeen(chatid)** — async | console: `getLastSeen`
- **getMyContacts(done)** — async | aceita callback `done` | console: `getMyContacts`
- **getStatus(id)** — async | console: `GetStatusMessage`
- **isValidNumber(phoneId)** — async
- **sendContact(to, contact)** — async
- **sendVCardContactMessage2Ex(chatid, contacts, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `ErrorResponse`
- **setMyStatus(newStatus)** — sync

### Chats & Estados
- **clearChat(id)** — async
- **deleteConversation(chatId)** — async
- **getAllChatIds(done)** — sync | aceita callback `done`
- **getAllChats(done)** — async | aceita callback `done` | console: `getAllChats`
- **getAllChatsWithNewMsg(done)** — sync | aceita callback `done`
- **getAllMessageIdsInChat(id, includeMe, includeNotifications, done)** — sync | aceita callback `done`
- **getAllMessagesInChat(id, includeMe, includeNotifications, done)** — sync | aceita callback `done`
- **getChat(id)** — sync
- **getChatById(id, done)** — sync | aceita callback `done`
- **getChatByName(name, done)** — sync | aceita callback `done`
- **getUnreadMessages(includeMe, includeNotifications, use_unread_count, done)** — async | aceita callback `done` | console: `getUnreadMessages`
- **getUnreadMessagesInChat(id, includeMe, includeNotifications, done)** — sync | aceita callback `done`
- **sendChatstate(state, chatId)** — async
- **sendSeen(id, done)** — sync | aceita callback `done`

### Mensagens & Envio
- **areAllMessagesLoaded(id, done)** — sync | aceita callback `done`
- **asyncLoadAllEarlierMessages(id, done)** — sync | aceita callback `done`
- **deleteMessage(chatId, messageArray, revoke = false, done)** — async | aceita callback `done`
- **getAllNewMessages()** — async
- **getBufferedNewMessages(done)** — sync | aceita callback `done`
- **getNewMessageId(chatId)** — sync
- **getPlatformFromMessage2(UniqueID, chatid)** — async | console: `getPlatformFromMessage`
- **loadAllEarlierMessages(id, done)** — sync | aceita callback `done`
- **loadEarlierMessages(id, done)** — sync | aceita callback `done`
- **loadEarlierMessagesTillDate(id, lastMessage, done)** — sync | aceita callback `done`
- **processMessageObj(messageObj, includeMe, includeNotifications)** — sync | console: `processMessageObj`
- **ReplyMessage(idMessage, message, done)** — sync | aceita callback `done`
- **sendButtons(chatId, title, buttons, description = '')** — async
- **sendCreatePollMessage2Ex(chatid, name, choices, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `sendCreatePollMessageEx`
- **sendFileMessage2Ex(chatid, content, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `ErrorResponse`
- **sendImage(imgBase64, chatid, filename, caption)** — async
- **sendImageFromDatabasePicBot(picId, chatId, caption)** — sync
- **sendLinkWithAutoPreview(chatId, url, text)** — async
- **sendListMenu(to, title, subTitle, description, buttonText, menu)** — async
- **sendListMessage2(chatid, options)** — async | console: `ErrorResponse`
- **sendListMessage2Ex(chatid, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `ErrorResponse`
- **sendLocation(chatId, lat, lng, loc)** — async
- **sendLocation2(chatid, options)** — async | console: `ErrorResponse`
- **sendLocationMessage2Ex(chatid, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `ErrorResponse`
- **sendMessage(id, message, done)** — sync | aceita callback `done`
- **sendMessage2(id, message, done)** — sync | aceita callback `done`
- **sendMessageOptions(chatId, content, options = {})** — async
- **sendMessageToID(chatid, msgText)** — async
- **sendMessageToID2(id, msgText)** — sync
- **sendMessageWithThumb(thumb, url, title, description, text, chatId)** — sync
- **sendTextMessage2(chatid, content, options)** — async | console: `ErrorResponse`
- **sendTextMessage2Ex(chatid, content, options, xSeuID = '', xSeuID2 = '', xSeuID3 = '', xSeuID4 = '')** — async | console: `ErrorResponse`
- **sendVCard(chatId, contactNumber, contactName)** — async
- **waitNewMessages(rmCallbackAfterUse = true, done)** — sync | aceita callback `done`

### Mídia & Conteúdos
- **base64ImageToFile(b64Data, filename)** — sync
- **downloadFile(url, done)** — sync | aceita callback `done`
- **downloadFileWithCredentials(url, done)** — sync | aceita callback `done`
- **list()** — async | console: `getList`

### Grupos & Comunidades
- **addParticipant(idGroup, idParticipant)** — async
- **createGroup(name, contactsId)** — async
- **demoteParticipant(idGroup, idParticipant)** — async
- **getAllCommunitys(done)** — async | aceita callback `done` | console: `getAllcommunity`
- **getAllGroupMetadata(done)** — sync | aceita callback `done`
- **getAllGroups(done)** — async | aceita callback `done` | console: `getAllGroups`
- **getAllGroupsList(done)** — async | aceita callback `done` | console: `getAllGroups`
- **getAllParticipantsGroup(groupID)** — async | console: `GetAllParticipantsGroup`
- **getCommonGroups(id, done)** — async | aceita callback `done`
- **getGroupAdmins(id, done)** — async | aceita callback `done` | console: `getAllGroupAdmins`
- **getGroupInviteLink(chatId)** — async | console: `GetGroupInviteLink`
- **getGroupMetadata(id)** — async
- **getGroupOwnerID(id, done)** — async | aceita callback `done` | console: `getGroupOwnerID`
- **getGroupParticipantIDs(id, done)** — async | aceita callback `done`
- **getInviteCode2(GROUP_ID)** — async | console: `GetGroupInviteLink`
- **joinGroupViaLink(link)** — async | console: `GetGroupInviteLink`
- **leaveGroup(groupId)** — sync
- **promoteParticipant(idGroup, idParticipant)** — async
- **removeParticipant(idGroup, idParticipant)** — async
- **revokeGroupInviteLink(chatId)** — async

### Internos/Serialização
- **_getGroupParticipants(id)** — async
- **_serializeChatObj(obj)** — async
- **_serializeContactObj(obj)** — async
- **_serializeMessageObj(obj)** — sync
- **_serializeNumberStatusObj(obj)** — sync
- **_serializeNumberStatusObjMD(obj)** — sync
- **_serializeProfilePicThumb(obj)** — async
- **_serializeRawObj(obj)** — sync
- **_unloadInform(event)** — sync

### Outros
- **getGeneratedUserAgent(useragent)** — sync
- **GetisLidMigrated()** — async | console: `GetisLidMigrated`
- **getNewId()** — sync
- **GetPnLidEntry(chatid)** — async | console: `GetPnLidEntry`
- **getVotes(uniqueID)** — async | console: `getVotes`
- **haveNewMsg(chat)** — sync
- **ProductCatalog()** — async | console: `ProductCatalog`
- **quickClean(ob)** — sync
- **teste(url)** — async | console: `GetProfilePicThumb`

