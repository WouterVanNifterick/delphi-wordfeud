(*
 *                         Delphi Wordfeud Library
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Embarcadero Technologies Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 * Unit owner : Wouter van Nifterick <woutervannifterick@gmail.com>
 * Web site   : http://code.google.com/p/delphi-wordfeud/
 *
 * Inspired by PHP Wordfeud API (https://github.com/iostream3/PHP-Wordfeud-API)
 *
 *)

unit WvN.WordFeud;

interface

uses
  Generics.Collections, SuperObject{, Graphics};

type
  TRuleSet = (American=0,Norwegian=1,Dutch=2,Danish=3,Swedish=4,English=5,Spanish=6,French=7);
  TBoardType = (Normal=0,Random=1);
const
  cBoardTypeStr: array[TBoardType] of String = ('normal','random');

type
  TUserId=int64;
  TGameId=int64;

  TBoardLayout=record
    type
      TBoardColor=(Black,DarkBlue,DarkGreen,DarkCyan,DarkRed,DarkMagenta,DarkYellow,Gray,DarkGray,Blue,Green,Cyan,Red,Magenta,Yellow,White);
      TBoardLocationType                               = (__,DL,TL,DW,TW,Stone);
    const
      BGColors:array[TBoardLocationType] of TBoardColor= (DarkBlue , DarkGreen,DarkCyan  ,DarkRed ,DarkYellow,White  );
      FGColors:array[TBoardLocationType] of TBoardColor= (Black    , Gray     ,Gray      ,Gray    ,Gray      ,Black  );
      BGColorsWin:array[TBoardLocationType] of Cardinal= ($02B2E35 , $739D6B  , $1B8998  ,$B67420 ,$9C481A   ,$ffffff);
      FGColorsWin:array[TBoardLocationType] of cardinal= ($0       , $dddddd  , $dddddd  , $dddddd, $dddddd  , 0     );
      Texts      :array[TBoardLocationType] of String  = (''       , 'DL'     ,'TL'      ,'DW'    ,'TW'      ,''     );

      DefaultWordFeudLayout:
        Array[0..14,0..14] of TBoardLocationType =
        (
          (TL,__,__,__,TW,__,__,DL,__,__,TW,__,__,__,TL),
          (__,DL,__,__,__,TL,__,__,__,TL,__,__,__,DL,__),
          (__,__,DW,__,__,__,DL,__,DL,__,__,__,DW,__,__),
          (__,__,__,TL,__,__,__,DW,__,__,__,TL,__,__,__),
          (TW,__,__,__,DW,__,DL,__,DL,__,DW,__,__,__,TW),
          (__,TL,__,__,__,TL,__,__,__,TL,__,__,__,TL,__),
          (__,__,DL,__,DL,__,__,__,__,__,DL,__,DL,__,__),
          (DL,__,__,DW,__,__,__,__,__,__,__,DW,__,__,DL),
          (__,__,DL,__,DL,__,__,__,__,__,DL,__,DL,__,__),
          (__,TL,__,__,__,TL,__,__,__,TL,__,__,__,TL,__),
          (TW,__,__,__,DW,__,DL,__,DL,__,DW,__,__,__,TW),
          (__,__,__,TL,__,__,__,DW,__,__,__,TL,__,__,__),
          (__,__,DW,__,__,__,DL,__,DL,__,__,__,DW,__,__),
          (__,DL,__,__,__,TL,__,__,__,TL,__,__,__,DL,__),
          (TL,__,__,__,TW,__,__,DL,__,__,TW,__,__,__,TL)
        );


  end;


type
  TRack=TArray<Char>;

  TLoginResponse=record
    username: string;
    cookies: boolean;
    id: TUserId;
    pontiflex_weight: integer;
    banner: string;
    email: string;
    procedure FromJSON(obj:ISuperObject);
    function AsJSON:ISuperObject;
  end;

  TUser = record
    UserName:String;
    UserId:String;
  end;

  TUserList=TArray<TUser>;



  TChatMessage=record
    Message:String;
  end;

  TChatMessageList=TArray<TChatMessage>;

  TNotification=record
    Message:string;
  end;
  TNotificationList=TArray<TNotification>;

  TWfDateTime=Double;

  TMoveType = (move);
const
  cMoveTypeStr : array[TMoveType] of string = ('move');

type
  TLetterOnBoard=record
    X,Y:Byte;
    Letter:Char;
    Blank:Boolean;
    procedure FromJSON(obj:ISuperObject);
  end;

  TMoveLetters = record
    Letters:TArray<TLetterOnBoard>;
    function ToString:String;
  end;

  TMove = record
    move_type: TMoveType;
    user_id:TUserId;
    move:TMoveLetters;
    points: Integer;
    main_word: String;
    procedure FromJSON(obj:ISuperObject);
  end;

  TPlayer = record
    username: string;
    position: byte;
    score: integer;
    id: TUserId;
    avatar_updated: TWfDateTime;
    Rack:array of Char;
    procedure FromJSON(obj:ISuperObject);
  end;

  TTiles=TArray<TLetterOnBoard>;

  TGame = record
    updated:TWfDateTime;
    is_running: Boolean;
    end_game: integer;
    id: TGameId;
    current_player: Integer;
    created: TWfDateTime;
		chat_count:Integer;
    board:TBoardType;
	  move_count: Integer;
    bag_count:Integer;
		ruleset:TRuleSet;
    pass_count:Integer;
    last_move:TMove;
    players:Array[0..1]of TPlayer;
    Tiles : TTiles;
    private
      function GetPlayer1:TPlayer;
      function GetPlayer2:TPlayer;
      procedure FromJSON(obj:ISuperObject);
    function GetMe: TPlayer;
    function GetOpponent: TPlayer;
    public
      property Player1:TPlayer read GetPlayer1;
      property Player2:TPlayer read GetPlayer2;
      property Me:TPlayer read GetMe;
      property Opponent:TPlayer read GetOpponent;
  end;

  TGameList = TArray<TGame>;

  TBoardId = int64;
  TBoard = record
    procedure FromJSON(obj:ISuperObject);
  end;

  TCredentials = record
    Email,
    UserId,
    Password:String;
  private
    function GetPasswordHash: String;
  public
    property PasswordHash:String read GetPasswordHash;
  end;

  TWordFeud=record
    private
      FSessionId:String;
      FLoginResponse:TLoginResponse;
      debugMode:Boolean;
      acceptEncoding:Boolean;
      FGames:TGameList;
      FCredentials:TCredentials;
      function Execute(URL: String; Data: TDictionary<String, String>):ISuperObject;
      property SessionId:String read FSessionId write FSessionId;
    public
      constructor Create(aSession_id:string; aAccept_encoding:Boolean = true; aDebug_mode :Boolean = false);
      procedure LogOut;
      procedure LogInUsingEmail(aEmail:String; aPassword:String);
      procedure LogInUsingEmail2(aEmail:String; aPasswordHash:String);
      function LogInUsingId(aId:String; aPassword:String):TLoginResponse;
      function SearchUser(aQuery:String):TUserList;
      function GetChatMessages(aGameId:TGameId):TChatMessageList;
      function SendChatMessage(aGameID:TGameId; aMessage:String):ISuperObject;
      function InviteRandomOpponent(Ruleset:TRuleSet;Board:TBoardType):ISuperObject;
      function GetAvatarURL(UserId:TUserId;Size:Integer):String;
      function CreateAccount(aUserName,aEmail,aPassword:String):TUserId;
      function GetNotifications:TNotificationList;
      function GetStatus:ISuperObject;
      function GetGames:TGameList;
      function GetGame(aGameId:TGameId):TGame;
      function GetBoard(BoardId:TBoardId):TBoard;
      procedure AcceptInvite(InviteId:string);
      procedure RejectInvite(InviteId:string);
      procedure ChangePassword(NewPassword:String);
      property MyCredentials:TCredentials read FCredentials;
      property MyUserName:String read FLoginResponse.username;
      property MyId:TUserId read FLoginResponse.id;
      property MyBanner:String read FLoginResponse.banner;
      property MyEmail:String read FLoginResponse.email;
      property MyPassword:String read FCredentials.password;
      property Games:TGameList read FGames write FGames;
  end;

var
  ctx: TSuperRttiContext;

implementation

uses
  WvN.Crypt.SHA1,
  idHTTP,
  idStream,
  IdCookieManager,
  IdURI,
  Classes,
  SysUtils;

const cServerURL = 'http://game06.wordfeud.com';

{ TLoginResponse }

procedure TLoginResponse.FromJSON(obj: ISuperObject);
begin
  Self := ctx.AsType<TLoginResponse>(obj);
end;

{ TWordFeud }

constructor TWordFeud.Create(aSession_id: string; aAccept_encoding, aDebug_mode: Boolean);
begin
  if aSession_id<>'' then
    self.sessionId := aSession_id;
  acceptEncoding := aAccept_encoding;
  debugMode := aDebug_mode;
end;


function TWordFeud.Execute(URL: String; Data: TDictionary<String, String>):ISuperObject;
var
  i :Integer;
  HTTP:TIdHTTP;
  Obj:ISuperObject;
  RBody: TStringStream;
  JSonStr:String;
  ResultStr:string;
  Item:TPair<String,String>;
begin
  obj := TSuperObject.Create;
  Result := TSuperObject.Create;

  if not Assigned(Data) then
    JSonStr := ''
  else
  begin
    for Item in Data do
      obj.S[Item.Key] := Item.Value;
    JSonStr := obj.AsJSon;
  end;

  HTTP := TIdHTTP.Create(nil);
  try
    HTTP.Request.Accept := 'application/json';
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.UserAgent := 'PHP Wordfeud API 0.2';
    HTTP.AllowCookies:=true;
    HTTP.HandleRedirects := true;
    HTTP.CookieManager := TIdCookieManager.Create(HTTP);

    if sessionId<>'' then
      HTTP.Request.CustomHeaders.AddValue('Cookie','sessionid='+sessionId);

    if sessionId<>'' then
      HTTP.CookieManager.AddServerCookie('sessionid='+sessionId,  TIdURI.Create('/') );

    HTTP.Request.URL := cServerURL + '/wf/'+url+'/';
    HTTP.Request.Method := Id_HTTPMethodPost;
    RBody := TStringStream.Create(JsonStr);
    try
      ResultStr := HTTP.Post(HTTP.Request.URL,  RBody);
      for I := 0 to HTTP.CookieManager.CookieCollection.Count-1 do
      begin
        if HTTP.CookieManager.CookieCollection[i].CookieName = 'sessionid' then
          sessionId := HTTP.CookieManager.CookieCollection[i].Value;
      end;
      Result := SO(ResultStr);
    finally
      RBody.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TWordFeud.logInUsingEmail(aEmail, aPassword: String);
begin
  FCredentials.Email := aEmail;
  FCredentials.Password := aPassword;
  LogInUsingEmail2(aEmail, FCredentials.GetPasswordHash);
end;

procedure TWordFeud.LogInUsingEmail2(aEmail, aPasswordHash: String);
var
  ReqResult:ISuperObject;
  url:String;
  data:TDictionary<String,String>;
begin
  FCredentials.Email := aEmail;
  url := 'user/login/email';
  data := TDictionary<String,String>.Create;
  data.Add('email',aEmail);
  data.Add('password',aPasswordHash);
  ReqResult := Execute(url, data);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type']);

  FLoginResponse.FromJSON(ReqResult.O['content']);
end;

function TWordFeud.logInUsingId(aId, aPassword: String):TLoginResponse;
var
  ReqResult:ISuperObject;
  url:String;
  data:TDictionary<String,String>;
begin
  FCredentials.UserId := aId;
  FCredentials.Password := aPassword;

  url := 'user/login/id';
  data := TDictionary<String,String>.Create;
  data.Add('id',aId);
  data.Add('password',FCredentials.GetPasswordHash);
  ReqResult := Execute(url, data);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type']);

  result.username         := ReqResult.S['content.username'];
  result.cookies          := ReqResult.S['content.cookies']='True';
  result.id               := ReqResult.I['content.id'];
  result.pontiflex_weight := ReqResult.I['content.pontiflex_weight'];
  result.banner           := ReqResult.S['content.banner'];
  result.email            := ReqResult.S['content.email'];

  FLoginResponse := Result;
end;

procedure TWordFeud.LogOut;
begin
  FSessionId := '';
end;

function TWordFeud.SearchUser(aQuery:String):TUserList;
var
  url : String;
  data:TDictionary<String,String>;
  ReqResult:ISuperObject;
  i:Integer;
  UserList:TSuperArray;

begin
  url := 'user/search';
  data := TDictionary<String,String>.Create;
  data.Add('username_or_email',aQuery);
  ReqResult := Execute(url, data);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
  begin
    UserList := ReqResult['content.result'].AsArray;
    SetLength(result, UserList.Length);
    for I := 0 to UserList.Length-1 do
    begin
      Result[I].UserName := UserList.O[I].S['username'];
      Result[I].UserId := UserList.O[I].S['user_id'];
    end;
  end;
end;

function TWordFeud.InviteRandomOpponent(Ruleset:TRuleSet;Board:TBoardType):ISuperObject;
var
  url : String;
  data:TDictionary<String,String>;
  ReqResult:ISuperObject;
begin
  url := 'random_request/create';
  data := TDictionary<String,String>.Create;

  data.Add('ruleset',IntToStr(ord(Ruleset)));
  data.Add('board_type',cBoardTypeStr[Board]);
  ReqResult := Execute(url, data);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
    Result := ReqResult['content'];
end;

function TWordfeud.GetChatMessages(aGameId: TGameId):TChatMessageList;
var
  url : String;
  ReqResult:ISuperObject;
  i:Integer;
  MessageList:TSuperArray;
begin
  url := 'game/' + IntToStr(aGameID)+'/chat';
  ReqResult := Execute(url, nil);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
  begin
    MessageList := ReqResult['content.messages'].AsArray;
    SetLength(Result,MessageList.Length);
    for I := 0 to MessageList.Length-1 do
      Result[I].Message := MessageList.S[I];
  end;
end;

function TWordFeud.SendChatMessage(aGameID: TGameId; aMessage: string):ISuperObject;
var
  url : String;
  data:TDictionary<String,String>;
  ReqResult:ISuperObject;
begin
  url := 'game/' + IntToStr(aGameID)+'/chat/send';
  data := TDictionary<String,String>.Create;
  data.Add('message',aMessage);
  ReqResult := Execute(url, data);

  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
    Result := ReqResult['content'];
end;

function TWordFeud.GetAvatarURL(UserId: TUserId; Size: Integer):String;
begin
  Result := 'http://avatars.wordfeud.com/' + IntToStr(Size) + '/' + IntToStr(UserId);
end;

function TWordFeud.CreateAccount(aUserName,aEmail,aPassword:String):TUserId;
var
  url : String;
  data:TDictionary<String,String>;
  ReqResult:ISuperObject;
begin
  url := 'user/create';
  data := TDictionary<String,String>.Create;
  data.Add('username' ,aUserName);
  data.Add('email'    ,aEmail   );
  data.Add('password' ,aPassword);
  ReqResult := Execute(url, data);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
    Result := ReqResult['content'].I['id'];
end;


function TWordFeud.GetNotifications:TNotificationList;
var
  url : String;
  ReqResult:ISuperObject;
  i:Integer;
  NotificationList:TSuperArray;
begin
  url := 'user/notifications';
  ReqResult := Execute(url, nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
  begin
    NotificationList := ReqResult['content.entries'].AsArray;
    SetLength(Result,NotificationList.Length);
    for I := 0 to NotificationList.Length-1 do
      Result[I].Message := NotificationList.S[I];
  end;
end;

procedure TWordFeud.AcceptInvite(InviteId:string);
var
  url : String;
  ReqResult:ISuperObject;
begin
  url := 'invite/' + inviteID + '/accept';
  ReqResult := Execute(url, nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
end;

procedure TWordFeud.RejectInvite(InviteId:string);
var
  url : String;
  ReqResult:ISuperObject;
begin
  url := 'invite/' + inviteID + '/reject';
  ReqResult := Execute(url, nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
end;


function TWordFeud.GetStatus:ISuperObject;
var
  url : String;
  ReqResult:ISuperObject;
begin
  url := 'user/status';
  ReqResult := Execute(url, nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
    Result := ReqResult['content'];
end;

function TWordFeud.GetGame(aGameId:TGameId):TGame;
var
  url : String;
  ReqResult:ISuperObject;
begin
  url := 'game/'+IntToSTr(aGameId);
  ReqResult := Execute(url,nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
    Result.FromJSON(ReqResult['content.game']);
end;

function TWordFeud.GetGames:TGameList;
var
  url : String;
  ReqResult:ISuperObject;
  i:Integer;
  GamesList:TSuperArray;
begin
  url := 'user/games';
  ReqResult := Execute(url,nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type'])
  else
  begin
    GamesList:= ReqResult['content.games'].AsArray;
    SetLength(Result,GamesList.Length);
    for I := 0 to GamesList.Length-1 do
    begin
      Result[I].FromJSON(GamesList.O[I]);
    end;
  end;
  FGames := Result;
end;


procedure TWordFeud.ChangePassword(NewPassword: string);
var
  url : String;
  data:TDictionary<String,String>;
  ReqResult:ISuperObject;
  OldPass:String;
begin
  OldPass := FCredentials.Password;
  FCredentials.Password := NewPassword;
  url := 'user/password/set';
  data := TDictionary<String,String>.Create;
  data.Add('password' , FCredentials.PasswordHash);
  ReqResult := Execute(url, data);
  if ReqResult.S['status']<>'success' then
  begin
    FCredentials.Password := OldPass;
    raise EReadError.Create(ReqResult.S['content.type'])
  end;
end;

function TWordFeud.GetBoard(BoardId: Int64):TBoard;
var
  url : String;
  ReqResult:ISuperObject;
begin
  url := 'board/' + INtToStr(BoardID);
  ReqResult := Execute(url, nil);
  if ReqResult.S['status']<>'success' then
    raise EReadError.Create(ReqResult.S['content.type']);

  Result.FromJSON(ReqResult);
end;

procedure TLetterOnBoard.FromJSON(obj:ISuperObject);
begin
  X      := obj.AsArray.I[0];
  Y      := obj.AsArray.I[1];
  if Length(obj.AsArray.S[2])>0 then
    Letter := obj.AsArray.S[2][1]
  else
    Letter := ' ';
  Blank  := obj.AsArray.B[3];
end;

procedure TMove.FromJSON(obj: ISuperObject);
var
i:Integer;
LLetters:TSuperArray;
begin
  self := ctx.AsType<TMove>(obj);

  if obj.O['move']<>nil then
    case obj.O['move'].DataType of
      stArray: begin
          LLetters := obj.O['move'].AsArray;
          SetLength(move.Letters,LLetters.Length);
          for i := 0 to LLetters.Length-1 do
            move.Letters[I].FromJSON(LLetters[i]);

      end;
    end;
end;

procedure TPlayer.FromJSon(obj:ISuperObject);
var i:Integer;
begin
//  '{"position":0,"username":"de_echte_wouter","id":8340492,"score":152,"rack":["Y","N","H","T","S","I","S"]}'
  Self := ctx.AsType<TPlayer>(obj);
  if obj['rack']<>nil then
  begin
    SetLength(Rack, obj['rack'].AsArray.Length);
    for I := 0 to obj['rack'].AsArray.Length-1 do
      Rack[I] := obj['rack'].AsArray.S[I][1];
  end;

end;


procedure TGame.FromJSon(obj:ISuperObject);
var i:Integer;
begin
  Self := ctx.AsType<TGame>(obj);
  last_move.FromJSON(obj['last_move']);
  if obj['tiles']<>nil then
    if obj['tiles'].AsArray<>nil then
    begin
      SetLength(Tiles,obj['tiles'].AsArray.Length);
      for I := 0 to obj['tiles'].AsArray.Length-1 do
        Tiles[I].FromJSON(obj['tiles'].AsArray[i]);
    end;
  self.players[0].FromJSON(obj['players'].AsArray.O[0]);
end;

function TGame.GetMe: TPlayer;
begin
  if Length(Players[0].Rack)>0 then
    Result := Players[0]
  else
    Result := Players[1];
end;

function TGame.GetOpponent: TPlayer;
begin
  if Length(Players[0].Rack)=0 then
    Result := Players[0]
  else
    Result := Players[1];
end;

function TGame.getPlayer1:TPlayer;
begin
  if Players[0].id=0 then
    Result := Players[0]
  else
    Result := Players[1]
end;

function TGame.getPlayer2:TPlayer;
begin
  if Players[0].id=0 then
    Result := Players[1]
  else
    Result := Players[0]
end;

procedure TBoard.FromJSon(obj:ISuperObject);
begin
  Self := ctx.AsType<TBoard>(obj);
end;



{ TMoveLetters }

function TMoveLetters.ToString: String;
var
  Letter:TLetterOnBoard;
begin
  Result := '';
  for Letter in Letters do
    Result := Result + Letter.Letter;
end;

{ TCredentials }

function TCredentials.GetPasswordHash: String;
begin
  Result := SHA1(AnsiString(Password + 'JarJarBinks9'));
end;

function TLoginResponse.AsJSON: ISuperObject;
begin
  Result := ctx.AsJson<TLoginResponse>(self);
end;

initialization
  ctx := TSuperRttiContext.Create;

finalization
  FreeAndNil(ctx);


end.


