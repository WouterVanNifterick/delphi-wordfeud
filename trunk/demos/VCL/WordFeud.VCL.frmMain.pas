unit WordFeud.VCL.frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ExtCtrls,
  WvN.WordFeud, VCL.StdCtrls, VirtualTrees, Vcl.ComCtrls, SuperObject;


type
  TNotifyProc = reference to procedure(Sender:TObject);

  TStoneBase=class(TWinControl)
  private
    public
      Shape: TShape;
      lbl:TLabel;
    constructor Create(AOwner: TComponent); override;
  end;

  TStone=class(TStoneBase)
  private
    FXPos: Integer;
    FYPos: Integer;
    procedure SetXPos(const Value: Integer);virtual;
    procedure SetYPos(const Value: Integer);virtual;
    constructor Create(AOwner: TComponent); override;
  end;

  TBGStone=class(TStone)
  private
    procedure SetXPos(const Value: Integer);override;
    procedure SetYPos(const Value: Integer);override;
  public
    constructor Create(AOwner: TComponent); override;
    property XPos:Integer read FXPos write SetXPos;
    property YPos:Integer read FYPos write SetYPos;
  end;

type
  TfrmMain = class(TForm)
    Panel2: TPanel;
    Button1: TButton;
    ListView1: TListView;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel1: TPanel;
    pnlRack: TPanel;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    procedure Shape1MouseEnter(Sender: TObject);
    procedure Shape1MouseLeave(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VirtualStringTree1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
  private
    procedure CreateBGStone(Y: Integer; X: Integer);
    { Private declarations }
  public
    { Public declarations }
    WF:TWordFeud;
    BGStones:array[0..15,0..15] of TBgStone;
    procedure PaintBoardBG;
  end;

type
  TNotifierRef = procedure(Sender:TObject) of object;
  TNotifiedHandler=class
    class function GetNotifyEvent(Code:TNotifierRef):TNotifyEvent;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  IoUtils,
  WordFeud.VCL.frmConnect;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  Game:TGame;
  Settings:isuperObject;
  json:String;
  s1,s2,s3:String;
const
  cSettingsFileName= 'settings.json';
begin
  if FileExists(cSettingsFileName) then
  begin
    s1 := TFile.ReadAllText(cSettingsFileName);
    Settings := SO(TFile.ReadAllText(cSettingsFileName));
    s2 := Settings['credentials'].AsObject.S['email'];
    s3 := Settings['credentials'].AsObject.S['password'];
    WF.LogInUsingEmail(s2,s3);
  end
  else
    if frmConnect.ShowModal = mrOK then
    begin
      Settings := TSuperObject.Create;
      Settings.S['credentials.email']    := WF.MyCredentials.Email;
      Settings.S['credentials.password'] := WF.MyCredentials.Password;
      Settings.S['credentials.password_hash'] := WF.MyCredentials.PasswordHash;
      TFile.WriteAllText(cSettingsFileName,Settings.AsJSon(True));
      WF.LogInUsingEmail(frmConnect.edEmail.Text, frmConnect.edPassword.Text);
    end
    else
      Exit;

  WF.GetGames;
  ListView1.Items.Count := Length(WF.Games);

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PaintBoardBG
end;

procedure TfrmMain.ListView1Data(Sender: TObject; Item: TListItem);
begin
//  Caption := WF.Games[ Item.Index ].Player1.username +WF.Games[ Item.Index ].Player2.username;
//  Caption := InttoSTr(Item.Index);

  Item.SubItems.Add(WF.Games[ Item.Index ].Opponent.username);
  Item.SubItems.Add(WF.Games[ Item.Index ].last_move.main_word);
  Item.SubItems.Add(IntToStr(WF.Games[ Item.Index ].last_move.points));
end;

procedure TfrmMain.ListView1DblClick(Sender: TObject);
var
  i:INteger;
  Game:TGame;
  Letter:TLetterOnBoard;
  Stone:TStoneBase;
  Stone2:TStone;
begin
  if Listview1.ItemIndex<0 then
    Exit;

  Game := WF.GetGame(WF.Games[Listview1.ItemIndex].id);
  WF.Games[Listview1.ItemIndex] := Game;

  for I := 0 to Length(Game.Me.Rack)-1 do
  begin
    Stone:= TStoneBase.Create(self);
    Stone.Shape.Brush.Color := $00E6FFFE;
    Stone.lbl.Caption := Game.Me.Rack[I];
    Stone.Parent := pnlRack;
    Stone.Align := alLeft;
    Stone.Width := Stone.Height;
  end;
{
  for i := 0 to Length(Game.Tiles)-1 do
  begin
    Letter := Game.Tiles[i];
    BGStones[Letter.X,Letter.Y].Shape.Brush.Color := $00E6FFFE;
    BGStones[Letter.X,Letter.Y].lbl.Caption := Game.Tiles[I].Letter;
  end;
}

  for I := 0 to Length(Game.Tiles)-1 do
  begin
    Stone2 := TStone.Create(self);
    Stone2.Shape.Brush.Color := $00E6FFFE;
    Stone2.lbl.Caption := Game.Tiles[I].Letter;
    Stone2.Parent := Panel1;
    Stone2.SetXPos(Game.Tiles[i].X);
    Stone2.SetYPos(Game.Tiles[i].Y);
  end;


end;

procedure TfrmMain.PaintBoardBG;
var
  Y    : Integer;
  X    : Integer;
begin
  for Y   := 0 to 14 do
    for X := 0 to 14 do
      CreateBGStone(Y, X);
end;

procedure TfrmMain.CreateBGStone(Y: Integer; X: Integer);
var
  BgStone: TBGStone;
begin
  BGStones[X, Y] := TBgStone.Create(Panel1);
  BgStone := BGStones[X, Y];
  BgStone.Parent := panel1;
  BgStone.Left := Round((Panel1.Width / 15) * X);
  BgStone.Top := Round((Panel1.Height / 15) * Y);
  BgStone.Width := Round((Panel1.Width / 15)) - 2;
  BgStone.Height := Round((Panel1.Height / 15)) - 2;
  BgStone.XPos := X;
  BgStone.YPos := Y;

end;

procedure TfrmMain.Panel1Resize(Sender: TObject);
var
  Y    : Integer;
  X    : Integer;
  Shape: TBgStone;
begin
  if not Assigned(BGStones[0,0]) then
    Exit;

  Panel1.Locked := True;
  for Y   := 0 to 14 do
    for X := 0 to 14 do
    begin
      Shape             := BGStones[X,Y];
      Shape.Left        := Round((Panel1.Width / 15) * X);
      Shape.Top         := Round((Panel1.Height / 15) * Y);
      Shape.Width       := Round((Panel1.Width / 15))-2;
      Shape.Height      := Round((Panel1.Height / 15))-2;
    end;
  Panel1.Locked := False;
end;

procedure TfrmMain.Shape1MouseEnter(Sender: TObject);
begin
  TShape(Sender).Brush.Color := clWhite;
end;

procedure TfrmMain.Shape1MouseLeave(Sender: TObject);
begin
  TShape(Sender).Brush.Color := clDkGray;
end;

procedure TfrmMain.VirtualStringTree1InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

{ TNotifiedHandler }

class function TNotifiedHandler.GetNotifyEvent(Code: TNotifierRef): TNotifyEvent;
begin

end;

{ TBGStone }

constructor TBGStone.Create(AOwner: TComponent);
begin
  inherited;
  @lbl.OnMouseEnter := pPointer(Cardinal(pPointer(procedure (sender: tObject)
  begin
    (((sender as TLabel).Parent) as TBgStone).Brush.Color := clWhite
  end)^) + $C)^;
  @lbl.OnMouseLeave := pPointer(Cardinal(pPointer(procedure (sender: tObject)
  begin
    (((sender as TLabel).Parent) as TBgStone).Brush.Color := clBlack
  end)^) + $C)^;
end;


procedure TBGStone.SetXPos(const Value: Integer);
begin
  FXPos := Value;

  Shape.Brush.Color := TBoardLayout.BGColorsWin[TBoardLayout.DefaultWordFeudLayout[fxPOS,fypos]];
  lbl.Font.Color := TBoardLayout.FGColorsWin[TBoardLayout.DefaultWordFeudLayout[fxPOS,fypos]];
  lbl.Caption := TBoardLayout.Texts[TBoardLayout.DefaultWordFeudLayout[fxPOS,fyPOS]];
end;

procedure TBGStone.SetYPos(const Value: Integer);
begin
  FYPos := Value;

  Shape.Brush.Color := TBoardLayout.BGColorsWin[TBoardLayout.DefaultWordFeudLayout[fxPOS,fypos]];
  lbl.Font.Color := TBoardLayout.FGColorsWin[TBoardLayout.DefaultWordFeudLayout[fxPOS,fypos]];
  lbl.Caption := TBoardLayout.Texts[TBoardLayout.DefaultWordFeudLayout[fxPOS,fyPOS]];

end;

{ TStone }

constructor TStone.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TStone.SetXPos(const Value: Integer);
begin
  FXPos := Value;
  Self.Left := Round((TWinControl(Parent).Width / 15) * FXPos);
  Self.Width := Round((TWinControl(Parent).Width / 15));
end;

procedure TStone.SetYPos(const Value: Integer);
begin
  FYPos := Value;
  Self.Top := Round((TWinControl(Parent).Height / 15) * FYPos);
  Self.Height := Round((TWinControl(Parent).Height / 15));
end;

{ TStoneBase }

constructor TStoneBase.Create(AOwner: TComponent);
begin
  inherited;
  ParentColor := True;
  Shape := TShape.Create(self);
  self.Color := clNone;
  Shape.Align := alClient;
  Shape.Shape := stRoundRect;
  Shape.Parent := TWinControl(Self);
  lbl := TLabel.Create(self);
  lbl.Align := alClient;
  lbl.Parent := Self;
  lbl.Caption := '';
  lbl.Alignment := taCenter;
  lbl.Layout := tlCenter;

end;

end.
