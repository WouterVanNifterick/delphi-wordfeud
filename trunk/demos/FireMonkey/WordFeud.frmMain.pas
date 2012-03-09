unit WordFeud.frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.ListBox, Zip, FMX.ExtCtrls, FMX.Effects, WvN.WordFeud, FMX.Types3D,
  FMX.Objects3D, FMX.TabControl, FMX.Layers3D, FMX.Filter.Effects, FMX.Memo;

type
  TOpenTaal = record
    Words: TStringList;
    procedure LoadFromFile;
  end;

  TLetter = record
    Letter: Char;
    Points: Byte;
    Count: Byte;
  end;

  TWord = record
  public
    Letters: Array of TLetter;
    function GetPoints: Integer;
    property Points: Integer read GetPoints;
    class operator implicit(word: TWord): string;
    class operator implicit(text: string): TWord;
    class operator explicit(text: string): TWord;
  end;


var
  Down: TPointF;

const
  cLetters: array ['a' .. 'z'] of TLetter = ((Letter: 'A'; Points: 1; Count: 6), (Letter: 'B'; Points: 3; Count: 2), (Letter: 'C'; Points: 5; Count: 2), (Letter: 'D'; Points: 1; Count: 5), (Letter: 'E'; Points: 1; Count: 18), (Letter: 'F'; Points: 4; Count: 2), (Letter: 'G'; Points: 3; Count: 3), (Letter: 'H'; Points: 4; Count: 2), (Letter: 'I'; Points: 1; Count: 4), (Letter: 'J'; Points: 4; Count: 2), (Letter: 'K'; Points: 3; Count: 3), (Letter: 'L'; Points: 3; Count: 3), (Letter: 'M'; Points: 3; Count: 3), (Letter: 'N'; Points: 1; Count: 10), (Letter: 'O'; Points: 1; Count: 6), (Letter: 'P'; Points: 3; Count: 2), (Letter: 'Q'; Points: 10; Count: 1), (Letter: 'R'; Points: 2; Count: 5), (Letter: 'S'; Points: 2; Count: 5), (Letter: 'T'; Points: 2; Count: 5), (Letter: 'U'; Points: 4; Count: 3), (Letter: 'V'; Points: 4; Count: 2), (Letter: 'W'; Points: 5; Count: 2), (Letter: 'X'; Points: 8; Count: 1), (Letter: 'Y'; Points: 8; Count: 1), (Letter: 'Z'; Points: 4; Count: 2));

type
  TItem = record
    TheWord: String;
    Score: Byte;
    constructor Create(const aWord: String; aScore: Byte);
  end;

  TForm9 = class(TForm)
    StyleBook1: TStyleBook;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Viewport3D1: TViewport3D;
    Panel2: TPanel;
    ListBox3: TListBox;
    TabItem2: TTabItem;
    ListBox1: TListBox;
    Panel1: TPanel;
    ClearingEdit1: TClearingEdit;
    ClearingEdit2: TClearingEdit;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ComboTrackBar1: TComboTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox2: TListBox;
    Layout3D1: TLayout3D;
    Light1: TLight;
    RadialBlurEffect1: TRadialBlurEffect;
    Camera1: TCamera;
    TabItem3: TTabItem;
    ShadowEffect1: TShadowEffect;
    pnlConnect: TPanel;
    Panel3: TPanel;
    lblConnectTitle: TLabel;
    edEmail: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edPassword: TEdit;
    btnConnect: TButton;
    ListBox4: TListBox;
    Timer1: TTimer;
    StyleBook2: TStyleBook;
    btnDisconnect: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ClearingEdit1ChangeTracking(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1Compare(Item1, Item2: TListBoxItem; var Result: Integer);
    procedure BtnConnectClick(Sender: TObject);
    procedure Grid3D1Render(Sender: TObject; Context: TContext3D);
    procedure ListBox3DblClick(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure AddBGStones;
    procedure ShowGames;
    { Private declarations }
  public
    t:TThread;
    NewList : TStringList;
    OpenTaal: TOpenTaal;
    word    : TWord;
    wf:twordfeud;
    CubesStuff:TList;

    FMousePosition: TPointf;
    FContextState:  TContextState;
    FRotSpeed: Single;
    FMoveSpeed: Single;
    FZoomSpeed: Single;

    FPixelShader: TContextShader;
    FVertexShader: TContextShader;

    FSunPosition: TVector3D;
    FAnimationTime: Single;
    FLightSize: Single;


    procedure Clear;
    procedure AddItem(const Item: TItem);
    procedure Show;
    procedure AddLetter(x,y,score:Integer;text:String);
    procedure MoveOverLetter(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure ClickOnLetter(Sender: TObject);

  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

uses StrUtils, Math, generics.Collections;

{ TOpenTaal }

procedure TOpenTaal.LoadFromFile;
begin
  FreeAndNil(Words);
  if not FileExists('OpenTaal\OpenTaal-210G-basis-gekeurd.txt') then
    TZipFile.ExtractZipFile('OpenTaal\OpenTaal-210G-basis-gekeurd.dat', 'OpenTaal\');
  Words := TStringList.Create;
  Words.LoadFromFile('OpenTaal\OpenTaal-210G-basis-gekeurd.txt');
end;

procedure TForm9.AddItem(const Item: TItem);
var
  li    : TListBoxItem;
  Points: Integer;
  word  : TWord;
begin
  li         := TListBoxItem.Create(self);
  li.Height  := 0;
  li.Opacity := 0;

  word      := Item.TheWord;
  Points    := word.Points;
  li.text   := Item.TheWord + ' (' + IntToSTr(Points) + ')';
  li.Parent := ListBox1;
  li.AnimateFloat('height', 21, 0.2);
  li.AnimateFloat('opacity', 1, 0.2);
end;

var
  Editing:Boolean = false;

procedure TForm9.AddLetter(x, y, score: Integer; text: String);
var
  Cube:TRoundCube;
  Text3d:TText3d;
begin
  Cube := TRoundCube.Create(Viewport3D1);
  Cube.Width := 1;
  Cube.Height := 1;
  Cube.Parent := Layout3D1;
  Cube.Material.Emissive := $00F2F2E1;
  Text3d := TText3D.Create(self);
  Text3d.Parent := Cube;
  Text3d.Font.Size := 1;
  Text3d.Position.X := 0;
  Text3d.Position.Y := 0;
  Text3d.Position.Z := -25;
  Text3d.Width := 0.8;
  Text3d.Height := 0.8;
  Text3d.Font.Family := 'Arial Black';
  Text3d.Text := Text;

  Randomize;
  Text3d.RotationAngle.X := Math.RandomRange(0,360);
  Text3d.RotationAngle.Y := Math.RandomRange(0,360);
  Text3d.RotationAngle.Z := Math.RandomRange(0,360);

  Text3d.AnimateFloat('Position.Z'     ,-0.5,1);
  Text3d.AnimateFloat('RotationAngle.X',720,1);
  Text3d.AnimateFloat('RotationAngle.Y',360,1);
  Text3d.AnimateFloat('RotationAngle.Z',0,1);

  Cube.Position.X := X+RandomRange(-10,10);
  Cube.Position.Y := Y+RandomRange(-10,10);
  Cube.AnimateFloat('Position.X',X-7,1);
  Cube.AnimateFloat('Position.Y',Y-7,1);

  Cube.OnMouseMove := MoveOverLetter;
  Cube.OnClick := ClickOnLetter;

  CubesStuff.Add(Cube);
end;

procedure TForm9.AddBGStones;
var
  Cube: TRoundCube;
  x: Integer;
  t: TBoardLayout.TBoardLocationType;
  y: Integer;
  text3d:TText3D;
begin
  for X := 0 to 14 do
    for Y := 0 to 14 do
    begin
      t := TBoardLayout.DefaultWordFeudLayout[X, Y];
      Cube := TRoundCube.Create(Viewport3D1);
      Cube.Width := 0.7;
      Cube.Height := 0.7;
      Cube.Parent := Layout3D1;
      Cube.Material.Emissive := TBoardLayout.BGColorsWin[t];
      Cube.Position.X := X - 6.9;
      Cube.Position.Y := Y - 6.9;
      Cube.Position.Z := 0.4;

      if (TBoardLayout.Texts[TBoardLayout.DefaultWordFeudLayout[X, Y]])<>'' then
      begin
        Text3d := TText3D.Create(self);
        Text3d.Parent := Cube;
        Text3d.Stretch := True;
        Text3d.Position.X := 0;
        Text3d.Position.Y := 0;
        Text3d.Position.Z := -0.4;
        Text3d.Width := 0.4;
        Text3d.Height := 0.4;
        Text3d.Text := TBoardLayout.Texts[TBoardLayout.DefaultWordFeudLayout[X, Y]];
      end;

      CubesStuff.Add(Cube);
    end;
end;



procedure TForm9.BtnDisconnectClick(Sender: TObject);
begin
  wf.LogOut;
  btnConnect.Text := 'Connect';
  btnConnect.OnClick := BtnConnectClick;
  Listbox3.Clear;
  Clear;

  btnConnect.Enabled := True;
  pnlConnect.AnimateFloat('Opacity',1,1);
  pnlConnect.AnimateFloat('Scale.X',1,1);
  pnlConnect.AnimateFloat('Scale.Y',1,1);

end;

procedure TForm9.BtnConnectClick(Sender: TObject);
begin
  btnConnect.Enabled := False;
  wf.logInUsingEmail(edEmail.Text,edPassword.Text);
  wf.GetGames;
  Memo1.Lines.Text := ctx.AsJson<TArray<TGame>>(WF.Games).AsJson(True);
  ShowGames;
  btnConnect.Text := 'Disconnect';
  btnConnect.OnClick := BtnDisconnectClick;

  pnlConnect.AnimateFloat('Opacity',0,1);
  pnlConnect.AnimateFloat('Scale.X',0.1,1);
  pnlConnect.AnimateFloat('Scale.Y',0.1,1);

  Timer1.Enabled := True;
end;

procedure TForm9.Clear;
begin
  Layout3D1.DeleteChildren;
end;

procedure TForm9.ClearingEdit1ChangeTracking(Sender: TObject);
  var
    S       : String;
    c       : Char;
    Found   : Boolean;
    Opt, Req: String;
begin
  NewList.Clear;
  if ComboBox1.ItemIndex = 3 then
  begin
    Label4.AnimateFloat('opacity', 0.7, 0.5);
    ComboTrackBar1.AnimateFloat('opacity', 0.7, 0.5);
    ComboTrackBar1.ReadOnly := False;
  end
  else
  begin
    Label4.AnimateFloat('opacity', 0.2, 0.5);
    ComboTrackBar1.AnimateFloat('opacity', 0.2, 0.5);
    ComboTrackBar1.ReadOnly := True;
  end;

  begin
    for S in OpenTaal.Words do
    begin
      Req := ClearingEdit2.text;

      Found := True;

      Opt := ClearingEdit1.text;

      for c in S do
        if Pos(c, Opt) < 1 then
        begin
          Found := False;
          Break;
        end
        else
          Delete(Opt,Pos(c, Opt),1);

      if trim(Req) <> '' then
        case ComboBox1.ItemIndex of
          0: if Pos(Req, S) < 1 then            Found := False;
          1: if Pos(Req, S) <> 1 then            Found := False;
          2: if not EndsText(Req, S) then            Found := False;
          3: if Pos(Req, S) <> Round(ComboTrackBar1.Value) then Found := False;
        end;

      if Length(S) = 1 then
        Found := False;

      if not(CharInSet(S[1],['A' .. 'Z'])) then
        if Found then
          NewList.Add(S);

      if NewList.Count > 50 then
        Break;
    end;
  end;
  Show;
  Editing := False;

end;

procedure TForm9.ClickOnLetter(Sender: TObject);
begin
  if not (Sender is TRoundCube) then
    Exit;
end;

procedure TForm9.ShowGames;
var
  Game: TGame;
  OldIndex:Integer;
begin
  OldIndex := ListBox3.ItemIndex;
  ListBox3.Clear;
  for Game in wf.Games do
    ListBox3.Items.Add(Format('%s %s (%d points)', [Game.Opponent.username, Game.last_move.main_word, Game.last_move.points]));

  if OldIndex<ListBox3.Count then
    ListBox3.ItemIndex := OldIndex;
end;

procedure TForm9.Timer1Timer(Sender: TObject);
begin
  wf.GetGames;
  ShowGames;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  wf.Create('');
  self.StyleBook := StyleBook1;
  NewList        := TStringList.Create;
  OpenTaal.LoadFromFile;
  CubesStuff := TList.Create;

  FSunPosition := Vector3D(45, 70, 80);

  // create camera nad speed settings
  FRotSpeed := 1/4;
  FMoveSpeed := 0.5;
  FZoomSpeed := 2;

  Clear;
  AddBGStones;

end;

procedure TForm9.Grid3D1Render(Sender: TObject; Context: TContext3D);
begin
//
end;

procedure TForm9.ListBox1Click(Sender: TObject);
var
  l   : TLetter;
  Item: TListBoxItem;
begin
  if not Assigned(ListBox1.Selected) then
    Exit;

  word := Copy(ListBox1.Selected.text, 1, Pos('(', ListBox1.Selected.text));
  ListBox2.Clear;
  for l in word.Letters do
  begin
    Item             := TListBoxItem.Create(self);
    Item.Parent      := Listbox2;
    Item.text        := l.Letter;
    Item.TextAlign := TTextAlign.taCenter;
    Item.Margins.Left := 4;
  end;

end;

procedure TForm9.ListBox1Compare(Item1, Item2: TListBoxItem;
  var Result: Integer);
var
  Word1,Word2:TWord;
begin
  Word1 := Copy(Item1.Text, 1, Pos('(', Item1.Text));
  Word2 := Copy(Item2.Text, 1, Pos('(', Item2.Text));

  Result := CompareValue(Word2.Points,Word1.Points);
end;

procedure TForm9.ListBox3DblClick(Sender: TObject);
var
  Game:TGame;
  Letter:TLetterOnBoard;
  i:Integer;
  C:Char;
begin
  if Listbox3.ItemIndex<0 then
    Exit;

  Clear;
  Game := wf.GetGame(wf.Games[Listbox3.ItemIndex].id);
  with Game do
    begin
      AddBGStones;
      for i := 0 to Length(Tiles)-1 do
      begin
        Letter := Tiles[i];
        AddLetter(Letter.X,Letter.Y,0,Letter.Letter);
      end;
      Listbox4.Clear;
      for C in Game.Me.Rack do
        Listbox4.Items.Add(C);

    end;
end;


procedure TForm9.MoveOverLetter(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
var
  MyCube:TRoundCube;
begin
  if not (Sender is TRoundCube) then
    Exit;

  MyCube := Sender as TRoundCube;
  MyCube.AnimateFloat('Scale.X',1.2,0.5);
  MyCube.AnimateFloatDelay('Scale.X',1,0.5,0.5);
  MyCube.AnimateFloat('Scale.Y',1.2,0.5);
  MyCube.AnimateFloatDelay('Scale.Y',1,0.5,0.5);
end;

procedure TForm9.Show;
var
  i, index: Integer;
  S       : String;
begin
  for i := ListBox1.Count - 1 downto 0 do
  begin
    S     := ListBox1.Items[i];
    index := NewList.IndexOf(S);
    if index < 0 then
      ListBox1.Items.Delete(i);
  end;

  for S in NewList do
    if ListBox1.Items.IndexOf(S) < 0 then
      AddItem(TItem.Create(S, 0));

end;

procedure TForm9.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Down:= PointF(X,Y);

end;

procedure TForm9.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    Layout3D1.RotationAngle.X := Layout3D1.RotationAngle.X - ((Y - Down.Y) * 0.05);
    Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y - ((X - Down.X) * 0.05);
    Down := PointF(X, Y);
  end;

end;

procedure TForm9.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  NewPos:TVector3d;
begin
  NewPos := Vector3DAdd(Viewport3D1.Camera.Position.Vector, Vector3DScale(Vector3D(0, 0, 1), (WheelDelta / 120) * FZoomSpeed));

  Viewport3D1.Camera.AnimateFloat('Position.Vector.X', NewPos.X, 0.2);
  Viewport3D1.Camera.AnimateFloat('Position.Vector.Y', NewPos.Y, 0.2);
  Viewport3D1.Camera.AnimateFloat('Position.Vector.Z', NewPos.Z, 0.2);
  Viewport3D1.Camera.AnimateFloat('Position.Vector.W', NewPos.W, 0.2);
end;

{ TItem }

constructor TItem.Create(const aWord: String; aScore: Byte);
begin
  TheWord := aWord;
  Score   := aScore;
end;

{ TWord }

class operator TWord.implicit(word: TWord): string;
var
  c: TLetter;
begin
  Result := '';
  for c in word.Letters do
    Result := Result + c.Letter;
end;

class operator TWord.explicit(text: string): TWord;
var
  i     : Integer;
begin
  SetLength(Result.Letters, Length(text));
  for i                   := 1 to Length(text) do
    Result.Letters[i - 1] := cLetters[text[i]];
end;

function TWord.GetPoints: Integer;
var
  l: TLetter;
begin
  Result := 0;
  for l in Letters do
    Result := Result + l.Points;
end;

class operator TWord.implicit(text: string): TWord;
var
  i     : Integer;
  c     : Char;
begin
  SetLength(Result.Letters, Length(text));
  for i := 1 to Length(text) do
  begin
    c := text[i];
    if CharInSet(c,['a' .. 'z']) then
      Result.Letters[i - 1] := cLetters[c]
    else
      SetLength(Result.Letters, Length(Result.Letters) - 1);
  end;
end;


end.
