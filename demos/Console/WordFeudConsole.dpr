program WordFeudConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DateUtils,
  WvN.WordFeud in '..\..\WvN.WordFeud.pas',
  WvN.Console in 'WvN.Console.pas',
  System.SysUtils;

var
  Credentials:record
    Email:String;
    Password:String;
  end;

procedure PaintGame(const aGame:TGame);
var
  x,y,i:Integer;
begin
    Console.ClrScr;
    for y := 0 to 14 do
    begin
      for x := 0 to 14 do
      begin
        Console.GotoXY( X*4, Y*2 );
        Console.TextBackground( ord(TBoardLayout.BGColors[TBoardLayout.DefaultWordFeudLayout[x,y]]));
        Console.TextColor     ( ord(TBoardLayout.FGColors[TBoardLayout.DefaultWordFeudLayout[x,y]]));
        Write(Copy(TBoardLayout.Texts[TBoardLayout.DefaultWordFeudLayout[x,y]]+'   ',1,3));
      end;
    end;

    for i := 0 to Length(aGame.Tiles)-1 do
    begin
      Console.GotoXY( aGame.Tiles[i].X*4,
                      aGame.Tiles[i].Y*2 );
      WriteC('~w^z '+aGame.Tiles[i].Letter+' ~z^w ');
    end;
end;

var
  wf:TWordFeud;
  Game:TGame;
  GameIndex:Integer;
  Board:TBoard;


begin
  try

//    Board := wf.GetBoard( Game.id );
    Console.BufferHeight := 120;
    Console.BufferWidth  := 120;
    Console.CursorVisible := False;
    Console.Clear;

    Write('Email: ');
    Readln(Credentials.Email);
    Write('Password: ');
    Readln(Credentials.Password);

    wf.Create('');
    wf.LogInUsingEmail(Credentials.Email, Credentials.Password);
    wf.GetGames;
    GameIndex := 0;
    for Game in wf.Games do
    begin
      Write(' ');
      Write(GameIndex:2);
      Write(' ');
      Write(HoursBetween(now,UnixToDateTime(Round(Game.updated))),'hrs');
      Write(' ');
//      Write(Game.id); Write(' ');
      Write(Game.Player2.username:5);
      Write(' ');
      Write(Game.last_move.main_word:-10);
      Write(' (');
      Write(Game.last_move.points);
      Write(')');

//      Write(Game.last_move.move.Letters;

      WriteLn;
      Inc(GameIndex);
    end;

    WriteLn('Enter game to play:');
    ReadLn(GameIndex);
    Game := wf.Games[GameIndex];
    Game := wf.GetGame(Game.id);

    PaintGame(Game);
    ReadLn;





  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
