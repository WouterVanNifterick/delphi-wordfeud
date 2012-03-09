unit WvN.Console;

{
Copyright (c) <2012> <Wouter van Nifterick>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


Console library.
Can do most of what the CRT unit used to do under dos,
and mimics the DotNet console class as well (interface is nearly the same)
}

{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 17.0}
{$DEFINE INLINES}
{$IFEND}
{$IF RTLVersion >= 14.0}
{$DEFINE HASERROUTPUT}
{$IFEND}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$SCOPEDENUMS ON}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Variants;

type

  PD3InputRecord = ^TD3InputRecord;
  TD3InputRecord = record
    EventType: Word;
    case Integer of
      0: (KeyEvent: TKeyEventRecord);
      1: (MouseEvent: TMouseEventRecord);
      2: (WindowBufferSizeEvent: TWindowBufferSizeRecord);
      3: (MenuEvent: TMenuEventRecord);
      4: (FocusEvent: TFocusEventRecord);
  end;

  TWinColor= (
    colBackgroundBlue     = $10,
    colBackgroundGreen    = $20,
    colBackgroundRed      = $40,
    colBackgroundYellow   = $60,
    colBackgroundIntensity= $80,
    colBackgroundMask     = $F0,
    colBlack              = 0,
    colColorMask          = $ff,
    colForegroundBlue     = 1,
    colForegroundGreen    = 2,
    colForegroundRed      = 4,
    colForegroundYellow   = 6,
    colForegroundIntensity= 8,
    colForegroundMask     = 15
  );

  TConsoleColor=
  (
    Black,	    // the color black.
    DarkBlue,	  // the color dark blue.
    DarkGreen,	// the color dark green.
    DarkCyan,	  // the color dark cyan (dark blue-green).
    DarkRed,   	// the color dark red.
    DarkMagenta,// the color dark magenta (dark purplish-red).
    DarkYellow,	// the color dark yellow (ochre).
    Gray,	      // the color gray.
    DarkGray,  	// The color dark gray.
    Blue,	      // The color blue.
    Green,     	// The color green.
    Cyan,	      // The color cyan (blue-green).
    Red,	      // The color red.
    Magenta,    // The color magenta (purplish-red).
    Yellow,	    // The color yellow.
    White	      // The color white.
  );

  TConsoleKey = (
    Backspace         = 8,
    Tab               = 9,
    Clear             = 12,
    Enter             = 13,
    Escape            = $1B,
    PauseKey          = $13,
    Spacebar          = $20,
    PageUp            = $21,
    PageDown          = $22,
    &End              = $23,
    Home              = $24,
    LeftArrow         = $25,
    UpArrow           = $26,
    RightArrow        = $27,
    DownArrow         = $28,
    Select            = $29,
    Print             = $2A,
    Execute           = $2B,
    PrintScreen       = $2C,
    Insert            = $2D,
    Delete            = $2E,
    Help              = $2F,
    D0                = $30,
    D1                = $31,
    D2                = $32,
    D3                = $33,
    D4                = $34,
    D5                = $35,
    D6                = $36,
    D7                = $37,
    D8                = $38,
    D9                = $39,
    A                 = $41,
    B                 = $42,
    C                 = $43,
    D                 = $44,
    E                 = $45,
    F                 = $46,
    G                 = $47,
    H                 = $48,
    I                 = $49,
    J                 = $4A,
    K                 = $4B,
    L                 = $4C,
    M                 = $4D,
    N                 = $4E,
    O                 = $4F,
    P                 = $50,
    Q                 = $51,
    R                 = $52,
    S                 = $53,
    T                 = $54,
    U                 = $55,
    V                 = $56,
    W                 = $57,
    X                 = $58,
    Y                 = $59,
    Z                 = $60,
    Applications      = $5D,
    Multiply          = $6A,
    Add               = $6B,
    Separator         = $6C,
    Subtract          = $6D,
    Divide            = $6F,

    F1                = $70,
    F2                = $71,
    F3                = $72,
    F4                = $73,
    F5                = $74,
    F6                = $75,
    F7                = $76,
    F8                = $77,
    F9                = $78,
    F10               = $79,
    F11               = $7A,
    F12               = $7B,
    F13               = $7C,
    F14               = $7D,
    F15               = $7E,
    F16               = $7F,
    F17               = $80,
    F18               = $81,
    F19               = 130,
    F20               = $83,
    F21               = $84,
    F22               = $85,
    F23               = $86,
    F24               = $87,


    BrowserSearch     = 170,
    BrowserBack       = $A6,
    BrowserForward    = $A7,
    BrowserRefresh    = $A8,
    BrowserStop       = $A9,
    BrowserFavorites  = $AB,
    BrowserHome       = $AC,
    VolumeMute        = $AD,
    VolumeDown        = $AE,
    VolumeUp          = $AF,

    Attention         = $F6,
    CrSel             = $F7,
    ExSel             = $F8,
    EraseEndOfFile    = $F9,
    Decimal           = 110,

    MediaNext         = $B0,
    MediaPrevious     = $B1,
    MediaStop         = $B2,
    MediaPlay         = $B3,
    LaunchMediaSelect = $B5,
    LaunchApp1        = $B6,
    LaunchApp2        = $B7,
    LaunchMail        = 180,
    LeftWindows       = $5B,

    NoName            = $FC,
    NumPad0           = $60,
    NumPad1           = $61,
    NumPad2           = $62,
    NumPad3           = $63,
    NumPad4           = 100,
    NumPad5           = $65,
    NumPad6           = $66,
    NumPad7           = $67,
    NumPad8           = $68,
    NumPad9           = $69,
    Oem1              = $BA,
    Oem2              = $BF,
    Oem3              = $C0,
    Oem4              = $DB,
    Oem5              = $DC,
    Oem6              = $DD,
    Oem7              = $DE,
    Oem8              = $DF,
    Oem102            = $E2,
    OemClear          = $FE,
    OemComma          = $BC,
    OemMinus          = $BD,
    OemPeriod         = 190,
    OemPlus           = $BB,
    Pa1               = $FD,
    Packet            = $E7,
    Play              = 250,
    Process           = $E5,
    RightWindows      = $5C,
    Sleep             = $5F,
    Zoom              = $FB
  );




  TConsoleModifiers=(
    Alt,	  // The left or right ALT modifier key.
    Shift,  // The left or right SHIFT modifier key.
    Control	// The left or right CTRL modifier key.
  );

  TConsoleModifiersSet=set of TConsoleModifiers;

  TConsoleKeyInfo=record
  private
    // Fields
  strict private
    FKey: TConsoleKey;
    FKeyChar: Char;
    FMods: TConsoleModifiersSet;
  private
    function GetKey: TConsoleKey;
    function GetKeyChar: Char;
    function GetModifiers: TConsoleModifiersSet;
  public
    constructor Create(keyChar: Char; key: TConsoleKey; shift: boolean; alt: boolean; control: boolean);
    property Key:TConsoleKey read GetKey write FKey;
    property KeyChar:Char read GetKeyChar write FKeyChar;
    property Modifiers:TConsoleModifiersSet read GetModifiers;
  end;


type
  TBoxCharacters=record
    const
      UpLeftChar   = Char(218); //'Ú';
      UpRightChar  = Char(191); //'¿';
      LowLeftChar  = Char(192); //'À';
      LowRightChar = Char(217); //'Ù';
      HorChar      = Char(196); // 'Ä';
      VertChar     = Char(179); //'³';
      SpaceChar    = Char(32) ;//' ';
  end;

  TConsoleColors=record
    const
      // Background and foreground colors
      Black     = 0;
      Blue      = 1;
      Green     = 2;
      Cyan      = 3;
      Red       = 4;
      Magenta   = 5;
      Brown     = 6;
      LightGray = 7;

      // Foreground colors
      DarkGray     = 8;
      LightBlue    = 9;
      LightGreen   = 10;
      LightCyan    = 11;
      LightRed     = 12;
      LightMagenta = 13;
      Yellow       = 14;
      White        = 15;

      // Blink attribute, to be or-ed with background colors.
      Blink = 128;

      // Background and foreground colors
      BlinkBlack     = 0 or Blink;
      BlinkBlue      = 1 or Blink;
      BlinkGreen     = 2 or Blink;
      BlinkCyan      = 3 or Blink;
      BlinkRed       = 4 or Blink;
      BlinkMagenta   = 5 or Blink;
      BlinkBrown     = 6 or Blink;
      BlinkLightGray = 7 or Blink;
    end;

    TConsoleTextModes=record
    const
      // Text modes:
      BW40    = 0;   // 40x25 B/W on Color Adapter
      CO40    = 1;   // 40x25 Color on Color Adapter
      BW80    = 2;   // 80x25 B/W on Color Adapter
      CO80    = 3;   // 80x25 Color on Color Adapter
      Mono    = 7;   // 80x25 on Monochrome Adapter
      Font8x8 = 256; // Add-in for ROM font

      // Mode constants for 3.0 compatibility of original CRT unit }
      C40 = CO40;
      C80 = CO80;
    end;

type
  TSoundProc   = procedure(Frequency: Smallint); // Plays a sound at the given frequency (in Herz).
  TNoSoundProc = procedure; // Stops the sound started with Sound.
  TDelayProc   = procedure(Millisecs: Integer); // Delays for the given amount of milliseconds, or as close as possible.
  TBeepProc    = procedure(Frequency, Duration: Smallint); // Plays a sound at the given frequency (in Hz) and duration (in ms).

  TConsole = record
  private
    Sound      : TSoundProc;
    NoSound    : TNoSoundProc;
    Delay      : TDelayProc;
    FTextWindow: TSmallRect;
    TextAttr   : Byte;
    DefaultAttr: Byte;
    ScreenSize : TCoord;
    StdErr     : THandle;
    LastMode   : Word;
    WindMin    : Word;
    WindMax    : Word;

    function GetCursorX: Integer; {$IFDEF INLINES}inline; {$ENDIF}
    function GetCursorY: Integer; {$IFDEF INLINES}inline; {$ENDIF}
    procedure SetCursorPos(X, Y: Smallint);
    procedure Scroll(Left, Top, Right, Bottom: Integer; Distance: Integer = 0);
    function Validate(X1, Y1, X2, Y2: Integer): Boolean;
    procedure WriteText(Line: PAnsiChar; Len: Integer);
    procedure GetScreenSizes(var Width, Height: Smallint);
    procedure InitScreenMode;

  public
    StdIn, StdOut: THandle;
    BeepProc : TBeepProc;

    // Turbo/Borland Pascal Crt routines:
    function ReadKeyChar: Char; // Waits for keypress and returns the key pressed. If the key is not an ASCII key, #0 is returned, and a successive ReadKey will give the extended key code of the key.
    function KeyPressed: Boolean; // Checks whether a key was pressed.
    procedure GotoXY(X, Y: Smallint); // Puts the cursor at the given coordinates on the screen.
    function WhereX: Integer;         // Returns the current X position of the cursor.
    function WhereY: Integer;         // Returns the current Y position of the cursor.
    procedure TextColor(Color: Byte); overload; // Sets text foreground color.
    function TextColor: Byte; overload; // Gets text forground color.
    procedure TextBackground(Color: Byte); overload; // Sets text background color.
    function TextBackground: Byte; overload; // Gets text background color.
    procedure TextMode(Mode: Word); // Sets text mode.
    procedure LowVideo;  // Sets text colors to low intensity
    procedure HighVideo; // Sets text colors to high intensity
    procedure NormVideo; // Sets text attribute to value at startup.
    procedure ClrScr;    // Clears the entire screen, or, if a window is set, the entire window, in the current background color.
    procedure ClrEol;    // Clears until the end of the line, in the current background color.
    procedure InsLine;   // Inserts a line at the current cursor position.
    procedure DelLine;   // Deletes the line at the current cursor position.
    procedure Window(Left, Top, Right, Bottom: Integer);
    // Sets a window, into which all successive output will go. You can reset the window to full screen by calling Window with a zero or negative value for Left.

    // Additional routines:
    function ScreenWidth: Smallint;
    function ScreenHeight: Smallint;
    property TextWindow: TSmallRect read FTextWindow write FTextWindow;
    function IsOutputRedirected: Boolean;
  public
    procedure Init;
    procedure Beep;overload;

    procedure Beep(frequency, duration : Integer);overload;

    procedure Clear;

    procedure MoveBufferArea(
      sourceLeft,
      sourceTop,
      sourceWidth,
      sourceHeight,
      targetLeft,
      targetTop : integer);overload;

    procedure MoveBufferArea(
      sourceLeft,
      sourceTop,
      sourceWidth,
      sourceHeight,
      targetLeft,
      targetTop : integer;
      sourceChar : char;
      sourceForeColor,
      sourceBackColor : TConsoleColor);overload;


    class function OpenStandardError : TStream;overload;static;
    class function OpenStandardError(bufferSize:Integer) : TStream;overload;static;

    class function OpenStandardInput : TStream;overload;static;
    class function OpenStandardInput(bufferSize:Integer) : TStream;overload;static;

    class function OpenStandardOutput : TStream;overload;static;
    class function OpenStandardOutput(bufferSize:Integer) : TStream;overload;static;

    class function Read : integer;static;
    class function ReadKey() : TConsoleKeyInfo;overload;static;
    class function ReadKey(intercept : boolean) : TConsoleKeyInfo;overload;static;

    class function ReadLine : String;static;

    class procedure ResetColor;static;

    public class procedure SetBufferSize(width, height : integer);overload;static;
    public class procedure SetBufferSize(Size : TCoord);overload;static;
    private class function GetBufferSize:TCoord;static;
    public class property  BufferSize:TCoord read GetBufferSize write SetBufferSize;

    class procedure SetCursorPosition(left, top : integer);overload;static;

    class procedure SetError(newError : TTextWriter);static;
    class procedure SetIn(newIn : TTextReader);static;
    class procedure SetOut(newOut : TTextWriter);static;

    class procedure SetWindowPosition(left, top : integer);static;
    class procedure SetWindowSize(width, height : integer);static;

    class procedure Write(value : Variant);overload;static;
    class procedure WriteLine(value : Variant);overload;static;
    class procedure WriteLine(value : Variant; Args:array of const);overload;static;

    private   function  GetBackgroundColor () : TConsoleColor;
    private   procedure SetBackgroundColor (value : TConsoleColor);
    public    property  BackgroundColor:TConsoleColor read GetBackgroundColor write SetBackgroundColor;

    private   function  GetBufferHeight () : integer;
    private   procedure SetBufferHeight (value : integer);
    public    property  BufferHeight:integer read GetBufferHeight write SetBufferHeight;

    private   function  GetBufferWidth:Integer;
    private   procedure SetBufferWidth(AValue:Integer);
    public    property  BufferWidth: Integer read GetBufferWidth write SetBufferWidth;


    private   procedure setCapsLock (aValue:Boolean);
    private   function  getCapsLock () : boolean;
    public    property  CapsLock:Boolean read getCapsLock write setCapsLock;

    private   function  getNumberLock () : boolean;
    public    property  NumberLock:Boolean read getNumberLock;

    private   function  GetCursorLeft:Integer;
    private   procedure SetCursorLeft(AValue:Integer);
    public    property  CursorLeft: Integer read GetCursorLeft write SetCursorLeft;

    private   function  GetCursorTop:Integer;
    private   procedure SetCursorTop(AValue:Integer);
    public    property  CursorTop: Integer read GetCursorTop write SetCursorTop;

    private   function  GetCursorSize:Integer;
    private   procedure SetCursorSize(AValue:Integer);
    public    property  CursorSize: Integer read GetCursorSize write SetCursorSize;

    private   function  GetCursorVisible:Boolean;
    private   procedure SetCursorVisible(AValue:Boolean);
    public    property  CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;

    private   function  GetError:TTextWriter;
    public    property  Error: TTextWriter read GetError;

    private   function  GetInReader:TTextReader;
    public    property  InReader: TTextReader read GetInReader;

    private   function  GetOut:TTextWriter;
    public    property  OutWriter: TTextWriter read GetOut;

    private   function  GetForegroundColor:TConsoleColor;
    private   procedure SetForegroundColor(AValue:TConsoleColor);
    public    property  ForegroundColor: TConsoleColor read GetForegroundColor write SetForegroundColor;

    private   function  GetKeyAvailable:Boolean;
    public    property  KeyAvailable: Boolean read GetKeyAvailable;

    private   function  GetLargestWindowHeight:Integer;
    public    property  LargestWindowHeight: Integer read GetLargestWindowHeight;

    private   function  GetLargestWindowWidth:Integer;
    public    property  LargestWindowWidth: Integer read GetLargestWindowWidth;

    private   function  GetTitle:String;
    private   procedure SetTitle(AValue:String);
    public    property  Title: String read GetTitle write SetTitle;

    private   function  GetTreatControlCAsInput:Boolean;
    private   procedure SetTreatControlCAsInput(AValue:Boolean);
    public    property  TreatControlCAsInput: Boolean read GetTreatControlCAsInput write SetTreatControlCAsInput;

    private   function  GetWindowHeight :Integer;
    private   procedure SetWindowHeight (AValue:Integer);
    public    property  WindowHeight : Integer read GetWindowHeight  write SetWindowHeight ;

    private   function  GetWindowLeft:Integer;
    private   procedure SetWindowLeft(AValue:Integer);
    public    property  WindowLeft: Integer read GetWindowLeft write SetWindowLeft;

    private   function  GetWindowTop:Integer;
    private   procedure SetWindowTop(AValue:Integer);
    public    property  WindowTop: Integer read GetWindowTop write SetWindowTop;

    private   function  GetWindowWidth:Integer;
    private   procedure SetWindowWidth(AValue:Integer);
    public    property  WindowWidth: Integer read GetWindowWidth write SetWindowWidth;

    class function GetBufferInfo:Windows._CONSOLE_SCREEN_BUFFER_INFO;static;

    class function  GetConsoleOutputHandle:THandle; static;
    class procedure SetConsoleOutputHandle(AValue:THandle); static;
    public         class property     ConsoleOutputHandle: THandle read GetConsoleOutputHandle write SetConsoleOutputHandle;

    private        function  GetConsoleInputHandle:Integer;
    private        procedure SetConsoleInputHandle(AValue:Integer);
    public         property     ConsoleInputHandle: Integer read GetConsoleInputHandle write SetConsoleInputHandle;

    class function ConsoleColorToColorAttribute(color: TConsoleColor; isBackground: boolean): TWinColor;static;
    class function ColorAttributeToConsoleColor(c: TWinColor): TConsoleColor;static;

    type TColorRange=0..15;
    private var FColors:array[TColorRange] of TWinColor;
    function GetColors(Index: TColorRange): TWinColor;
    procedure SetColors(Index: TColorRange; const Value: TWinColor);
    public property Colors[Index:TColorRange]:TWinColor read GetColors write SetColors;

  end;

function GetPassword(const InputMask: Char = '*'): string;
function GetNumber: Int64;

function NewTextOut(var T: TTextRec): Integer;
function Darken(const S:String):String;
function Brighten(const S:String):String;

procedure WriteLnC(const S: string); overload;
procedure WriteLnC(const S: string; Params: array of const ); overload;
procedure WriteC(const S: string); overload;
procedure WriteC(const I: integer); overload;inline;
procedure WriteC(const DateTime: TDateTime); overload;inline;
procedure WriteC(const S: string; Params: array of const ); overload;
function StripC(const S: string): string;

procedure SaveColor;
procedure RestoreColor;
procedure ShowText(const aString: string);
procedure ShowKeyValue(key, value: string);overload;
procedure ShowKeyValue(key:String; value: Integer);overload;
procedure ShowKeyValue(key:String; value: TDateTime);overload;
procedure ShowKeyValue(key:String; value: Boolean);overload;
procedure ShowError(const aString: string; Values: array of const ); overload;
procedure ShowError(const aString: string); overload;
procedure ShowError(const aException: Exception); overload;
procedure ShowInfo(const aString: string; Values: array of const ); overload;
procedure ShowInfo(const aString: string); overload;
procedure ShowWarning(const aString: string); overload;
procedure ShowWarning(const aString: string; Values: array of const ); overload;
procedure ShowSuccess(const aString: string); overload;
procedure ShowSuccess(const aString: string; Values: array of const ); overload;
function FormatSQL(S: string): string;
procedure Pause;
procedure AddBox(S:String;col:String);

var
  Console: TConsole;

implementation

uses StrUtils, DateUtils;

type
  PKey = ^TKey;

  TKey = record
    KeyCode: Smallint;
    Normal: Smallint;
    Shift: Smallint;
    Ctrl: Smallint;
    Alt: Smallint;
  end;

const
  CKeys: array [0 .. 88] of TKey = ((KeyCode: VK_BACK; Normal: $8; Shift: $8; Ctrl: $7F; Alt: $10E;), (KeyCode: VK_TAB; Normal: $9; Shift: $10F; Ctrl: $194; Alt: $1A5;
    ), (KeyCode: VK_RETURN; Normal: $D; Shift: $D; Ctrl: $A; Alt: $1A6), (KeyCode: VK_ESCAPE; Normal: $1B; Shift: $1B; Ctrl: $1B; Alt: $101), (KeyCode: VK_SPACE; Normal: $20; Shift: $20; Ctrl: $103;
    Alt: $20), (KeyCode: Ord('0'); Normal: Ord('0'); Shift: Ord(')'); Ctrl: - 1; Alt: $181), (KeyCode: Ord('1'); Normal: Ord('1'); Shift: Ord('!'); Ctrl: - 1; Alt: $178), (KeyCode: Ord('2');
    Normal: Ord('2'); Shift: Ord('@'); Ctrl: $103; Alt: $179), (KeyCode: Ord('3'); Normal: Ord('3'); Shift: Ord('#'); Ctrl: - 1; Alt: $17A), (KeyCode: Ord('4'); Normal: Ord('4'); Shift: Ord('$');
    Ctrl: - 1; Alt: $17B), (KeyCode: Ord('5'); Normal: Ord('5'); Shift: Ord('%'); Ctrl: - 1; Alt: $17C), (KeyCode: Ord('6'); Normal: Ord('6'); Shift: Ord('^'); Ctrl: $1E; Alt: $17D),
    (KeyCode: Ord('7'); Normal: Ord('7'); Shift: Ord('&'); Ctrl: - 1; Alt: $17E), (KeyCode: Ord('8'); Normal: Ord('8'); Shift: Ord('*'); Ctrl: - 1; Alt: $17F), (KeyCode: Ord('9'); Normal: Ord('9');
    Shift: Ord('('); Ctrl: - 1; Alt: $180), (KeyCode: Ord('A'); Normal: Ord('a'); Shift: Ord('A'); Ctrl: $1; Alt: $11E), (KeyCode: Ord('B'); Normal: Ord('b'); Shift: Ord('B'); Ctrl: $2; Alt: $130),
    (KeyCode: Ord('C'); Normal: Ord('c'); Shift: Ord('C'); Ctrl: $3; Alt: $12E), (KeyCode: Ord('D'); Normal: Ord('d'); Shift: Ord('D'); Ctrl: $4; Alt: $120), (KeyCode: Ord('E'); Normal: Ord('e');
    Shift: Ord('E'); Ctrl: $5; Alt: $112), (KeyCode: Ord('F'); Normal: Ord('f'); Shift: Ord('F'); Ctrl: $6; Alt: $121), (KeyCode: Ord('G'); Normal: Ord('g'); Shift: Ord('G'); Ctrl: $7; Alt: $122),
    (KeyCode: Ord('H'); Normal: Ord('h'); Shift: Ord('H'); Ctrl: $8; Alt: $123), (KeyCode: Ord('I'); Normal: Ord('i'); Shift: Ord('I'); Ctrl: $9; Alt: $117), (KeyCode: Ord('J'); Normal: Ord('j');
    Shift: Ord('J'); Ctrl: $A; Alt: $124), (KeyCode: Ord('K'); Normal: Ord('k'); Shift: Ord('K'); Ctrl: $B; Alt: $125), (KeyCode: Ord('L'); Normal: Ord('l'); Shift: Ord('L'); Ctrl: $C; Alt: $126),
    (KeyCode: Ord('M'); Normal: Ord('m'); Shift: Ord('M'); Ctrl: $D; Alt: $132), (KeyCode: Ord('N'); Normal: Ord('n'); Shift: Ord('N'); Ctrl: $E; Alt: $131), (KeyCode: Ord('O'); Normal: Ord('o');
    Shift: Ord('O'); Ctrl: $F; Alt: $118), (KeyCode: Ord('P'); Normal: Ord('p'); Shift: Ord('P'); Ctrl: $10; Alt: $119), (KeyCode: Ord('Q'); Normal: Ord('q'); Shift: Ord('Q'); Ctrl: $11; Alt: $110),
    (KeyCode: Ord('R'); Normal: Ord('r'); Shift: Ord('R'); Ctrl: $12; Alt: $113), (KeyCode: Ord('S'); Normal: Ord('s'); Shift: Ord('S'); Ctrl: $13; Alt: $11F), (KeyCode: Ord('T'); Normal: Ord('t');
    Shift: Ord('T'); Ctrl: $14; Alt: $114), (KeyCode: Ord('U'); Normal: Ord('u'); Shift: Ord('U'); Ctrl: $15; Alt: $116), (KeyCode: Ord('V'); Normal: Ord('v'); Shift: Ord('V'); Ctrl: $16; Alt: $12F),
    (KeyCode: Ord('W'); Normal: Ord('w'); Shift: Ord('W'); Ctrl: $17; Alt: $111), (KeyCode: Ord('X'); Normal: Ord('x'); Shift: Ord('X'); Ctrl: $18; Alt: $12D), (KeyCode: Ord('Y'); Normal: Ord('y');
    Shift: Ord('Y'); Ctrl: $19; Alt: $115), (KeyCode: Ord('Z'); Normal: Ord('z'); Shift: Ord('Z'); Ctrl: $1A; Alt: $12C), (KeyCode: VK_PRIOR; Normal: $149; Shift: $149; Ctrl: $184; Alt: $199),
    (KeyCode: VK_NEXT; Normal: $151; Shift: $151; Ctrl: $176; Alt: $1A1), (KeyCode: VK_END; Normal: $14F; Shift: $14F; Ctrl: $175; Alt: $19F), (KeyCode: VK_HOME; Normal: $147; Shift: $147; Ctrl: $177;
    Alt: $197), (KeyCode: VK_LEFT; Normal: $14B; Shift: $14B; Ctrl: $173; Alt: $19B), (KeyCode: VK_UP; Normal: $148; Shift: $148; Ctrl: $18D; Alt: $198), (KeyCode: VK_RIGHT; Normal: $14D; Shift: $14D;
    Ctrl: $174; Alt: $19D), (KeyCode: VK_DOWN; Normal: $150; Shift: $150; Ctrl: $191; Alt: $1A0), (KeyCode: VK_INSERT; Normal: $152; Shift: $152; Ctrl: $192; Alt: $1A2), (KeyCode: VK_DELETE;
    Normal: $153; Shift: $153; Ctrl: $193; Alt: $1A3), (KeyCode: VK_NUMPAD0; Normal: Ord('0'); Shift: $152; Ctrl: $192; Alt: - 1), (KeyCode: VK_NUMPAD1; Normal: Ord('1'); Shift: $14F; Ctrl: $175;
    Alt: - 1), (KeyCode: VK_NUMPAD2; Normal: Ord('2'); Shift: $150; Ctrl: $191; Alt: - 1), (KeyCode: VK_NUMPAD3; Normal: Ord('3'); Shift: $151; Ctrl: $176; Alt: - 1), (KeyCode: VK_NUMPAD4;
    Normal: Ord('4'); Shift: $14B; Ctrl: $173; Alt: - 1), (KeyCode: VK_NUMPAD5; Normal: Ord('5'); Shift: $14C; Ctrl: $18F; Alt: - 1), (KeyCode: VK_NUMPAD6; Normal: Ord('6'); Shift: $14D; Ctrl: $174;
    Alt: - 1), (KeyCode: VK_NUMPAD7; Normal: Ord('7'); Shift: $147; Ctrl: $177; Alt: - 1), (KeyCode: VK_NUMPAD8; Normal: Ord('8'); Shift: $148; Ctrl: $18D; Alt: - 1), (KeyCode: VK_NUMPAD9;
    Normal: Ord('9'); Shift: $149; Ctrl: $184; Alt: - 1), (KeyCode: VK_MULTIPLY; Normal: Ord('*'); Shift: Ord('*'); Ctrl: $196; Alt: $137), (KeyCode: VK_ADD; Normal: Ord('+'); Shift: Ord('+');
    Ctrl: $190; Alt: $14E), (KeyCode: VK_SUBTRACT; Normal: Ord('-'); Shift: Ord('-'); Ctrl: $18E; Alt: $14A), (KeyCode: VK_DECIMAL; Normal: Ord('.'); Shift: Ord('.'); Ctrl: $153; Alt: $193),
    (KeyCode: VK_DIVIDE; Normal: Ord('/'); Shift: Ord('/'); Ctrl: $195; Alt: $1A4), (KeyCode: VK_F1; Normal: $13B; Shift: $154; Ctrl: $15E; Alt: $168), (KeyCode: VK_F2; Normal: $13C; Shift: $155;
    Ctrl: $15F; Alt: $169), (KeyCode: VK_F3; Normal: $13D; Shift: $156; Ctrl: $160; Alt: $16A), (KeyCode: VK_F4; Normal: $13E; Shift: $157; Ctrl: $161; Alt: $16B), (KeyCode: VK_F5; Normal: $13F;
    Shift: $158; Ctrl: $162; Alt: $16C), (KeyCode: VK_F6; Normal: $140; Shift: $159; Ctrl: $163; Alt: $16D), (KeyCode: VK_F7; Normal: $141; Shift: $15A; Ctrl: $164; Alt: $16E), (KeyCode: VK_F8;
    Normal: $142; Shift: $15B; Ctrl: $165; Alt: $16F), (KeyCode: VK_F9; Normal: $143; Shift: $15C; Ctrl: $166; Alt: $170), (KeyCode: VK_F10; Normal: $144; Shift: $15D; Ctrl: $167; Alt: $171),
    (KeyCode: VK_F11; Normal: $185; Shift: $187; Ctrl: $189; Alt: $18B), (KeyCode: VK_F12; Normal: $186; Shift: $188; Ctrl: $18A; Alt: $18C), (KeyCode: $DC; Normal: Ord('\'); Shift: Ord('|');
    Ctrl: $1C; Alt: $12B), (KeyCode: $BF; Normal: Ord('/'); Shift: Ord('?'); Ctrl: - 1; Alt: $135), (KeyCode: $BD; Normal: Ord('-'); Shift: Ord('_'); Ctrl: $1F; Alt: $182), (KeyCode: $BB;
    Normal: Ord('='); Shift: Ord('+'); Ctrl: - 1; Alt: $183), (KeyCode: $DB; Normal: Ord('['); Shift: Ord('{'); Ctrl: $1B; Alt: $11A), (KeyCode: $DD; Normal: Ord(']'); Shift: Ord('}'); Ctrl: $1D;
    Alt: $11B), (KeyCode: $BA; Normal: Ord(';'); Shift: Ord(':'); Ctrl: - 1; Alt: $127), (KeyCode: $DE; Normal: Ord(''''); Shift: Ord('"'); Ctrl: - 1; Alt: $128), (KeyCode: $BC; Normal: Ord(',');
    Shift: Ord('<'); Ctrl: - 1; Alt: $133), (KeyCode: $BE; Normal: Ord('.'); Shift: Ord('>'); Ctrl: - 1; Alt: $134), (KeyCode: $C0; Normal: Ord('`'); Shift: Ord('~'); Ctrl: - 1; Alt: $129));

var
  ExtendedChar: Char = #0;

function FindKeyCode(KeyCode: Smallint): PKey; {$IFDEF INLINES}inline; {$ENDIF}
var
  I: Integer;
begin
  for I := 0 to high(CKeys) do
    if CKeys[I].KeyCode = KeyCode then
    begin
      Result := @CKeys[I];
      Exit;
    end;
  Result := nil;
end;

// This has a complexity of 11, because of the if else ladder.
// That bugs me a bit. Looking for something more elegant.
function TranslateKey(const Rec: TInputRecord; State: Integer; key: PKey; KeyCode: Integer): Smallint;
begin
  if State and (RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED) <> 0 then
    Result := key^.Alt
  else if State and (RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED) <> 0 then
    Result := key^.Ctrl
  else if State and SHIFT_PRESSED <> 0 then
    Result := key^.Shift
  else if KeyCode in [Ord('A') .. Ord('Z')] then
    Result := Ord(Rec.Event.KeyEvent.AsciiChar)
  else
    Result := key^.Normal;
end;

function ConvertKey(const Rec: TInputRecord; key: PKey): Smallint;
{$IFDEF INLINES}inline; {$ENDIF}
begin
  if Assigned(key) then
    Result := TranslateKey(Rec, Rec.Event.KeyEvent.dwControlKeyState, key, Rec.Event.KeyEvent.wVirtualKeyCode)
  else
    Result := -1
end;

function TConsole.ReadKeyChar: Char;
var
  InputRec: TInputRecord;
  NumRead : Cardinal;
  KeyMode : DWORD;
  KeyCode : Smallint;
begin
  if ExtendedChar <> #0 then
  begin
    Result       := ExtendedChar;
    ExtendedChar := #0;
    Exit;
  end
  else
  begin
    Result := #$FF;
    GetConsoleMode(StdIn, KeyMode);
    SetConsoleMode(StdIn, 0);
    repeat
      ReadConsoleInput(StdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          // From Delphi 2009 on, Result is WideChar
          Result := Chr(Ord(InputRec.Event.KeyEvent.AsciiChar));
          Break;
        end;
        KeyCode := ConvertKey(InputRec, FindKeyCode(InputRec.Event.KeyEvent.wVirtualKeyCode));
        if KeyCode > $FF then
        begin
          ExtendedChar := Chr(KeyCode and $FF);
          Result       := #0;
          Break;
        end;
      end;
    until False;
    SetConsoleMode(StdIn, KeyMode);
  end;
end;

function TConsole.KeyPressed: Boolean;
var
  InputRecArray: array of TInputRecord;
  NumRead      : DWORD;
  NumEvents    : DWORD;
  I            : Integer;
  KeyCode      : Word;
begin
  Result := False;
  GetNumberOfConsoleInputEvents(StdIn, NumEvents);
  if NumEvents = 0 then
    Exit;
  SetLength(InputRecArray, NumEvents);
  PeekConsoleInput(StdIn, InputRecArray[0], NumEvents, NumRead);
  for I := 0 to high(InputRecArray) do
  begin
    if (InputRecArray[I].EventType and KEY_EVENT <> 0) and InputRecArray[I].Event.KeyEvent.bKeyDown then
    begin
      KeyCode := InputRecArray[I].Event.KeyEvent.wVirtualKeyCode;
      if not(KeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL]) then
      begin
        if ConvertKey(InputRecArray[I], FindKeyCode(KeyCode)) <> -1 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TConsole.TextColor(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (TextAttr and $F0) or (Color and $0F);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TConsole.TextBackground(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TConsole.LowVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr and $F7;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TConsole.HighVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr or $08;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TConsole.NormVideo;
begin
  TextAttr := DefaultAttr;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

// The following functions are independent of TextWindow.

function TConsole.GetCursorX: Integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(StdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.X;
end;

function TConsole.GetCursorY: Integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(StdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.Y;
end;

procedure TConsole.SetCursorPos(X, Y: Smallint);
var
  NewPos: TCoord;
begin
  NewPos.X := X;
  NewPos.Y := Y;
  SetConsoleCursorPosition(StdOut, NewPos);
end;

// The following functions are relative to TextWindow.

procedure TConsole.ClrScr;
var
  StartPos       : TCoord;
  Len, NumWritten: DWORD;
  I              : Integer;
begin
  if (TextWindow.Left = 0) and (TextWindow.Top = 0) and (TextWindow.Right = BufferSize.X - 1) and (TextWindow.Bottom = BufferSize.Y - 1) then
  begin
    StartPos.X := 0;
    StartPos.Y := 0;
    Len        := BufferSize.X * BufferSize.Y;
    FillConsoleOutputCharacterA(StdOut, ' ', Len, StartPos, NumWritten);
    FillConsoleOutputAttribute(StdOut, TextAttr, Len, StartPos, NumWritten);
    if NumWritten < Len then
    begin
      ScreenSize.X := ScreenWidth;
      ScreenSize.Y := ScreenHeight;
    end;
  end
  else
  begin
    Len        := TextWindow.Right - TextWindow.Left + 1;
    StartPos.X := TextWindow.Left;
    for I      := TextWindow.Top to TextWindow.Bottom do
    begin
      StartPos.Y := I;
      FillConsoleOutputCharacterA(StdOut, ' ', Len, StartPos, NumWritten);
      FillConsoleOutputAttribute(StdOut, TextAttr, Len, StartPos, NumWritten);
    end;
  end;
  GotoXY(1, 1);
end;

procedure TConsole.GotoXY(X, Y: Smallint);
begin
  Inc(X, TextWindow.Left - 1);
  Inc(Y, TextWindow.Top - 1);
  if (X >= TextWindow.Left) and (X <= TextWindow.Right) and (Y >= TextWindow.Top) and (Y <= TextWindow.Bottom) then
    SetCursorPos(X, Y);
end;

procedure TConsole.ClrEol;
var
  Len       : Integer;
  Pos       : TCoord;
  NumWritten: DWORD;
begin
{$IFDEF CONSOLE}
  Pos.X := GetCursorX;
  if Pos.X > TextWindow.Right then
    Exit;

  Pos.Y := GetCursorY;
  Len   := TextWindow.Right - GetCursorX + 1;
  FillConsoleOutputCharacterA(StdOut, ' ', Len, Pos, NumWritten);
  FillConsoleOutputAttribute(StdOut, TextAttr, Len, Pos, NumWritten);
{$ENDIF}
end;

procedure TConsole.Scroll(Left, Top, Right, Bottom: Integer; Distance: Integer = 0);
var
  Rect  : TSmallRect;
  Fill  : TCharInfo;
  NewPos: TCoord;
begin
  Fill.AsciiChar  := ' ';
  Fill.Attributes := TextAttr;
  if Distance = 0 then
    Distance  := Bottom - Top + 1;
  Rect.Left   := Left;
  Rect.Right  := Right;
  Rect.Top    := Top;
  Rect.Bottom := Bottom;
  NewPos.X    := Left;
  NewPos.Y    := Top + Distance;
  ScrollConsoleScreenBufferA(StdOut, Rect, @Rect, NewPos, Fill);
end;

procedure TConsole.InsLine;
begin
  Scroll(TextWindow.Left, GetCursorY, TextWindow.Right, TextWindow.Bottom, 1);
end;

function TConsole.IsOutputRedirected: Boolean;
var
  StdOutHandle    : THandle;
  bIsNotRedirected: Boolean;
  FileInfo        : TByHandleFileInformation;
begin
  StdOutHandle     := GetStdHandle(STD_OUTPUT_HANDLE);
  bIsNotRedirected := (not GetFileInformationByHandle(StdOutHandle, FileInfo) and (GetLastError = ERROR_INVALID_HANDLE));
  Result           := (not bIsNotRedirected);
end;

procedure TConsole.DelLine;
begin
  Scroll(TextWindow.Left, GetCursorY, TextWindow.Right, TextWindow.Bottom, -1);
end;

function TConsole.Validate(X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := (X1 < X2) and (Y1 < Y2) and (X1 >= 0) and (X2 < BufferSize.X) and (Y1 >= 0) and (Y2 < BufferSize.Y);
end;

procedure TConsole.WriteText(Line: PAnsiChar; Len: Integer);
var
  NumWritten: DWORD;
begin
  SetConsoleTextAttribute(StdOut, TextAttr);
  WriteConsoleA(StdOut, Line, Len, NumWritten, nil);
end;

// Replacement for TTextRec.InOutFunc and TTextRec.FlushFunc for the Output
// and ErrOutput pseudo-textfiles.
// This is generally only used if a text window is set, otherwise this is
// handled by the runtime library.
function NewTextOut(var T: TTextRec): Integer;
var
  ReadPtr, WritePtr: PAnsiChar;
  Line             : AnsiString;
  DistanceToEdge   : Integer;

  // Moves cursor to start of line, updates DistanceToEdge.
  procedure CarriageReturn;
  begin
    SetCursorPos(Console.TextWindow.Left, Console.GetCursorY);
    DistanceToEdge := Console.TextWindow.Right - Console.TextWindow.Left + 1;
  end;

// Moves cursor down one line. If necessary, scrolls window.
  procedure LineFeed; {$IFDEF INLINES}inline; {$ENDIF}
  begin
    if Console.GetCursorY < Console.TextWindow.Bottom then
      SetCursorPos(Console.GetCursorX, Console.GetCursorY + 1)
    else
      Console.Scroll(Console.TextWindow.Left, Console.TextWindow.Top, Console.TextWindow.Right, Console.TextWindow.Bottom, -1);
  end;

// Store one char in write buffer.
  procedure CharToWriteBuffer(C: AnsiChar);
  begin
    WritePtr^ := C;
    Inc(WritePtr);
    Dec(DistanceToEdge);
  end;

// True if at right edge of window.
  function WriteLine: Boolean;
  begin
    WritePtr^ := #0;
    Console.WriteText(PAnsiChar(Line), WritePtr - PAnsiChar(Line));
    Result         := DistanceToEdge = 0;
    WritePtr       := PAnsiChar(Line);
    DistanceToEdge := Console.TextWindow.Right - Console.TextWindow.Left + 1;
  end;

// Converts tabs to spaces, since WriteConsole will do its own tabbing when
// it encounters a #9, which is of course independent of this unit's
// TextWindow settings.
  procedure ProcessTab;
  var
    Num, I: Integer;
  begin
    Num := 8 - (WritePtr - PAnsiChar(Line)) mod 8;
    if Num > DistanceToEdge then
      Num := DistanceToEdge;
    for I := 1 to Num do
      CharToWriteBuffer(' ');
  end;

begin
  SetLength(Line, Console.BufferSize.X); // Line only contains one line of windowed text.
  WritePtr       := PAnsiChar(Line);
  ReadPtr        := T.BufPtr;
  DistanceToEdge := Console.TextWindow.Right - Console.GetCursorX + 1;
  while T.BufPos > 0 do
  begin
    while (T.BufPos > 0) and (DistanceToEdge > 0) do
    begin
      case ReadPtr^ of
        #7:
          Windows.Beep(800, 200); // this is what my internal speaker uses.
        #8:
          begin
            Dec(WritePtr);
            Inc(DistanceToEdge);
          end;
        #9:
          ProcessTab;
        // LineFeed is not just a line feed, it takes the function of #13#10
        #10:
          begin
            WriteLine;
            CarriageReturn;
            LineFeed;
          end;
        #13:
          begin
            WriteLine;
            CarriageReturn;
          end;
      else
        CharToWriteBuffer(ReadPtr^);
      end;
      Inc(ReadPtr);
      Dec(T.BufPos);
    end;
    if WriteLine then
    begin
      CarriageReturn;
      // If TexWindow.Right is at the edge of the screen, WriteConsole will
      // already do a linefeed.
      if Console.TextWindow.Right <> Console.ScreenWidth - 1 then
        LineFeed;
    end;
  end;
  Result := 0;
end;

var
  OldInOutFunc: Pointer;
  OldFlushFunc: Pointer;

procedure TConsole.Window(Left, Top, Right, Bottom: Integer);
begin
  Dec(Left);
  Dec(Top);
  Dec(Right);
  Dec(Bottom);
  if Validate(Left, Top, Right, Bottom) then
  begin
    FTextWindow.Left   := Left;
    FTextWindow.Top    := Top;
    FTextWindow.Right  := Right;
    FTextWindow.Bottom := Bottom;
    if (Left > 0) or (Top > 0) or (Right < BufferSize.X - 1) or (Bottom < BufferSize.Y - 1) then
    // Text must be contained in window
    begin
      OldInOutFunc               := TTextRec(Output).InOutFunc;
      OldFlushFunc               := TTextRec(Output).FlushFunc;
      TTextRec(Output).InOutFunc := @NewTextOut;
      TTextRec(Output).FlushFunc := @NewTextOut;
      SetCursorPos(Left, Top);
    end;
  end
  else
  begin
    FTextWindow.Left   := 0;
    FTextWindow.Right  := BufferSize.X - 1;
    FTextWindow.Top    := 0;
    FTextWindow.Bottom := BufferSize.Y - 1;
    SetCursorPos(0, 0);
    if Assigned(OldInOutFunc) then
    begin
      TTextRec(Output).InOutFunc := OldInOutFunc;
      OldInOutFunc               := nil;
    end;
    if Assigned(OldFlushFunc) then
    begin
      TTextRec(Output).FlushFunc := OldFlushFunc;
      OldFlushFunc               := nil;
    end;
  end;
  WindMin := (TextWindow.Left and $FF) or (TextWindow.Top and $FF) shl 8;
  WindMax := (TextWindow.Right and $FF) or (TextWindow.Bottom and $FF) shl 8;
end;

procedure HardwareSound(Frequency: Smallint);
asm
  CMP     AX,37
  JB      @@1
  MOV     CX,AX
  MOV     AL,$B6
  OUT     $43,AL
  MOV     AX,$3540
  MOV     DX,$0012
  DIV     CX
  OUT     $42,AL
  MOV     AL,AH
  OUT     $42,AL
  MOV     AL,3
  OUT     $61,AL
@@1:
end;

procedure HardwareNoSound;
asm
  MOV     AL,0
  OUT     $61,AL
end;

procedure HardwareDelay(Millisecs: Integer);
begin
  Sleep(Millisecs);
end;

procedure HardwareBeep(Frequency, Duration: Smallint);
begin
  Console.Sound(Frequency);
  Console.Delay(Duration);
  Console.NoSound;
end;

type
  TSoundState = (ssPending, ssPlaying, ssFreed);

var
  CurrentFrequency: Integer;
  SoundState      : TSoundState;

  // On Windows NT and later, direct port access is prohibited, so there is
  // no way to use HardwareSound and HardwareNoSound.
  //
  // Since probably every note played by Sound will be delimited by some kind
  // of Delay, the playing of the note is deferred to Delay. Sound only stores
  // the frequency and sets the SoundState to ssPending. Delay now knows both
  // parameters, and can use Windows.Beep.
  //
  // Note that such code is not reentrant.

procedure SoftwareSound(Frequency: Smallint);
begin
  // $123540 div Frequency must be <= $7FFF, so Frequency must be >= 37.
  if Frequency >= 37 then
  begin
    CurrentFrequency := Frequency;
    SoundState       := TSoundState.ssPending;
  end;
end;

procedure SoftwareDelay(Millisecs: Integer);
begin
  if SoundState = TSoundState.ssPending then
  begin
    SoundState := TSoundState.ssPlaying;
    Windows.Beep(CurrentFrequency, Millisecs);
    SoundState := TSoundState.ssFreed;
  end
  else
    Sleep(Millisecs);
end;

procedure SoftwareBeep(Frequency, Duration: Smallint);
begin
  if Frequency >= 37 then
  begin
    SoundState := TSoundState.ssPlaying;
    Windows.Beep(Frequency, Duration);
    SoundState := TSoundState.ssFreed;
  end;
end;

procedure SoftwareNoSound;
begin
  Windows.Beep(CurrentFrequency, 0);
  SoundState := TSoundState.ssFreed;
end;

function TConsole.WhereX: Integer;
begin
  Result := GetCursorX - TextWindow.Left + 1;
end;

function TConsole.WhereY: Integer;
begin
  Result := GetCursorY - TextWindow.Top + 1;
end;

procedure TConsole.GetScreenSizes(var Width, Height: Smallint);
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(StdOut, BufferInfo);
  Width  := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
  Height := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
end;

function TConsole.ScreenWidth: Smallint;
var
  Height: Smallint;
begin
  GetScreenSizes(Result, Height);
end;

function TConsole.ScreenHeight: Smallint;
var
  Width: Smallint;
begin
  GetScreenSizes(Width, Result);
end;


function TConsole.TextColor: Byte;
begin
  Result := TextAttr and $0F;
end;

function TConsole.TextBackground: Byte;
begin
  Result := (TextAttr and $F0) shr 4;
end;

procedure TConsole.TextMode(Mode: Word);
begin
  Window(0, 0, 0, 0);
  NormVideo;
end;

procedure TConsole.InitScreenMode;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  Reset(Input);
  Rewrite(Output);
  StdIn  := TTextRec(Input).Handle;
  StdOut := TTextRec(Output).Handle;
{$IFDEF HASERROUTPUT}
  Rewrite(ErrOutput);
  StdErr := TTextRec(ErrOutput).Handle;
{$ELSE}
  StdErr := GetStdHandle(STD_ERROR_HANDLE);
{$ENDIF}
  if not GetConsoleSCreenBufferInfo(StdOut, BufferInfo) then
  begin
    SetInOutRes(GetLastError);
    Exit;
  end;
  FTextWindow.Left   := 0;
  FTextWindow.Top    := 0;
  FTextWindow.Right  := BufferInfo.dwSize.X - 1;
  FTextWindow.Bottom := BufferInfo.dwSize.Y - 1;
  TextAttr           := BufferInfo.wAttributes and $FF;
  DefaultAttr        := TextAttr;
  BufferSize         := BufferInfo.dwSize;
  ScreenSize.X       := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
  ScreenSize.Y       := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
  WindMin            := 0;
  WindMax            := (ScreenSize.X and $FF) or (ScreenSize.Y and $FF) shl 8;
  LastMode           := TConsoleTextModes.CO80;
  OldInOutFunc       := nil;
  OldFlushFunc       := nil;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Sound   := SoftwareSound;
    NoSound := SoftwareNoSound;
    Delay   := SoftwareDelay;
    BeepProc:= SoftwareBeep;
  end
  else
  begin
    Sound   := HardwareSound;
    NoSound := HardwareNoSound;
    Delay   := HardwareDelay;
    BeepProc:= HardwareBeep;
  end;
end;

procedure WriteC(const S: string);
const
  ColorChars: array [0 .. 15] of Char = 'zbgcrmywZBGCRMYW';
  FGEscapeChar                        = '^';
  BGEscapeChar                        = '~';
var
  I: Integer;
begin
{$IFDEF CONSOLE}
  if Console.IsOutputRedirected then
  begin
    write(StripC(S));
    Exit;
  end;

  I := 1;
  while I <= Length(S) do
  begin
    if (I < Length(S)) and (S[I] = FGEscapeChar) then
    begin
      Inc(I);
      if S[I] <> FGEscapeChar then
        Console.TextColor(Pos(S[I], ColorChars) - 1)
      else
        write(S[I]);
    end
    else if (I < Length(S)) and (S[I] = BGEscapeChar) then
    begin
      Inc(I);
      if S[I] <> BGEscapeChar then
        Console.TextBackground(Pos(S[I], ColorChars) - 1)
      else
        write(S[I]);
    end
    else
      write(S[I]);
    Inc(I);
  end;
{$ENDIF}
end;

procedure WriteC(const I: integer); overload;
begin
  WriteC(IntToSTr(I));
end;


procedure WriteC(const DateTime: TDateTime); overload;inline;
begin
  WriteC('^W%0.04d^w-^W%0.02d^w-^W%0.02d ^W%0.02d^w:^W%0.02d^w:^W%0.02d',[
    YearOf(dateTime),
    MonthOf(dateTime),
    DayOf(dateTime),
    HourOf(dateTime),
    MinuteOf(dateTime),
    SecondOf(dateTime)
  ]);
end;

function StripC(const S: string): string;
{$IFDEF CONSOLE}
const
  FGEscapeChar = '^';
  BGEscapeChar = '~';
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF CONSOLE}
  I      := 1;
  Result := '';
  while I <= Length(S) do
  begin
    if (I < Length(S)) and (S[I] = FGEscapeChar) then
    begin
      Inc(I);
      if S[I] <> FGEscapeChar then
      begin
        //
      end
      else
        Result := Result + S[I];
    end
    else if (I < Length(S)) and (S[I] = BGEscapeChar) then
    begin
      Inc(I);
      if S[I] <> BGEscapeChar then
      begin
        //
      end
      else
        Result := Result + S[I];
    end
    else
      Result := Result + S[I];
    Inc(I);
  end;

{$ENDIF}
end;

procedure WriteC(const S: string; Params: array of const ); overload;
begin
  WriteC(Format(S, Params));
end;

procedure WriteLnC(const S: string);
begin
{$IFDEF CONSOLE}
  WriteC(S);
  Console.ClrEol;
  WriteLn;
{$ENDIF}
end;

procedure WriteLnC(const S: string; Params: array of const ); overload;
begin
  WriteLnC(Format(S, Params));
end;

var
  OldColor: Byte;

procedure SaveColor;
begin
  OldColor := Console.TextAttr;
end;

procedure RestoreColor;
begin
  Console.TextBackground((OldColor and $F0) shr 4);
  Console.TextColor(OldColor and $0F);
end;

procedure ShowText(const aString: string);
begin
  WriteLnC('^w~z' + aString + '^w~z');
  Console.ClrEol;
end;

procedure ShowError(const aString: string; Values: array of const ); overload;
begin
  ShowError(Format(aString, Values));
end;

procedure ShowError(const aString: string); overload;
begin
  WriteLnC('^R~rERROR:   ^W' + aString + '^w~z');
  Console.ClrEol;
end;

procedure ShowError(const aException: Exception); overload;
begin
  ShowError(aException.ClassName + '^W: ^Y' + aException.Message);
end;

procedure ShowInfo(const aString: string; Values: array of const ); overload;
begin
  ShowInfo(Format(aString, Values));
end;

procedure ShowInfo(const aString: string); overload;
begin
  WriteLnC('^B~bINFO:    ^W' + aString + '^w~z');
end;

procedure ShowWarning(const aString: string);
begin
  WriteLnC('^M~mWARNING: ^W' + aString + '^w~z');
end;

procedure ShowWarning(const aString: string; Values: array of const ); overload;
begin
  ShowWarning(Format(aString, Values));
end;

procedure ShowSuccess(const aString: string); overload;
begin
  WriteLnC('^G~gOK:      ^W' + aString + '^w~z');
end;

procedure ShowSuccess(const aString: string; Values: array of const ); overload;
begin
  ShowSuccess(Format(aString, Values));
end;

procedure ShowKeyValue(key, value: string);overload;
begin
  WriteLnC('^W%15s^w : ^W%s~z^w', [key, value]);
end;

procedure ShowKeyValue(key:String; value: Integer);overload;
begin
  ShowKeyValue(Key,IntToStr(Value));
end;
procedure ShowKeyValue(key:String; value: TDateTime);overload;
begin
  ShowKeyValue(Key,FormatDateTime('yyyy-mm-dd hh:nn:ss',Value));
end;

procedure ShowKeyValue(key:String; value: Boolean);overload;
begin
  if Value then
    ShowKeyValue(Key,'True')
  else
    ShowKeyValue(Key,'False')
end;


function HighLightFormatStr(const S:String):String;
begin
  Result := S;
  Result := ReplaceStr(Result,'%s','^W%s^w');
  Result := ReplaceStr(Result,'%d','^M%s^w');
  Result := ReplaceStr(Result,'%f','^C%s^w');
  Result := ReplaceStr(Result,'%g','^C%s^w');
  Result := ReplaceStr(Result,'[','^w[^Y');
  Result := ReplaceStr(Result,']','^w]');
  Result := ReplaceStr(Result,'[Error]','^w[^YERROR^w]');
end;

function FormatSQL(S: string): string;
const
  Keywords : array [0 .. 27] of string = ('call', 'delete', 'create', 'select', 'from', 'where', 'limit', 'join', 'left', 'having', 'group by', 'order by', 'table', ' as ', ' on ', ' and ', ' or ', 'between', 'set ', 'ignore','load','into', 'infile', 'load','fields terminated by', 'lines terminated by', 'table', 'character set');
  Special  : array [0 .. 11] of string = ('''', '"', '`', '(', ')', '<', '>', '[', ']', '.', ',', ';');
  Quotes   : array [0 ..  1] of string = ('"', '''');
  BackTicks: array [0 ..  0] of string = ('`');

var
  tmp: string;

  procedure ColorQuotes(var S2: string; aQuoteStr: Char; aColor: string);
  var
    InQuote: Boolean;
    I      : Integer;
  begin
    I       := 1;
    InQuote := False;
    while I < Length(S2) do
    begin
      if S2[I] = aQuoteStr then
      begin
        InQuote := not InQuote;
        if InQuote then
          System.Insert(aColor, S2, I + 1);
      end;
      Inc(I);
    end;
  end;

  procedure ColorNums(var S2: string; aColor: string);
  var
    InQuote: Boolean;
    I      : Integer;
    Str:String;
    Start:Integer;
    Val:Int64;
  begin
    I       := 1;
    InQuote := False;
    Str := '';
    Start := 0;
    while I < Length(S2)-1 do
    begin
      if ((S2[I]=' ')
      or (S2[I]='(')
      or (S2[I]=')')
      or (S2[I]='[')
      or (S2[I]=']')
      or (S2[I]='''')
      or (S2[I]='"')
      or (S2[I]=',')
      or (S2[I]=';')
      or (S2[I]='/')
      or (S2[I]='*')
      or (S2[I]='+')
      or (S2[I]='-')
      or (S2[I]='<')
      or (S2[I]='>')
      or (S2[I]=#9)
      or (S2[I]=#13)
      or (S2[I]=#10))

      and
        ((S2[I+1]<>' ')
      or (S2[I+1]<>'(')
      or (S2[I+1]<>')')
      or (S2[I+1]<>'[')
      or (S2[I+1]<>']')
      or (S2[I+1]<>'''')
      or (S2[I+1]<>'"')
      or (S2[I+1]<>',')
      or (S2[I+1]<>';')
      or (S2[I+1]<>'/')
      or (S2[I+1]<>'*')
      or (S2[I+1]<>'+')
      or (S2[I+1]<>'-')
      or (S2[I+1]<>'<')
      or (S2[I+1]<>'>')
      or (S2[I+1]<>#9)
      or (S2[I+1]<>#13)
      or (S2[I+1]<>#10))

      then
      begin
        InQuote := not InQuote;
        if InQuote then
        begin
          Start := I+1;
        end
        else
        begin
          Str := Trim(Str);
          if Str<>'' then
            if TryStrToInt64(Str,Val) then
            begin
              System.Insert(aColor, S2, Start);
              Inc(I,Length(aColor));
            end;
          Str := '';
        end;
      end;
      if InQuote then
        Str := Str + S2[I];
      Inc(I);
    end;
  end;

begin
  Result := S;

  ColorNums  (Result, '^B');
  ColorQuotes(Result, '"', '^g');
  ColorQuotes(Result, '''', '^g');
  ColorQuotes(Result, '`', '^R');

  for tmp in Keywords do
    Result := ReplaceStr(Result, tmp, '^C' + uppercase(tmp) + '^w');

  for tmp in Keywords do
    Result := ReplaceStr(Result, uppercase(tmp), '^C' + uppercase(tmp) + '^w');

  for tmp in Special do
    Result := ReplaceStr(Result, tmp, '^W' + uppercase(tmp) + '^w');

  for tmp in Special do
    Result := ReplaceStr(Result, uppercase(tmp), '^W' + uppercase(tmp) + '^w');

end;

procedure Pause;
begin
  WriteLn('Press any key to continue...');
  ReadLn;
end;

function Darken(const S:String):String;
var
  i:integer;
begin
  Result := S;
  for i := 2 to Length(result) do
  begin
//    if (i>2) and (not (Result[i-2] in ['^','~'])) then
      if Result[i-1] in ['^','~'] then
      begin
        if Result[I] in ['w'] then
          Result[I] := 'Z'
        else
          if Result[I] in ['Z','B','G','C','R','M','Y','W'] then
            Result[I] := Lowercase(Result[I])[1];

        if Result[i-1] in ['~'] then
          if Result[I] in ['z','b','g','c','r','m','y','w'] then
            Result[I] := 'z';
      end;
  end;
end;

function Brighten(const S:String):String;
var
  i:integer;
begin
  Result := S;
  for i := 2 to Length(result) do
  begin
    if (i>2) and (not (Result[i-2] in ['^','~'])) then
      if Result[i-1] in ['^','~'] then
      begin
        if Result[I] in ['Z','B','G','C','R','M','Y','W'] then
          Result[I] := Uppercase(Result[I])[1];

      end;
  end;
end;

constructor TConsoleKeyInfo.Create(keyChar: Char; key: TConsoleKey; shift, alt, control: boolean);
begin
  if ((key < TConsoleKey(0)) or (key > TConsoleKey($ff))) then
      raise ERangeError.Create('ConsoleKey out of range');

  FKeyChar := keyChar;
  FKey := key;
  FMods := [];
  if shift   then Include(FMods,TConsoleModifiers.Shift);
  if alt     then Include(FMods,TConsoleModifiers.Shift);
  if control then Include(FMods,TConsoleModifiers.Control);
end;

function TConsoleKeyInfo.GetKey: TConsoleKey;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsoleKeyInfo.GetKeyChar: Char;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsoleKeyInfo.GetModifiers: TConsoleModifiersSet;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

{ TConsole }

procedure TConsole.Beep(frequency, duration: Integer);
begin
{
  if ((frequency < $25) or (frequency > $7fff)) then
      raise ArgumentOutOfRangeException.Create('frequency', frequency, Environment.GetResourceString('ArgumentOutOfRange_BeepFrequency', New(array[2] of TObject, ( ( $25, $7fff ) ))));
  if (duration <= 0) then
      raise ArgumentOutOfRangeException.Create('duration', duration, Environment.GetResourceString('ArgumentOutOfRange_NeedPosNum'));
}
  Windows.Beep(frequency, duration)
end;

procedure TConsole.Beep;
begin
  self.Beep(800, 200)
end;

procedure TConsole.Clear;
begin
  ClrScr;
end;

class function TConsole.ColorAttributeToConsoleColor(
  c: TWinColor): TConsoleColor;
begin
  if ((byte (integer(c) and (Integer(TWinColor.colBackgroundYellow) or Integer(TWinColor.colBackgroundIntensity) or Integer(TWinColor.colBackgroundBlue)))) <> 0) then
    c :=TWinColor(Byte(Byte(c) Shr 4));

  Result := TConsoleColor(c);
end;

class function TConsole.ConsoleColorToColorAttribute(color: TConsoleColor;
  isBackground: boolean): TWinColor;
var
  color2: TWinColor;
begin
 if ((Integer(color) and not Integer(TConsoleColor.White)) <> Integer(TConsoleColor.Black)) then
      raise EArgumentException.Create('Arg_InvalidConsoleColor');
  color2 := TWinColor(color);
  if (isBackground) then
      color2 := TWinColor(SmallInt(Integer(color2) shl 4));
  begin
      Result := color2;
      exit
  end
end;

procedure TConsole.Init;
begin
  Console.ConsoleOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  Console.InitScreenMode;
  SaveColor;
end;

function TConsole.GetBackgroundColor: TConsoleColor;
var
  lbufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
  C: TWinColor;
begin
  lbufferInfo := GetBufferInfo;

  C := TWinColor(SmallInt(lbufferInfo.wAttributes and 240));
  Exit(ColorAttributeToConsoleColor(C));
end;

function TConsole.GetBufferWidth: Integer;
begin
  Result := GetBufferInfo.dwSize.X;
end;

function TConsole.GetBufferHeight: integer;
begin
  Result := GetBufferInfo.dwSize.Y;
end;

class function TConsole.GetBufferInfo: Windows._CONSOLE_SCREEN_BUFFER_INFO;
begin
  {$IFDEF CONSOLE}
  if not Windows.GetConsoleScreenBufferInfo(ConsoleOutputHandle {STD_OUTPUT_HANDLE},Result) then
    raise Exception.Create('Error while getting screenbuffer info');
  {$ELSE}

  {$ENDIF}

end;

class function TConsole.GetBufferSize: TCoord;
begin
  Result := GetBufferInfo.dwSize;
end;

function TConsole.getCapsLock: boolean;
begin
  Result := ((Windows.GetKeyState(20) and 1) = 1)
end;

function TConsole.GetColors(Index: TColorRange): TWinColor;
begin
  Result := FColors[Index] ;
end;

function TConsole.GetConsoleInputHandle: Integer;
begin
//  Result := TTextRec(Input).Handle;
  Result := GetStdHandle(STD_INPUT_HANDLE)
end;

class function TConsole.GetConsoleOutputHandle: THandle;
begin
//  Result := TTextRec(Output).Handle;
  Result := GetStdHandle(STD_OUTPUT_HANDLE)
end;

function TConsole.GetCursorLeft: Integer;
begin
  Result := self.GetBufferInfo.dwCursorPosition.X
end;

function TConsole.GetCursorSize: Integer;
var
    myconsole_cursor_info: CONSOLE_CURSOR_INFO;
begin
  Result := 0;
  if Win32Check(Windows.GetConsoleCursorInfo(Self.ConsoleOutputHandle, myconsole_cursor_info)) then
    Exit(myconsole_cursor_info.dwSize);
end;

function TConsole.GetCursorTop: Integer;
begin
  Result := GetBufferInfo.dwCursorPosition.Y
end;

function TConsole.GetCursorVisible: boolean;
var
  lconsole_cursor_info: CONSOLE_CURSOR_INFO;
begin
  Result := False;
  if Win32Check(Windows.GetConsoleCursorInfo(ConsoleOutputHandle, lconsole_cursor_info)) then
    Exit(lconsole_cursor_info.bVisible);
end;

function TConsole.GetError: TTextWriter;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetForegroundColor: TConsoleColor;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetInReader: TTextReader;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetKeyAvailable: Boolean;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetLargestWindowHeight: Integer;
begin
  Result := Windows.GetLargestConsoleWindowSize(ConsoleOutputHandle).Y
end;

function TConsole.GetLargestWindowWidth: Integer;
begin
  Result := Windows.GetLargestConsoleWindowSize(ConsoleOutputHandle).X
end;

function TConsole.getNumberLock: boolean;
begin
  Result := ((Windows.GetKeyState($90) and 1) = 1)
end;

function TConsole.GetOut: TTextWriter;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetTitle: String;
begin
{
  sb := StringBuilder.Create($5fb5);
  Win32Native.SetLastError(0);
  consoleTitle := Windows.GetConsoleTitle(sb, sb.Capacity);
  if (consoleTitle = 0) then
  begin
      errorCode := Marshal.GetLastWin32Error;
      if (errorCode = 0) then
          sb.Length := 0
      else
          __Error.WinIOError(errorCode, string.Empty)
      end
  else
      if (consoleTitle > $5fb4) then
          raise InvalidOperationException.Create(Environment.GetResourceString('ArgumentOutOfRange_ConsoleTitleTooLong'));
      begin
          Result := sb.ToString;
          exit
      end
}
end;

function TConsole.GetTreatControlCAsInput: Boolean;
var
  lConsoleInputHandle : THandle;
  Mode:Cardinal;
begin
  lConsoleInputHandle := ConsoleInputHandle;
  if (lConsoleInputHandle = Windows.INVALID_HANDLE_VALUE) then
      EInOutError.Create('IO.IO_NoConsole');

  mode := 0;
  if (not Windows.GetConsoleMode(lConsoleInputHandle, mode)) then
    RaiseLastOSError;
  begin
      Result := ((mode and 1) = 0);
      exit
  end
end;

function TConsole.GetWindowHeight: Integer;
var
  bufferInfo : _CONSOLE_SCREEN_BUFFER_INFO;
begin
  bufferInfo := GetBufferInfo;
  Result := ((bufferInfo.srWindow.Bottom - bufferInfo.srWindow.Top) + 1)
end;

function TConsole.GetWindowLeft: Integer;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetWindowTop: Integer;
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

function TConsole.GetWindowWidth: Integer;
var
  bufferInfo : _CONSOLE_SCREEN_BUFFER_INFO;
begin
  bufferInfo := GetBufferInfo;
  Result := ((bufferInfo.srWindow.Right - bufferInfo.srWindow.Left) + 1)
end;

procedure TConsole.MoveBufferArea(sourceLeft, sourceTop, sourceWidth,
  sourceHeight, targetLeft, targetTop: integer; sourceChar: char;
  sourceForeColor, sourceBackColor: TConsoleColor);
var
  i : Integer;
  char_infoArray: array of CHAR_INFO;
  char_infoRef,char_infoRef2: CHAR_INFO;
  num2: Cardinal;
  dwSize:TCoord;
  readRegion,writeRegion : SMALL_RECT;
  dwWriteCoord,bufferCoord : TCoord;
  wColorAttribute,Color:TWinColor;

begin
    if ((sourceForeColor < TConsoleColor.Black) or (sourceForeColor > TConsoleColor.White)) then
        raise EArgumentException.Create('Arg_InvalidConsoleColor ' + 'sourceForeColor');
    if ((sourceBackColor < TConsoleColor.Black) or (sourceBackColor > TConsoleColor.White)) then
        raise EArgumentException.Create('Arg_InvalidConsoleColor ' + 'sourceBackColor');
    dwSize := TConsole.GetBufferInfo.dwSize;
    if ((sourceLeft   < 0)  or (sourceLeft   > dwSize.X))                  then raise EArgumentOutOfRangeException.Create('sourceLeft ' + IntToStr(sourceLeft  )+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((sourceTop    < 0)  or (sourceTop    > dwSize.Y))                  then raise EArgumentOutOfRangeException.Create('sourceTop'   + IntToStr(sourceTop   )+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((sourceWidth  < 0)  or (sourceWidth  > (dwSize.X - sourceLeft)))   then raise EArgumentOutOfRangeException.Create('sourceWidth' + IntToStr(sourceWidth )+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((sourceHeight < 0)  or (sourceTop    > (dwSize.Y - sourceHeight))) then raise EArgumentOutOfRangeException.Create('sourceHeight'+ IntToStr(sourceHeight)+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((targetLeft   < 0)  or (targetLeft   > dwSize.X))                  then raise EArgumentOutOfRangeException.Create('targetLeft'  + IntToStr(targetLeft  )+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((targetTop    < 0)  or (targetTop    > dwSize.Y))                  then raise EArgumentOutOfRangeException.Create('targetTop'   + IntToStr(targetTop   )+ 'ArgumentOutOfRange_ConsoleBufferBoundaries');
    if ((sourceWidth <> 0) and (sourceHeight <> 0)) then
    begin
//        UIPermission.Create(UIPermissionWindow.SafeTopLevelWindows).Demand;
        SetLength(char_infoArray,(sourceWidth * sourceHeight));
        dwSize.X := sourceWidth ;
        dwSize.Y := sourceHeight;
        readRegion.Left := (sourceLeft);
        readRegion.Right := (((sourceLeft + sourceWidth) - 1));
        readRegion.Top := (sourceTop);
        readRegion.Bottom := (((sourceTop + sourceHeight) - 1));

        bufferCoord.X := 7;
        bufferCoord.Y := 7;

        if (not Windows.ReadConsoleOutput(TConsole.ConsoleOutputHandle, @char_infoRef, dwSize, bufferCoord, readRegion)) then
          RaiseLastOSError;

        dwWriteCoord.X := sourceLeft;
        Color :=
          TWinColor(Integer(ConsoleColorToColorAttribute(sourceBackColor, true)) or
          Integer(ConsoleColorToColorAttribute(sourceForeColor, false)));
        wColorAttribute := Color;
        i := sourceTop;

        while ((i < (sourceTop + sourceHeight))) do
        begin
          dwWriteCoord.Y := (i);
          if (not Windows.FillConsoleOutputCharacter(ConsoleOutputHandle, sourceChar, sourceWidth, dwWriteCoord, num2)) then
            RaiseLastOSError;
          if (not Windows.FillConsoleOutputAttribute(ConsoleOutputHandle, Word(wColorAttribute), sourceWidth, dwWriteCoord, num2)) then
            RaiseLastOSError;
          inc(i)
        end;

        writeRegion.Left := targetLeft;
        writeRegion.Right := targetLeft + sourceWidth;
        writeRegion.Top := targetTop;
        writeRegion.Bottom := targetTop + sourceHeight;

        Win32Check(Windows.WriteConsoleOutput(ConsoleOutputHandle, @char_infoRef2, dwSize, bufferCoord, writeRegion))

    end
end;

procedure TConsole.MoveBufferArea(sourceLeft, sourceTop, sourceWidth,
  sourceHeight, targetLeft, targetTop: integer);
begin
  MoveBufferArea(sourceLeft, sourceTop, sourceWidth, sourceHeight, targetLeft, targetTop, ' ', TConsoleColor.Black, BackgroundColor)
end;

class function TConsole.OpenStandardError: TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.OpenStandardError(bufferSize: Integer): TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.OpenStandardInput(bufferSize: Integer): TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.OpenStandardInput: TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.OpenStandardOutput: TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.OpenStandardOutput(bufferSize: Integer): TStream;
begin
  raise Exception.Create('Not yet implemented');
end;

class function TConsole.Read: integer;
begin
  System.Read(Result);
end;

class function TConsole.ReadKey(intercept: boolean): TConsoleKeyInfo;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
  StdIn:THandle;
begin
  begin
//    Result := #$FF;
    StdIn := TTextRec(Input).Handle;
    GetConsoleMode(StdIn, KeyMode);
    SetConsoleMode(StdIn, 0);
    repeat
      ReadConsoleInput(StdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and
         InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          Result.Key := TConsoleKey(InputRec.Event.KeyEvent.wVirtualKeyCode);
          Result.KeyChar := InputRec.Event.KeyEvent.UnicodeChar;
          Break;
        end;
      end;
    until False;
    SetConsoleMode(StdIn, KeyMode);
  end;
end;

class function TConsole.ReadKey: TConsoleKeyInfo;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
  StdIn:THandle;
begin
  begin
//    Result := #$FF;
    StdIn := TTextRec(Input).Handle;
    GetConsoleMode(StdIn, KeyMode);
    SetConsoleMode(StdIn, 0);
    repeat
      ReadConsoleInput(StdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and
         InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          Result.Key := TConsoleKey(InputRec.Event.KeyEvent.wVirtualKeyCode);
          Result.KeyChar := InputRec.Event.KeyEvent.UnicodeChar;
          Break;
        end;
      end;
    until False;
    SetConsoleMode(StdIn, KeyMode);
  end;
end;

class function TConsole.ReadLine: String;
begin
  ReadLn(Result);
end;

class procedure TConsole.ResetColor;
begin
  raise Exception.Create('Not yet implemented');
end;

procedure TConsole.SetBackgroundColor(value: TConsoleColor);
begin
  raise Exception.Create('Not yet implemented');
end;

procedure TConsole.SetBufferHeight(value: integer);
var
  Size:TCoord;
begin
  Size.X := GetBufferWidth;
  Size.Y := value;
  Windows.SetConsoleScreenBufferSize( ConsoleOutputHandle, Size );
end;

class procedure TConsole.SetBufferSize(Size: TCoord);
begin
  TConsole.SetBufferSize(Size.X,Size.Y);
end;

class procedure TConsole.SetBufferSize(width, height: integer);
var
  Size:  TCoord;
begin
  If not IsConsole then exit;

  Size.X := Width;
  Size.Y := Height;
  SetConsoleScreenBufferSize(ConsoleOutputHandle, Size)
end;

procedure TConsole.SetBufferWidth(AValue: Integer);
var
  Size:TCoord;
begin
  Size.X := AValue;
  Size.Y := GetBufferHeight;
  Windows.SetConsoleScreenBufferSize( ConsoleOutputHandle, Size );
end;

procedure TConsole.setCapsLock(aValue: Boolean);
begin
  if aValue <> odd(GetKeyState(VK_CAPITAL)) then
  begin
    keybd_event( VK_CAPITAL, 0 , 0, 0);
    keybd_event( VK_CAPITAL, 0 , KEYEVENTF_KEYUP, 0);
  end;
end;

procedure TConsole.SetColors(Index: TColorRange; const Value: TWinColor);
begin
  FColors[Index] := Value;
//  SetConsolePalette(FColors);
end;

procedure TConsole.SetConsoleInputHandle(AValue: Integer);
begin
  TTextRec(Input).Handle := AValue;
end;

class procedure TConsole.SetConsoleOutputHandle(AValue: THandle);
begin
  {R-}
  TTextRec(Output).Handle := AValue;
  {$R+}
end;

procedure TConsole.SetCursorLeft(AValue: Integer);
begin
  self.SetCursorPosition(AValue, self.CursorTop)
end;

class procedure TConsole.SetCursorPosition(left, top: integer);
var
  Coord:TCoord;
begin
  Coord.X := Left;
  Coord.Y := top;
  SetConsoleCursorPosition( ConsoleOutputHandle, Coord );
end;

procedure TConsole.SetCursorSize(AValue: Integer);
var
  myConsole_cursor_info: CONSOLE_CURSOR_INFO;
begin
   if ((AValue < 1) or (AValue > 100)) then
        raise ERangeError.Create('value' + IntToStr(AValue) + ' out of range for CursorSize');

    // UIPermission.Create(UIPermissionWindow.SafeTopLevelWindows).Demand;
    // if ((Avalue = 100) and ((Environment.OSInfo and OSName.Win9x) <> OSName.Invalid)) then
    //    Avalue := $63;
    // consoleOutputHandle := self.ConsoleOutputHandle;
    if (not Windows.GetConsoleCursorInfo(consoleOutputHandle, myConsole_cursor_info)) then
      RaiseLastOSError;
    myConsole_cursor_info.dwSize := Avalue;
    if (not Windows.SetConsoleCursorInfo(consoleOutputHandle, myConsole_cursor_info)) then
      RaiseLastOSError;
end;

procedure TConsole.SetCursorTop(AValue: Integer);
begin
  SetCursorPosition(CursorLeft, AValue)
end;

procedure TConsole.SetCursorVisible(AValue: Boolean);
var
  myconsole_cursor_info: TConsoleCursorInfo;
begin
  // UIPermission.Create(UIPermissionWindow.SafeTopLevelWindows).Demand;
  if (not Windows.GetConsoleCursorInfo( ConsoleOutputHandle, myconsole_cursor_info)) then
    RaiseLastOSError;
  myconsole_cursor_info.bVisible := aValue;
  if (not Windows.SetConsoleCursorInfo(consoleOutputHandle, myconsole_cursor_info)) then
    RaiseLastOSError
end;

class procedure TConsole.SetError(newError: TTextWriter);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

procedure TConsole.SetForegroundColor(AValue: TConsoleColor);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

class procedure TConsole.SetIn(newIn: TTextReader);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

class procedure TConsole.SetOut(newOut: TTextWriter);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

procedure TConsole.SetTitle(AValue: String);
begin
//    UIPermission.Create(UIPermissionWindow.SafeTopLevelWindows).Demand;
    if (Length(AValue) > $5fb4) then
        raise ERangeError.Create('value too long for console title');
    if (not Windows.SetConsoleTitle(PWideChar(AValue))) then
        RaiseLastOSError
end;

procedure TConsole.SetTreatControlCAsInput(AValue: Boolean);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

procedure TConsole.SetWindowHeight(AValue: Integer);
begin
  SetWindowSize(WindowWidth, AValue)
end;

procedure TConsole.SetWindowLeft(AValue: Integer);
begin
  SetWindowSize(aValue, WindowHeight)
end;

class procedure TConsole.SetWindowPosition(left, top: integer);
var
  wp:tagWindowPlacement;
begin
  wp.rcNormalPosition.Left := left;
  wp.rcNormalPosition.Top := top;
  SetWindowPlacement( ConsoleOutputHandle,wp);
end;

class procedure TConsole.SetWindowSize(width, height: integer);
var
  bufferInfo : _CONSOLE_SCREEN_BUFFER_INFO;
  srWindow : _SMALL_RECT;
  flag2 : Boolean;
  Size,largestConsoleWindowSize : TCoord;
//  errorCode : Integer;
begin
  Exit;
  bufferInfo := TConsole.GetBufferInfo;
  if (width <= 0) then
      raise ERangeError.Create('width ' + IntToStr(width) + ' ArgumentOutOfRange_NeedPosNum');
  if (height <= 0) then
      raise ERangeError.Create('height' + IntToStr(height) + 'ArgumentOutOfRange_NeedPosNum');

  flag2 := false;
  size.X := bufferInfo.dwSize.X;
  size.Y := bufferInfo.dwSize.Y;
  if (bufferInfo.dwSize.X < (bufferInfo.srWindow.Left + width)) then
  begin
      if (bufferInfo.srWindow.Left >= ($7fff - width)) then
          raise ERangeError.Create('width ' + 'ArgumentOutOfRange_ConsoleWindowBufferSize');
      size.X := bufferInfo.srWindow.Left + width;
      flag2 := true
  end;
  if (bufferInfo.dwSize.Y < (bufferInfo.srWindow.Top + height)) then
  begin
      if (bufferInfo.srWindow.Top >= ($7fff - height)) then
          raise ERangeError.Create('height ' + IntToStr(Height) + ' ArgumentOutOfRange_ConsoleWindowBufferSize');
      size.Y := bufferInfo.srWindow.Top + height;
      flag2 := true
  end;
  if (flag2 and not Windows.SetConsoleScreenBufferSize(TConsole.ConsoleOutputHandle, size)) then
    RaiseLastOsError;
  srWindow := bufferInfo.srWindow;
  srWindow.Bottom := (srWindow.Top + height) - 1;
  srWindow.Right := (srWindow.Left + width) - 1;
  if (not Windows.SetConsoleWindowInfo(TConsole.ConsoleOutputHandle, true, srWindow)) then
  begin
      // errorCode := GetLastError;
      if (flag2) then
          Windows.SetConsoleScreenBufferSize(TConsole.ConsoleOutputHandle, bufferInfo.dwSize);
      largestConsoleWindowSize := Windows.GetLargestConsoleWindowSize(TConsole.ConsoleOutputHandle);
      if (width > largestConsoleWindowSize.X) then
          raise ERangeError.Create('width' + IntToStr(width) + ' ArgumentOutOfRange_ConsoleWindowSize_Size');
      if (height > largestConsoleWindowSize.Y) then
          raise ERangeError.Create('height' + IntTostr(height) + ' ArgumentOutOfRange_ConsoleWindowSize_Size');

      // __Error.WinIOError(errorCode, string.Empty)
      RaiseLastOSError;
  end
end;

procedure TConsole.SetWindowTop(AValue: Integer);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

procedure TConsole.SetWindowWidth(AValue: Integer);
begin
  raise ENotImplemented.Create('Not yet implemented.');
end;

class procedure TConsole.Write(value: Variant);
begin
  // Todo: check if this is the right way
  System.Write(Value);
end;

class procedure TConsole.WriteLine(value: Variant; Args: array of const);
var
  S:String;
  i:Integer;
begin
  S := Value;
  for i := 0 to High(args) do
  begin
    S := ReplaceStr(S,'{'+IntToStr(i)+'}','%'+IntToStr(i)+':s');
  end;
  System.WriteLn(Format(S,Args));
end;

class procedure TConsole.WriteLine(value: Variant);
begin
  // Todo: check if this is the right way
  System.WriteLn(Value);
end;


function GetPassword(const InputMask: Char = '*'): string;
var
  OldMode: Cardinal;
  c: char;
begin
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode and not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  try
    while not Eof do
    begin
      Read(c);
      if c = #13 then // Carriage Return
        Break;
      if c = #8 then // Back Space
        Write(#8)
      else
      begin
        Result := Result + c;
        Write(InputMask);
      end;
    end;
  finally
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  end;
end;

function GetNumber: Int64;
var
  OldMode: Cardinal;
  c: char;
  s:String;
begin
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode and not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  S := '';
  try
    while not Eof do
    begin
      Read(c);
      if c = #13 then // Carriage Return
        Break;
      if c = #8 then // Back Space
        Write(#8)
      else
      begin
        if c in ['0'..'9'] then
        begin
          S := S + c;
          Write(C);
        end;
      end;
    end;
  finally
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  end;
  Result := StrToInt64(S)
end;


procedure AddBox(S:String;col:String);
const
  UpLeftChar	=	'Ú';
  UpRightChar	=	'¿';
  LowLeftChar	=	'À';
  LowRightChar =	'Ù';
  HorChar 	=	'Ä';
  VertChar	=	'³';
  SpaceChar	  =	' ';
var
  len,
  i:Integer;
begin
  len := Length(StripC(S));
  WriteC(col);

  Write(UpLeftChar);
  for i := 1 to Len+2 do
    Write(HorChar);
  Write(UpRightChar);
  Write(#13#10);


  Write(VertChar);
  Write(SpaceChar);
  WriteC(S+Col);
  Write(SpaceChar);
  Write(VertChar);
  Write(#13#10);

  Write(LowLeftChar);
  for i := 1 to Len+2 do
    Write(HorChar);
  Write(LowRightChar);
  Write(#13#10);
  WriteC('~z^w');
end;



initialization

{$IFDEF CONSOLE}
  Console.Init;
{$ENDIF}

finalization

end.




COLORREF DefaultColors[16] =
{
0x00000000, 0x00800000, 0x00008000, 0x00808000,
0x00000080, 0x00800080, 0x00008080, 0x00c0c0c0,
0x00808080, 0x00ff0000, 0x0000ff00, 0x00ffff00,
0x000000ff, 0x00ff00ff, 0x0000ffff, 0x00ffffff
};

SetConsolePalette(DefaultColors);
