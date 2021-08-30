unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  stax, stax.tasks.functional, stax.tasks.io.tcp;

type
  TPlayerMovement = (pmUp = -1, pmNone = 0, pmDown = 1);
  TVector2D = record
    X: Double;
    Y: Double;
  end;

  TGameState = record
    Width: Integer;
    Height: Integer;
    BallPosition: TPoint;
    BallVelocity: TVector2D;
    Player1Pos: Integer;
    Player1Movement: TPlayerMovement;
    Player2Pos: Integer;
    Player2Movement: TPlayerMovement;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    HostEdit: TEdit;
    StartServerButton: TButton;
    PlayerPanel: TShape;
    OpponentPanel: TShape;
    Ball: TShape;
    StartClientButton: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartClientButtonClick(Sender: TObject);
    procedure StartServerButtonClick(Sender: TObject);
  private
    FExecutor: TExecutor;
    FGameState: TGameState;
    FIsServer: Boolean;
    FSocket: TSocket;

    procedure HandleTaskError(ATask: TTask; AException: Exception);
    function NewGameState: TGameState;
    procedure GameLoop(AExecutor: TExecutor);
    procedure HandleConnection(AExecutor: TExecutor);
    procedure StartServer(AExecutor: TExecutor);
    procedure StartClient(AExecutor: TExecutor);
    procedure StartGame;
    procedure RenderGame(AExecutor: TExecutor);
  public

  end;

var
  Form1: TForm1;

const
  PlayerVelocity =  100;

implementation

function Vec2D(X, Y: Double): TVector2D;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.StartServerButtonClick(Sender: TObject);
begin
  FIsServer := True;
  StartGame;
end;

procedure TForm1.StartClientButtonClick(Sender: TObject);
begin
  FIsServer := False;
  StartGame;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  NewMovement: TPlayerMovement;
begin
  if Key = 38 then
    NewMovement := pmUp
  else if Key = 40 then
    NewMovement := pmDown
  else
    Exit;
  if FIsServer then
    FGameState.Player1Movement := NewMovement
  else
    Await(specialize AsyncSend<TPlayerMovement>(FSocket, NewMovement));
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key <> 38) and (Key <> 40) then
    Exit;

  if FIsServer then
    FGameState.Player1Movement := pmNone
  else
    Await(specialize AsyncSend<TPlayerMovement>(FSocket, pmNone));
end;

procedure TForm1.HandleTaskError(ATask: TTask; AException: Exception);
begin
  ShowMessage(AException.Message);
  FExecutor.Terminate;
end;

function TForm1.NewGameState: TGameState;
begin
  Result.Width := Self.ClientWidth;
  Result.Height := Self.ClientHeight;
  Result.BallPosition := Point(Result.Width div 2 - 12, Result.Height div 2 - 12);
  Result.BallVelocity := Vec2D(50, 50);
  Result.Player1Movement := pmNone;
  Result.Player1Pos := Result.Height div 2 - PlayerPanel.Height div 2;
  Result.Player2Movement := pmNone;
  Result.Player2Pos := Result.Height div 2 - OpponentPanel.Height div 2;
end;

procedure TForm1.GameLoop(AExecutor: TExecutor);
var
  delta: Double;
  start: QWord;
begin
  start := GetTickCount64;
  while not AExecutor.CurrentTask.Terminated do
  begin
    delta := (GetTickCount64 - start) / 1000;
    start :=  GetTickCount64;
    FGameState.BallPosition.X := Round(FGameState.BallPosition.X + FGameState.BallVelocity.X * delta);
    FGameState.BallPosition.Y := Round(FGameState.BallPosition.Y + FGameState.BallVelocity.Y * delta);
    FGameState.Player1Pos := Round(FGameState.Player1Pos + ord(FGameState.Player1Movement) * PlayerVelocity * delta);
    FGameState.Player2Pos := Round(FGameState.Player2Pos + ord(FGameState.Player2Movement) * PlayerVelocity * delta);
    if (FGameState.BallPosition.Y + Ball.Height > FGameState.Height) or (FGameState.BallPosition.Y < 0) then
      FGameState.BallVelocity.Y *= -1;
    if (FGameState.BallPosition.X + Ball.Width > FGameState.Width) or (FGameState.BallPosition.X < 0) then
      FGameState := NewGameState;
    if ((FGameState.BallPosition.X - Ball.Width < OpponentPanel.Width) and
        (FGameState.BallPosition.Y + Ball.Height > FGameState.Player2Pos) and
        (FGameState.BallPosition.Y < FGameState.Player2Pos + OpponentPanel.Height)) or
       ((FGameState.BallPosition.X + Ball.Width > FGameState.Width - PlayerPanel.Width) and
        (FGameState.BallPosition.Y + Ball.Height > FGameState.Player1Pos) and
        (FGameState.BallPosition.Y < FGameState.Player1Pos + PlayerPanel.Height)) then
      FGameState.BallVelocity.X *= -1;
    Await(specialize AsyncSend<TGameState>(FSocket, FGameState));
    AsyncSleep(10);
  end;
end;

procedure TForm1.HandleConnection(AExecutor: TExecutor);
begin
  while not AExecutor.CurrentTask.Terminated do
    if FIsServer then // server only receives which way the player panel should move
      FGameState.Player2Movement := specialize Await<TPlayerMovement>(specialize AsyncReceive<TPlayerMovement>(FSocket))
    else // client receives the current game state
      FGameState := specialize Await<TGameState>(specialize AsyncReceive<TGameState>(FSocket));
end;

procedure TForm1.StartServer(AExecutor: TExecutor);
var
  ServerSocket: TSocket;
begin
  ServerSocket := TCPServerSocket('0.0.0.0', 1337);
  try
    TCPServerListen(ServerSocket, 1);
    FSocket := specialize Await<TSocket>(AsyncAccept(ServerSocket));
    try
      RunAsync(AsyncProcedure(@Self.GameLoop));
      Await(AsyncProcedure(@Self.HandleConnection));
    finally
      TCPSocketClose(FSocket);
    end;
  finally
    TCPSocketClose(ServerSocket);
  end;
end;

procedure TForm1.StartClient(AExecutor: TExecutor);
begin
  FSocket := TCPSocket;
  try
    Await(AsyncConnect(FSocket, HostEdit.Text, 1337));
    Await(AsyncProcedure(@Self.HandleConnection));
  finally
    TCPSocketClose(FSocket);
  end;
end;

procedure TForm1.StartGame;
begin
  StartServerButton.Hide;
  StartClientButton.Hide;
  HostEdit.Hide;
  FGameState := NewGameState;
  FExecutor := TExecutor.Create;
  try
    FExecutor.OnError := @Self.HandleTaskError;
    FExecutor.RunAsync(AsyncProcedure(@RenderGame));
    if FIsServer then
      FExecutor.RunAsync(AsyncProcedure(@Self.StartServer))
    else
      FExecutor.RunAsync(AsyncProcedure(@Self.StartClient));
    FExecutor.Run;
  finally
    FExecutor.Free;
  end;
end;

procedure TForm1.RenderGame(AExecutor: TExecutor);
begin
  if AExecutor.CurrentTask.Terminated then
    Exit;
  Self.ClientWidth := FGameState.Width;
  Self.ClientHeight := FGameState.Height;
  if FIsServer then
    Ball.Left := FGameState.BallPosition.X
  else
    Ball.Left := -(FGameState.BallPosition.X - FGameState.Width div 2) + FGameState.Width div 2;
  Ball.Top := FGameState.BallPosition.Y;
  if FIsServer then
  begin
    PlayerPanel.Top := FGameState.Player1Pos;
    OpponentPanel.Top := FGameState.Player2Pos;
  end
  else
  begin
    PlayerPanel.Top := FGameState.Player2Pos;
    OpponentPanel.Top := FGameState.Player1Pos;
  end;
  AsyncSleep(10);
  // reschedule before calling process message
  RunAsync(AsyncProcedure(@RenderGame));
  Application.ProcessMessages;
end;

end.

