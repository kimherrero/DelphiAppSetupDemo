{**************************************************************************************************
 COMPLETE DELPHI APP INSTALLER DEMO PROJECT: ( 32 Bits ). V1.0. January 2020.
***************************************************************************************************

 This project covers most of the requisites a modern setup application needs to achieve on Windows,
 without the help of any scripting system. It is entirely made from the scratch with Delphi 7, thus
 providing you all the freedom you may need.

 IMPORTANT: This project contains compiled exe files as Zipped resources. Please read "readme.txt"
 file for a better understanding about the compiling mechanism.

***************************************************************************************************
 DISCLAIMER: All projects and units are provided "AS IS" with no warranty of any kind.
***************************************************************************************************
* Thanks and enjoy,
* Kim Herrero / kim@kimherrero.com
***************************************************************************************************}
unit FormMsg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmMsg = class(TForm)
    lbMsg: TLabel;
    btOk: TButton;
    btKo: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
  public
  end;

  function  ShowMsg( S: String; const IsQuestion: Boolean = False ): Boolean;


implementation

{$R *.dfm}

function ShowMsg(S: String; const IsQuestion: Boolean = False ): Boolean;
var      FrmMsg: TFrmMsg;
begin
  FrmMsg := TFrmMsg.Create( Application.MainForm );
  FrmMsg.lbMsg.Caption := S;

  If Not IsQuestion then begin

     FrmMsg.btKo.Caption := 'Ok';
     FrmMsg.btKo.ModalResult := mrOk;
     FrmMsg.btOk.Visible := False;

  end;

  If FrmMsg.ShowModal = mrOk then Result := True
  else Result := False;
end;

procedure TFrmMsg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


end.
