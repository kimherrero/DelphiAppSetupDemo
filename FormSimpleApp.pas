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
unit FormSimpleApp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, AppConsts, ExtCtrls;

type
  TFrmSimpleApp = class(TForm)
    lbTitle: TLabel;
    lbIns: TLabel;
    lbDat: TLabel;
    InsFolder: TEdit;
    DatFolder: TEdit;
    lbUpdate: TLabel;
    EZipFile: TEdit;
    btSelZip: TButton;
    lbText: TLabel;
    btUpdate: TBitBtn;
    BtClose: TButton;
    SelZip: TOpenDialog;
    pnImg: TPanel;
    Img: TImage;
    cbInsFiles: TComboBox;
    cbDatFiles: TComboBox;
    pnCompile: TPanel;
    lbBuilt: TLabel;
    btTest: TButton;
    lbAdmin: TLabel;
    procedure BtCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSelZipClick(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure cbDatFilesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    { Private declarations }
    procedure CustomPerform( Var Msg: TMessage ); message WM_CUSTOM_PERFORM;
    procedure LoadPicture;
  public
    { Public declarations }
  end;

var
  FrmSimpleApp: TFrmSimpleApp;

implementation

uses WinAppCore, SysCaller, jpeg, FormMsg;

{$R *.dfm}

  { Basic Conditional defines management }

  {$IFDEF PRODUCTION}
     const COMPILATION = 'PRODUCTION';
     {$UNDEF DEVELOP}
  {$ENDIF}

  {$IFDEF DEVELOP}
     const COMPILATION = 'DEVELOP';
  {$ENDIF}

  // Set to true to allow multiple instances of the application
  APP_IS_MULTIINSTANCE = False;


procedure TFrmSimpleApp.FormCreate(Sender: TObject);
const     MSG_INMEM = 'Application seems to be already in execution, proceed?';
var       SL : TStringList; Ok: Boolean;
begin
  // Conditional defines. Try running from IDE
  pnCompile.Caption := COMPILATION;
  Ok := True;

  // Multiple instance check
  If Not APP_IS_MULTIINSTANCE then begin

     // if Semaphore found
     If AtomFindDelete( SC_APP_EXE, False ) then Ok := ShowMsg( MSG_INMEM, True )
     else AtomFindCreate( SC_APP_EXE );  // Set Semaphore app is running

  end;

  If Not Ok then Application.Terminate;

  // custom color for updated app ( see AppBuildAndZip.bat )
  {$IFDEF CUSTOM}
     Self.Color := clGray;
     lbText.Color := clGray;
  {$ENDIF}

  If IsAdministrator then begin
     lbAdmin.Caption    := lbAdmin.Caption + 'ADMIN';
     lbAdmin.Font.Color := clMaroon;
  end else lbAdmin.Caption  := lbAdmin.Caption + 'standard user';

  If LoadPaths then begin

     InsFolder.Text := InstallDir;
     DatFolder.Text := UsrDataDir;

     SL := GetFileStringList( InstallDir + '*.*' );
     cbInsFiles.Items.Assign( SL );
     If SL.Count = 0 then begin
        cbInsFiles.Text := '< no files >';
        cbInsFiles.ItemIndex := -1;
     end else cbInsFiles.ItemIndex := 0;
     SL.Free;

     SL := GetFileStringList( UsrDataDir + '*.*' );
     cbDatFiles.Items.Assign( SL );
     If SL.Count = 0 then begin
        cbDatFiles.Text := '< no files >';
        cbDatFiles.ItemIndex := -1;
     end else cbDatFiles.ItemIndex := SL.Count-1;
     SL.Free;

     LoadPicture;

  end else begin

      InsFolder.Text := '< Not installed>';
      DatFolder.Text := InsFolder.Text;
      btSelZip.Enabled := False;

  end;
end;

procedure TFrmSimpleApp.FormDestroy(Sender: TObject);
begin
  // remove Semaphore
  AtomFindDelete( SC_APP_EXE, True );
end;

procedure TFrmSimpleApp.CustomPerform(var Msg: TMessage);
begin
  // let installer close app under demand
  Case Msg.WParam Of
     PFM_APP_QUIT : Close;
  end;
end;

procedure TFrmSimpleApp.cbDatFilesChange(Sender: TObject);
begin
  LoadPicture;
end;

procedure TFrmSimpleApp.LoadPicture;
var       AFile : String;
begin
  If cbDatFiles.itemindex >= 0 then begin
     AFile := UsrDataDir + cbDatFiles.Items[ cbDatFiles.itemindex ];
     If UpperCase( ExtractFileExt( AFile ) ) = '.JPG' then Img.Picture.LoadFromFile( AFile )
     else Img.Picture.Graphic.Free;
  end;
end;

procedure TFrmSimpleApp.btSelZipClick(Sender: TObject);
{  Zip file is supposed to contain bin.zip & data.zip wich will be installed by appupdater
   On real conditions this zip file could be an update downloaded by the app itself.       }
begin
  If SelZip.Execute then
     EZipFile.Text := SelZip.FileName;

  btUpdate.Enabled := ( EZipFile.Text <> '');
end;

procedure TFrmSimpleApp.btUpdateClick(Sender: TObject);
const     MSG_ERR = 'Error: %s not found.';
var       Setup: String;
begin
  Setup := InstallDir + SC_STP_EXE;
  If FileExists( Setup ) then begin

     // AppSetup with non elevated privileges takes care of calling the updater process ( elevated ) and recall SimpleApp
     ShellNewProcess( Setup, '/UPDATE:' + EZipFile.Text, True, False );  
     Close;

  end else ShowMsg( Format( MSG_ERR, [ Setup ] ) );
end;

procedure TFrmSimpleApp.BtCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TFrmSimpleApp.btTestClick(Sender: TObject);
begin
  //
end;

end.
