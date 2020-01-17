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
unit FormDMUnzip;

interface

uses
  SysUtils, AbUnzper, Classes, AbBase, AbBrowse, AbZBrows;

type
  TDMUnzip = class(TDataModule)
    UnZipper: TAbUnZipper;
    procedure UnZipperConfirmOverwrite(var Name: String; var Confirm: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    function  AppSendClose: Boolean;
    procedure UnzipFile( const ZipFile, Destination, FileNames: String);  
  end;

var
  Param, ParamInfo: String;
  DMUnzip: TDMUnzip;


implementation

uses FormMsg, AppConsts, Windows, SysCaller;

{$R *.dfm}

function TDMUnzip.AppSendClose: Boolean;
var       H: THandle; n: Integer;
begin
  H := 0;
  n := 0;

  // Detect app in execution
  If AtomFindDelete( SC_APP_EXE, False ) then
  Repeat

    // send close message to all windows of this class
    // IMPORTANT: Close design window in delphi too!
    H := FindWIndow( PChar( SC_APP_CLASS ), nil );
    If (H <> 0) then
       SendMessage( H, WM_CUSTOM_PERFORM, PFM_APP_QUIT, 0 );
    Sleep( 100 );
    Inc( n );

  Until ( H = 0 ) or (n > 50);

  Result := ( H = 0 );

end;

procedure TDMUnzip.UnzipFile(const ZipFile, Destination, FileNames: String);
begin
  UnZipper.FileName := ZipFile;
  UnZipper.BaseDirectory := Destination;
  UnZipper.ExtractFiles( FileNames );
  UnZipper.CloseArchive;
end;

procedure TDMUnzip.UnZipperConfirmOverwrite(var Name: String; var Confirm: Boolean);
begin
  Confirm := True;
end;

end.
