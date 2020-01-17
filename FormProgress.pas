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
unit FormProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AbMeter, StdCtrls;

type
  TFrmProgress = class(TForm)
    Meter: TAbMeter;
    lbText: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   FrmProgress: TFrmProgress;


implementation

{$R *.dfm}

end.
