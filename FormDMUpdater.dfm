inherited DMUpdater: TDMUpdater
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  Height = 196
  Width = 250
  inherited UnZipper: TAbUnZipper
    OnArchiveItemProgress = UnZipperArchiveItemProgress
  end
end
