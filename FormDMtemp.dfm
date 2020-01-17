inherited DMUpdater: TDMUpdater
  OldCreateOrder = True
  Height = 216
  Width = 242
  inherited UnZipper: TAbUnZipper
    OnArchiveItemProgress = UnZipperArchiveItemProgress
  end
end
