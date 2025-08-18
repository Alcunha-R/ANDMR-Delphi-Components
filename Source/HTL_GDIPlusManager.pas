unit HTL_GDIPlusManager;

interface

uses
  Winapi.Windows, Winapi.GDIPOBJ, Winapi.GDIPAPI;

var
  gdiplusToken: ULONG_PTR;
  gdiplusStartupInput: TGPStartupInput;

implementation

initialization
  gdiplusStartupInput.GdiplusVersion := 1;
  gdiplusStartupInput.DebugEventCallback := nil;
  gdiplusStartupInput.SuppressBackgroundThread := False;
  gdiplusStartupInput.SuppressExternalCodecs := False;
  GdiplusStartup(gdiplusToken, @gdiplusStartupInput, nil);

finalization
  GdiplusShutdown(gdiplusToken);

end.
