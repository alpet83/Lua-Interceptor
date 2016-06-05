{$IFDEF FAST_MM}
  FastMM4 in '..\lib\FastMM4.pas',
  FastMM4Messages in '..\lib\FastMM4Messages.pas',
  BlockMM in '..\lib\BlockMM.pas',
{$ELSE}
  XrayMM in 'XrayMM.pas',
//  msvcrt80 in '..\lib\msvcrt80.pas',
{$ENDIF}