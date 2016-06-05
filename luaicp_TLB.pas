unit luaicp_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 25.12.2015 20:44:53 from Type Library described below.

// ************************************************************************  //
// Type Lib: P:\Projects\LuaInterceptor\luaicp (1)
// LIBID: {0E662B3F-193C-4636-95A5-3EDBA011A77F}
// LCID: 0
// Helpfile:
// HelpString:
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  luaicpMajorVersion = 1;
  luaicpMinorVersion = 0;

  LIBID_luaicp: TGUID = '{0E662B3F-193C-4636-95A5-3EDBA011A77F}';

  IID_ILuaScript: TGUID = '{51F8A99C-20A0-480B-A2D2-542263603029}';
  CLASS_LuaScript: TGUID = '{5D21D4C8-8F28-4DA2-8E29-02B051F3937F}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  ILuaScript = interface;
  ILuaScriptDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  LuaScript = ILuaScript;


// *********************************************************************//
// Interface: ILuaScript
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51F8A99C-20A0-480B-A2D2-542263603029}
// *********************************************************************//
  ILuaScript = interface(IDispatch)
    ['{51F8A99C-20A0-480B-A2D2-542263603029}']
    function LoadScript(const bstrFileName: WideString): WideString; safecall;
    function Get_State: Integer; safecall;
    function CallFunc(const bstrFuncName: WideString; const bstrArgument: WideString): WideString; safecall;
    property State: Integer read Get_State;
  end;

// *********************************************************************//
// DispIntf:  ILuaScriptDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51F8A99C-20A0-480B-A2D2-542263603029}
// *********************************************************************//
  ILuaScriptDisp = dispinterface
    ['{51F8A99C-20A0-480B-A2D2-542263603029}']
    function LoadScript(const bstrFileName: WideString): WideString; dispid 201;
    property State: Integer readonly dispid 203;
    function CallFunc(const bstrFuncName: WideString; const bstrArgument: WideString): WideString; dispid 204;
  end;

// *********************************************************************//
// The Class CoLuaScript provides a Create and CreateRemote method to
// create instances of the default interface ILuaScript exposed by
// the CoClass LuaScript. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoLuaScript = class
    class function Create: ILuaScript;
    class function CreateRemote(const MachineName: string): ILuaScript;
  end;

implementation

uses System.Win.ComObj;

class function CoLuaScript.Create: ILuaScript;
begin
  Result := CreateComObject(CLASS_LuaScript) as ILuaScript;
end;

class function CoLuaScript.CreateRemote(const MachineName: string): ILuaScript;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LuaScript) as ILuaScript;
end;

end.

