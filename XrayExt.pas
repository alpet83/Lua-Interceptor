unit XrayExt;

interface
{ some nice Xray Extensions with RTM injection }
uses Windows, Classes, SysUtils, Misc, PsUtils, LuaTypes, LuaTools, XrayLua;

{$I stkdef.inc}

type
   TPatchFunc = function: Boolean;

var
   patch_list: TList;


procedure    AddGlobals (L: lua_State; disp: Boolean);
procedure    InitModule;
procedure    TryPatch;


implementation
uses LCGlobals;

{
  b_addr   = $10000000
  sec1_rva = $00001000
  shift    = $10001000
}


{$I x86.inc}

var
        RImplementation: DWORD = 0; // need add xrRenderR2
   detail_blink_fix_ptr: DWORD = 0; // need add xrRenderR2
   detail_blink_fix_ret: DWORD = 0;
     new_smap_sizes_ptr: DWORD = 0;
     new_smap_sizes_ret: DWORD = 0;
      new_smap_core_ptr: DWORD = 0;
            render_base: DWORD = 0;

        CKinematicsAnimated__ID_Cycle: Pointer = nil;
       CKinematicsAnimated__PlayCycle: Pointer = nil;

function  FindRender2: Boolean;
begin
 if render_base = 0 then
   begin
    render_base := GetModuleHandle ('xrRender_R2.dll');
    RImplementation      := render_base + $007A988;
    detail_blink_fix_ptr := render_base + $00229DC;
    detail_blink_fix_ret := render_base + $00229F6;
    new_smap_sizes_ptr   := render_base + $0003F43;
    new_smap_sizes_ret   := render_base + $0003F48;
    new_smap_core_ptr        := render_base + $006964C;
   end;
 result := ( render_base > 0 );
end;

procedure detail_blink_fix;
asm
  // restore cut useful code
  mov     [esp+148h-100h], ecx
  mov     [esp+148h-104h], edx
  // get viewbase fructum
  mov		esi, RImplementation
  add		esi, 8
  // copy viewbase into current frustum
  mov		ecx, 3Dh
  lea           edi, [esp+148h-0F8h]
  rep           movsd
  jmp           [detail_blink_fix_ret]
end;


function detail_blink_fix_patch: Boolean;
var
   pv: PByteArray;
   dw: DWORD;
begin
 result := FALSE;
 if not FindRender2 then exit;
 result := TRUE;

 dw := DWORD ( @detail_blink_fix );

 wprintf('[~T]. #DBG: detail_blink_fix_path, injecting jumps to~C0D $%x~C07, RImplementation at~C0D $%x~C07, stub at~C0D $%x~C07',
                         [detail_blink_fix_ptr, RImplementation, dw]);

 pv := Ptr (detail_blink_fix_ptr);
 UnlockRegion ( pv );
 if ( pv[0] = $6A ) and ( pv [1] = $2F ) then else
  begin
   ODS ('[~T].~C0C #WARN:~C07 detail_blink_fix_patch not applied. Probably already patched or incompatible xrRender_R2.dll');
   exit;
  end;
 pv [0] := op_push_dw;
 Move ( dw, pv [1], 4 );   // push stub_addr
 pv [5] := op_ret;
end;

procedure upd_smap_size(p: DWORD); stdcall;
var
   flag: DWORD;
   pval: PDWORD;
      s: PAnsiChar;
      v: DWORD;


begin
 if new_smap_core_ptr = 0 then exit;
 s := Ptr ( PDWORD(new_smap_core_ptr)^ + $4D0 );
 if not Assigned (s) then exit;

 if s = '-smap8192' then
    v := 8192 else
 if s = '-smap16384' then
    v := 16384 else
 exit;

 flag := $0FFFE0001 or v;

 pval := Ptr (p + $2D4);

 Assert ( not IsBadWritePtr(pval, 4), 'upd_smap_size: Bad write addr = ' + FormatPtr(pval) );

 pval^ := ( pval^ and flag ) or v;
end;


procedure new_smap_sizes_fix;
asm
 pushad
 push edi
 call    upd_smap_size
 popad
 mov     eax, new_smap_core_ptr
 mov     eax, [eax]
 add     eax, 4D0h
 jmp     [new_smap_sizes_ret]
end;

function  CScriptGameObject__CHudItem: Pointer; stdcall;
// MAKE_CASTING4
asm
  mov     ecx, [ecx+4]                   // get CGameObject instance
	// PRINT_UINT "obj=%x", ecx
	test    ecx, ecx
	mov     eax, ecx
	jz      @exit

	mov     eax, [ecx]                     // get vftable
  test    eax, eax
  jz      @exit

	mov     eax, [eax + $74]               // cast_inventory_item
  test    eax, eax
  jz      @exit
	call    eax
	// RRINT_UINT "1st=%x", eax
	;
	test    eax, eax
	jz      @exit


	mov     edx, [eax]                     // get vftable
	mov     ecx, eax
	mov     eax, [edx + $124]
	call    eax                            // cast_hud_item
  // PRINT_UINT "2nd=%x", eax
@exit:
	retn

end;


procedure CScriptGameObject__GetHudVisual; stdcall;
asm
	push    ebp
	mov     ebp, esp
	and     esp, 0FFFFFFF8h
	push    ecx

	call    CScriptGameObject__CHudItem
  test    eax, eax
  jz      @exit_fail

	mov     eax, [eax+16] // eax == m_pHUD
	movzx   ecx, byte ptr [eax+4]
	test    ecx, ecx
	jnz     @exit_fail

	mov     eax, [eax+48h]
	mov     ecx, [eax+8]    // ecx == visual
	test    ecx, ecx
	jz      @exit_fail

	mov     eax, [ecx]
	mov     eax, [eax+18h]
	call    eax             // pHudVisual = smart_cast<CKinematics*>(m_pHUD->Visual());
	test    eax, eax
	jnz     @exit
@exit_fail:
	xor     eax, eax
@exit:
	pop     ecx
	mov     esp, ebp
	pop     ebp
	retn
end;


procedure  CScriptGameObject__PlayHudAnimation;
const
     motion_ID =   -4;           // локальная переменная
     anim      =   8;            // 2 аргумент
     mix_in    =   anim + 4;     // 1 аргумент




asm
  // [ebp] = EIP?
	push    ebp
	mov     ebp, esp
	and     esp, 0FFFFFFF8h
	sub     esp, 8

  xor     ebx, ebx
  mov     [ebp + motion_ID], ebx // debug

	push    esi
	call    CScriptGameObject__GetHudVisual
  test    eax, eax
  jz      @exit_fail

	mov     esi, eax
	mov     ecx, esi // this == ecx
	mov     eax, [ebp+anim]
	push    eax
	lea     eax, [ebp+motion_ID]
	push    eax
	call    [CKinematicsAnimated__ID_Cycle]
	cmp     word ptr [ebp+motion_ID], 0FFFFh             // silent ignore
	jz      @exit_fail
	// ;mov     eax, [ebp+anim]
  // ;PRINT_UINT "ready: %s", eax
	push    0
	push    0
	push    0

	movzx   eax, BYTE PTR [ebp+mix_in]
	push    eax

	mov     eax, DWORD PTR [ebp+motion_ID]
	push    eax

	mov     ecx, esi // this == ecx
	call    [CKinematicsAnimated__PlayCycle]
@exit_fail:
	pop     esi
	mov     esp, ebp
	pop     ebp
	ret     8
end;

function        play_hud_anim (L: lua_State): Integer; cdecl;
var
   anim: AnsiString;
    anm: PAnsiChar;
    obj: PPointer;
    mix: Boolean;
      t: Integer;
begin
 result := 0;
 t := lua_gettop(L);
 if t < 3 then
   begin
    PrintError('play_hud_anim arguments: (game_object, string anim, bool mix), but arg_count = ' + IntToStr(t));
    exit;
   end;

 t := lua_type(L, 1);
 obj := nil;

 case t of
  LUA_TUSERDATA:
      obj := lua_touserdata (L, 1);
  LUA_TLUDATA:
      obj := lua_topointer (L, 1);
  else
     PrintError('bad object type ' + lua_typeof(L, 1));
 end;


 anim := AnsiString ( LuaStrArg(L, 2) );
 mix  := lua_toboolean (L, 3);

 if obj <> nil then
 try
  obj := obj^;              // userdata wrapper to CScriptGameObject
  anm := PAnsiChar (anim);
  asm
   push   ebx
   push   ecx

   mov    ecx,  [obj]
   movzx  eax,  mix
   push   eax
   mov    ebx,  anm
   push   ebx

   call   CScriptGameObject__PlayHudAnimation // push eip to [esp - 12]

   pop    ecx
   pop    ebx
  end;
 except
  on E: Exception do
     OnExceptLog('play_hud_anim', E);
 end;


end;


procedure    TryPatch;
var
   n: Integer;
   f: TPatchFunc;
begin
 for n := patch_list.Count - 1 downto 0 do
  begin
   f := patch_list [n];
   if f () then
      patch_list.Delete (n);
  end;
end;


procedure    AddGlobals (L: lua_State; disp: Boolean);
begin
 if DetectGameBuild = 3312 then
   begin
    LuaRegFunc (L, 'play_hud_anim', play_hud_anim, '(game_object *hud_item, string anim, int motin_ID, bool mix_in)', disp);
   end;


end;

procedure    InitModule;
var
   hLib: HMODULE;
begin
 hLib := GetModuleHandle (XR_EXE);
 if (hLib > 0) then
   begin
    CKinematicsAnimated__ID_Cycle := GetProcAddress (hLib, '?ID_Cycle@CKinematicsAnimated@@QAE?AUMotionID@@PBD@Z');
    CKinematicsAnimated__PlayCycle := GetProcAddress (hLib, '?PlayCycle@CKinematicsAnimated@@QAEPAVCBlend@@UMotionID@@HP6AXPAV2@@ZPAXE@Z');
   end;

end;


initialization
 patch_list := TList.Create;
 {$IFNDEF NEWEST_BUILD}
 patch_list.Add ( @detail_blink_fix_patch );
 {$ENDIF}

finalization
 patch_list.Free;
end.
