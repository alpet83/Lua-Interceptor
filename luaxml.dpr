library luaxml;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  Windows,
  SysUtils,
  StrUtils,
  Classes,
  ContNrs,
  XmlDom,
  XmlIntf,
  XmlDoc,
  LuaTypes in '..\lib\LuaTypes.pas',
  misc in '..\lib\misc.pas',
  ModuleMgr in '..\lib\ModuleMgr.pas',
  WinHeap in '..\lib\WinHeap.pas',
  FastSync in '..\lib\FastSync.pas',
  DateTimeTools in '..\lib\DateTimeTools.pas',
  StrClasses in '..\lib\StrClasses.pas',
  ArrayTypes in '..\lib\ArrayTypes.pas',
  Algs in '..\lib\Algs.pas',
  UniArray in '..\lib\UniArray.pas',
  MemStat in '..\lib\MemStat.pas',
  XrayLua in 'XrayLua.pas',
  xmltools in 'xmltools.pas',
  LuaTools in '..\lib\LuaTools.pas';

{$R *.res}





var
   garbage: TStrMap = nil;

function FSExpandPath (pSource, pResult: PWideChar): Boolean; stdcall; external 'luaicp.dll';


function StalkerPath (const path: String): String;

begin
 SetLength (result, 512);
 FSExpandPath ( PWideChar (path), PWideChar (result) );
 result := Trim (result);
end; // StalkerPath


procedure MemObject (AObj: TObject);
begin
 if garbage = nil then exit;

 garbage.Lock ('MemObject');
 try
  garbage.AddObject (AObj.ClassName, AObj);
 finally
  garbage.Unlock;
 end;
end; // MemObject

procedure KillObject (AObj: TObject);
var
   i: Integer;

begin
 if garbage = nil then exit;

 garbage.Lock ('KillObject');
 try
  i := garbage.IndexOfObject (AObj);
  if i >= 0 then
    begin
     garbage.Delete (i);
    end;
 finally
  garbage.Unlock;
 end;
end; // KillObject

function NodeObj (node: IXMLNode): TXMLNode;
begin
 result := (node as IXMLNodeAccess).GetNodeObject;
end;


function OpenXMLDoc (L: lua_State): Integer; cdecl;
var
   fname, root: String;
   xdoc: TLuaXMLDocument;
   node: IXMLNode;
begin
 result := 1;
 xdoc := nil;
 if (lua_gettop(L) < 1) then exit;


 fname := LuaStrArg (L);
 root := LuaStrArg (L, 2);

 fname := StalkerPath (fname);

 if root = '' then root := 'docroot';

 try

   if (fname <> '') and ( FileExists (fname)) then
       begin
        xdoc := TLuaXMLDocument.Create (fname);
       end
    else
       begin
        xdoc := TLuaXMLDocument.Create (nil);
        xdoc.Options := xdoc.Options + [doNodeAutoIndent];
        xdoc.ParseOptions := xdoc.ParseOptions + [poPreserveWhiteSpace];
        xdoc.Active := TRUE;
        xdoc.FileName := fname;
        xdoc.Version := '1.0';
        xdoc.Encoding := 'windows-1251';



        // xdoc.DOMDocument.createElement ('test-element');
        //  := xdoc.CreateNode ('docroot', ntElement, '');
        node := xdoc.AddChild (root);
        node.Attributes ['lang'] := 'ru';
        // node.Text := 'blablabla';

        xdoc.SaveNode ( NodeObj (node) );


        // ODS('[~T]. #DBG: XML test dump: ~C0E '#13#10 + xdoc.XML.Text + '~C07');
        if fname <> '' then
          begin
           xdoc.SaveToFile (fname);
          end;

       end;
 except
  on E: Exception do
     OnExceptLog ('OpenXMLDoc ("' + fname + '")', E);
 end;


 if xdoc <> nil then
   begin
    (xdoc as IInterface)._AddRef;
    MemObject (xdoc);
   end;
 lua_pushptr (L, xdoc);
end;

function SaveXMLDoc (L: lua_State): Integer; cdecl;
var
   obj: TObject;
   xdoc: TXMLDocument;
   fname, xmls: String;
   sa: AnsiString;
begin
 result := 1;
 obj := lua_topointer (L, 1);
 fname := '';
 if (lua_gettop (L) >= 2) then fname := LuaStrArg (L, 2);

 fname := StalkerPath (fname);

 xmls := '<error />';

 if (obj <> nil) and (obj is TXMLDocument) then
   begin
    xdoc := obj as TXMLDocument;
    if fname = '' then fname := xdoc.FileName;
    if fname = '' then exit;

    xdoc.Version := '1.0';
    xdoc.Encoding := 'windows-1251';

    xdoc.Options := xdoc.Options + [doNodeAutoIndent];
    xmls := FormatXMLData (xdoc.XML.Text);
    xmls := AnsiReplaceStr (xmls, '<?xml version="1.0"?> ', '<?xml version="1.0" encoding="windows-1251"?>' );

    xdoc.LoadFromXML (xmls);

    xdoc.Encoding := 'windows-1251';

    xdoc.SaveToFile (fname);
    xdoc.SaveToXML (xmls);
    // ODS('[~T]. #DBG: Test SaveXMLDoc/SaveToXML = ~C0E'#13#10 + xmls + '~C07');
   end;
 sa := AnsiString (xmls);

 lua_pushstring (L, PAnsiChar (sa));

end; // SaveXMLDoc


function SetXMLNodeAttr (L: lua_State): Integer; cdecl;
var
   node: IXMLNode;
   nobj: TXMLNode;
    obj: TObject;
   attr, value: String;
begin
 result := 0;
 if (lua_gettop (L) < 3) then exit;

 obj := lua_topointer (L, 1);
 attr  := LuaStrArg (L, 2);
 value := LuaStrArg (L, 3);

 if (obj is TXMLNode) and (attr <> '') then
   begin
    nobj  := obj as TXMLNode;
    node  := nobj as IXMLNode;
    node.Attributes [attr] := value;
   end;

end; // SetXMLNodeAttr

function SetXMLNodeText (L: lua_State): Integer; cdecl;
var
   node: IXMLNode;
   nobj: TXMLNode;
    obj: TObject;
   text: String;
begin
 result := 0;
 if (lua_gettop (L) < 2) then exit;

 obj := lua_topointer (L, 1);
 text  := LuaStrArg (L, 2);

 if (obj is TXMLNode) then
   begin
    nobj  := obj as TXMLNode;
    node  := nobj as IXMLNode;
    node.Text := text;
   end;

end; // SetXMLNodeAttr


function GetXMLNodeAttr (L: lua_State): Integer; cdecl;
var
   node: IXMLNode;
   nobj: TXMLNode;
    obj: TObject;
   attr, value: String;
begin
 result := 1;

 obj := nil;
 attr := '';

 if (lua_gettop (L) >= 2) then
   begin
    obj := lua_topointer (L, 1);
    attr  := LuaStrArg (L, 2);
   end;

 if (obj <> nil) and (obj is TXMLNode) and (attr <> '') then
   begin
    nobj  := obj as TXMLNode;
    node  := nobj as IXMLNode;
    if attr = '%tag' then
       value := node.NodeName
    else
       value := node.Attributes [attr];
   end;

 lua_pushwstr (L, value);
end; // GetXMLNodeAttr

function GetXMLNodeText (L: lua_State): Integer; cdecl;
var
   node: IXMLNode;
   nobj: TXMLNode;
    obj: TObject;
   text: String;
begin
 result := 1;
 obj := nil;
 if (lua_gettop (L) >= 1) then
     obj := lua_topointer (L, 1);



 if (obj <> nil) and (obj is TXMLNode) then
   begin
    nobj  := obj as TXMLNode;
    node  := nobj as IXMLNode;
    text := node.Text;
   end;

 lua_pushwstr (L, text);
end; // GetXMLNodeAttr


function CloseXMLDoc (L: lua_State): Integer; cdecl;
var
   obj: TObject;
   flags: DWORD;
   xdoc: TXMLDocument;
begin
 result := 0;
 flags := 0;
 obj := lua_topointer (L, 1);
 if (lua_gettop (L) > 1) then flags := lua_tointeger (L, 2);

 if (obj <> nil) and (obj is TXMLDocument) then
   begin
    xdoc := obj as TXMLDocument;
    if flags and 1 <> 0 then xdoc.SaveToFile ();
    KillObject (xdoc);
   end;

end;


function CreateXMLNode (L: lua_State): Integer; cdecl;
var
   obj: TObject;
   n, argc: Integer;
   xdoc: TLuaXMLDocument;
   s, tag, uri: String;
   node, cnode: IXMLNode;
   parent, res: TXMLNode;
   nt: TNodeType;

begin
 result := 1;
 res := nil;
 obj := nil;
 parent := nil;
 tag := '';
 uri := '';
 argc := lua_gettop (L);

 if argc >= 3 then
  begin
   obj := lua_topointer (L, 1);
   parent := TXMLNode ( lua_topointer (L, 2) );
   tag := LuaStrArg (L, 3);
  end;

 nt := ntElement;
 if lua_gettop (L) >= 4 then
   begin
    s := LuaStrArg (L, 4);
    if s = 'text' then nt := ntText;
    if s = 'attr' then nt := ntAttribute;
   end;

 if argc >= 5 then
   uri := LuaStrArg (L, 5);


 try
   if (obj <> nil) and (obj is TLuaXMLDocument) and (tag <> '') then
     begin
      xdoc := TLuaXMLDocument (obj);

      if parent = nil then
        begin
         cnode := nil;
         for n := 0 to xdoc.ChildNodes.Count - 1 do
           begin
            cnode := xdoc.ChildNodes.Get (n);
            if cnode.NodeType = ntElement then break;
           end;

         if cnode <> nil then parent := (cnode as IXMLNodeAccess).GetNodeObject;
        end;

      xdoc.Options := xdoc.Options + [doNodeAutoIndent];

      node := xdoc.CreateNode (tag, nt, uri);


      if parent <> nil then
        begin
         (parent as IXMLNode).ChildNodes.Add (node);
         res := (node as IXMLNodeAccess).GetNodeObject;
         xdoc.SaveNode (res);
        end;
     end;
 except
  on E: Exception do
     OnExceptLog ('CreateXMLNode ("' + tag + '", "' + uri + '")', E);

 end;

 lua_pushptr (L, res);
end;

function ReqFindNode (src: IXMLNode; tag, attr, value: String): IXMLNode;
var
   find_last: Boolean;
   n: Integer;
   rn: IXMLNode;
begin
 result := nil;


 find_last := ( attr = '%last' );

 for n := 0 to src.ChildNodes.Count - 1 do
  begin
   rn := src.ChildNodes.Get (n);
   if (rn.NodeType = ntElement) then
      begin
       // определение по тегу
       if (rn.NodeName = tag) and ( (attr = '') or (find_last) or (rn.Attributes [attr] = value) ) then
           result := rn
       else
           if (rn.HasChildNodes) then
             begin
              rn := ReqFindNode (rn, tag, attr, value);
              if rn <> nil then
                 result := rn;
             end;
      end;
   if (not find_last) and (result <> nil) then exit;
  end;

end;


function FindXMLNode (L: lua_State): Integer; cdecl;
var
   obj: TObject;
   node: IXMLNode;
   nobj, snode: TXMLNode;

   argc: Integer;
   xdoc: TLuaXMLDocument;
   tag, attr, value: String;

begin
 result := 1;
 snode := nil;
 obj := nil;
 nobj := nil;
 tag := '';
 argc := lua_gettop (L);
 attr := '';
 value := '';


 if argc >= 3 then
  begin
   // поиск первого нода с тегом
   obj := lua_topointer (L, 1);
   snode := lua_topointer (L, 2);
   tag := LuaStrArg (L, 3);
  end;

 if argc >= 5 then
  begin
   attr  := LuaStrArg (L, 4);
   value := LuaStrArg (L, 5);
  end;

 try
   if (obj <> nil) and (obj is TLuaXMLDocument) and (tag <> '') then
     begin
      xdoc := TLuaXMLDocument (obj);

      if snode = nil then
         snode := NodeObj (xdoc.Node);

      node := ReqFindNode(snode, tag, attr, value);

      if node <> nil then
        begin
         nobj := NodeObj (node);
         xdoc.SaveNode (nobj);
        end;
     end;
 except
  on E: Exception do
     OnExceptLog ('FindXMLNode ("' + tag + '", "' + attr + '", "' + value + '")', E);

 end;



 lua_pushptr (L, nobj);
end;



function CaptureFunc (L: lua_State): Integer; cdecl;
begin
 lua_register ( L, 'OpenXMLDoc',  OpenXMLDoc);
 lua_register ( L, 'SaveXMLDoc', SaveXMLDoc);

 lua_register ( L, 'GetXMLNodeAttr', GetXMLNodeAttr);
 lua_register ( L, 'GetXMLNodeText', GetXMLNodeText);

 lua_register ( L, 'SetXMLNodeAttr', SetXMLNodeAttr);
 lua_register ( L, 'SetXMLNodeText', SetXMLNodeText);

 lua_register ( L, 'CloseXMLDoc', CloseXMLDoc);

 lua_register ( L, 'CreateXMLNode', CreateXMLNode);
 lua_register ( L, 'FindXMLNode', FindXMLNode);

 result := 0;
end; // CaptureFunc

procedure LibInit ( pinf: PLibInitInfo ); stdcall;
begin
 // TODO: init procs
 Misc.LibInit (pinf);
 garbage := TStrMap.Create;
 garbage.OwnsObjects := TRUE;
end;

exports
     CaptureFunc, LibInit;


procedure LibProc (reason: Integer);
begin
 case reason of
   DLL_PROCESS_DETACH:
      begin
       if garbage.Count > 0 then
          ODS('[~T]. #DBG(~M): В сборщике мусора остались следующие объекты: ' + garbage.CommaText);
       garbage.Free;
      end;
 end;
end; // LibProc


begin
end.
