unit xmltools;

interface
uses Windows, SysUtils, Classes, ContNrs, XMLDoc, XMLIntf, Misc;


type
   TLuaXMLDocument = class (TXMLDocument)
  private
    function GetNode(nIndex: Integer): TXMLNode;
   protected
    FNodeList: TObjectList;
  published


   public


    { props }
    property            Nodes[nIndex: Integer]: TXMLNode read GetNode;

    { C & D }
    destructor          Destroy; override;

    { methods }

    procedure           KillNode (ANode: TXMLNode);
    procedure           SaveNode (ANode: TXMLNode);

   end; // TLuaXMLDocument

implementation


{ TLuaXMLDocument }

destructor TLuaXMLDocument.Destroy;
var n: Integer;
begin
 if FNodeList <> nil then
    for n := FNodeList.Count - 1 downto 0 do
        KillNode ( TXMLNode (FNodeList[n]) );

 FNodeList.Free;
 inherited;
end;

function TLuaXMLDocument.GetNode(nIndex: Integer): TXMLNode;
begin
 result := TXMLNode ( FNodeList [nIndex] );
end;

procedure TLuaXMLDocument.KillNode(ANode: TXMLNode);
begin
 if FNodeList <> nil then
    FNodeList.Remove (ANode);

 if (ANode as IXMLNode)._Release = 0 then
     ANode.Free;
end;

procedure TLuaXMLDocument.SaveNode(ANode: TXMLNode);
begin
 if FNodeList = nil then
    FNodeList := TObjectList.Create (FALSE);

 if FNodeList.IndexOf (ANode) < 0 then
  begin
   (ANode as IXMLNode)._AddRef;
   FNodeList.Add (ANode);
  end;
end;

end.
