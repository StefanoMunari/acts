separate (Interface_Layer.Presentation.Converter.JSON)

function Decode (
  This    : in JSON.Object;
  Message : in XFormat.Object'Class)
return String_Map.Data.Map
is
   Aux_Message : JSON_Format.Object := JSON_Format.Create_Object;
   Elements    : String_Map.Data.Map;
begin
   Aux_Message := JSON_Format.Object (Message);
   Elements    := JSON_Splitter.Split (Aux_Message);
   return Elements;
end Decode;
