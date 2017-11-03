-- local
with Interface_Layer.Utils.Unmarshaller;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Unmarshaller.Ack is

   package Unmarshaller renames Interface_Layer.Utils.Unmarshaller;
   package String_Map   renames Shared.Indefinite_String_Map;

   type Object is
      new Unmarshaller.Object
   with record
      Ack : Boolean;
   end record;
   type Reference is access all Ack.Object'Class;

   overriding
   procedure Unmarshalling (
      This       : in out Ack.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Unmarshaller.Ack;
