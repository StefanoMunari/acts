-- local
with Ada.Strings.Unbounded;

with Interface_Layer.Utils.Unmarshaller;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Unmarshaller.Message is

   package Unmarshaller renames Interface_Layer.Utils.Unmarshaller;
   package SU           renames Ada.Strings.Unbounded;
   package String_Map   renames Shared.Indefinite_String_Map;

   type Object is
     new Unmarshaller.Object
   with record
      Message : SU.Unbounded_String;
   end record;
   type Reference is access all Message.Object'Class;

   overriding
   procedure Unmarshalling (
      This       : in out Message.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Unmarshaller.Message;
