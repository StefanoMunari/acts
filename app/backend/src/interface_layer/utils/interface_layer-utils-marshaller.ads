with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Marshaller is

   type Object is interface;
   type Reference is access all Marshaller.Object'Class;

   procedure Marshalling (This : in Marshaller.Object;
          Stream : out Shared.Indefinite_String_Map.Data.Map) is abstract;

end Interface_Layer.Utils.Marshaller;
