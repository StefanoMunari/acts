package body Interface_Layer.Utils.Unmarshaller.Ack is

   procedure Unmarshalling (
      This       : in out Ack.Object;
      Stream_Map : in     Shared.Indefinite_String_Map.Data.Map)
   is
   begin
      This.Ack := Boolean'Value (Stream_Map.Element ("Ack"));
   end Unmarshalling;

end Interface_Layer.Utils.Unmarshaller.Ack;
