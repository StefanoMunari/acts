with Shared.String_Splitter;

package body Interface_Layer.Utils.Unmarshaller.Message is

   overriding
   procedure Unmarshalling (
      This       : in out Message.Object;
      Stream_Map : in     Shared.Indefinite_String_Map.Data.Map)
   is
      Escape : Character := '\';
      Quote  : Character := '"';
      Aux_Message       : SU.Unbounded_String;
      Formatted_Message : SU.Unbounded_String := SU.To_Unbounded_String ("");
   begin
      Aux_Message := SU.To_Unbounded_String (Stream_Map.Element ("Message"));
      This.Message := Shared.String_Splitter.Add_Character (
         Aux_Message, Escape, Quote);
   end Unmarshalling;

end Interface_Layer.Utils.Unmarshaller.Message;
