separate (Interface_Layer.Presentation.Converter.JSON)

function Encode (
   This : in JSON.Object;
   Raw  : in String_Map.Data.Map)
return XFormat.Object'Class
is
   Message     : JSON_Format.Object := JSON_Format.Create_Object;
   Separator   : String := " ";
   Accumulator : JSON_Array_Ref_HMap.Map;
-- TODO: implement scoped_pointers
   procedure Free is
      new Ada.Unchecked_Deallocation (G_JSON.JSON_Array, Utils.JSON_Array_Ref);
begin
   for Pair in Raw.Iterate loop
      if SFix.Index (String_Map.Data.Key (Pair), Separator) > 0 then
         This.Parse_Raw (Pair, Separator, Accumulator);
      else
         Message.Set_Field (
            Field_Name =>
               String (String_Map.Data.Key (Pair)),  -- Key
            Field      =>
               SFix.Trim (String_Map.Data.Element (Pair),
                          Ada.Strings.Both));  -- Value
         end if;
      end loop;

   -- Convert the accumulated arrays of the
   --+ Accumulator : Map<String,JSON_Array_Ref>
      declare
         JSON_Ref : Utils.JSON_Array_Ref;
      begin
         for Pair in Accumulator.Iterate
         loop
            JSON_Ref := JSON_Array_Ref_HMap.Element (Pair);
            Message.Set_Field (
               Field_Name => JSON_Array_Ref_HMap.Key (Pair),  -- Key
               Field      => JSON_Ref.all);  -- Value (copied by value)
         -- free memory on the heap (already copied)
            Free (JSON_Ref);
        end loop;
      end;

      return Message;
end Encode;
