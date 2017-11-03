separate (Interface_Layer.Presentation.Converter.JSON)

procedure Parse_Raw (This        : in     JSON.Object;
                     Pair        : in     String_Map.Data.Cursor;
                     Separator   : in     String;
                     Accumulator : in out JSON_Array_Ref_HMap.Map)
is
   Parsed_Key  : String
      := This.Parse_Key (String_Map.Data.Key (Pair), Separator);
   Parsed_List : Utils.JSON_Array_Ref
      := This.Parse_List (String_Map.Data.Element (Pair), Separator);
   List_Size   : Natural
      := G_JSON.Length (Parsed_List.all);
   Result      : Utils.JSON_Array_Ref;
    -- TODO: implement scoped_pointers
   procedure Free is
      new Ada.Unchecked_Deallocation (G_JSON.JSON_Array, Utils.JSON_Array_Ref);
begin

   if Accumulator.Contains (Parsed_Key)
   then
      Result := Accumulator.Element (Parsed_Key);
   else
      Result := new G_JSON.JSON_Array;
   end if;

   for Index in 1 .. List_Size loop
     G_JSON.Append (Result.all, G_JSON.Get (Parsed_List.all, Index));
   end loop;

   if not Accumulator.Contains (Parsed_Key)
   then
       Accumulator.Insert (Parsed_Key, Result);
   end if;

   Free (Parsed_List);

end Parse_Raw;
