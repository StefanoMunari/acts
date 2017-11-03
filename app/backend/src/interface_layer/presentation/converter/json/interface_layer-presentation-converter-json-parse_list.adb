separate (Interface_Layer.Presentation.Converter.JSON)

function Parse_List (This      : in JSON.Object;
                     Raw_List  : in String;
                     Separator : in String)
return Utils.JSON_Array_Ref is
   use GNAT; -- view String_Split and other utilities
   Substrings           : String_Split.Slice_Set;
   Elements             : Utils.JSON_Array_Ref := new G_JSON.JSON_Array;
   Number_Of_Substrings : String_Split.Slice_Number;
begin
   String_Split.Create (S          => Substrings,
                        From       => Raw_List,
                        Separators => Separator,
                        Mode       => String_Split.Multiple);

   Number_Of_Substrings := String_Split.Slice_Count (Substrings);
   for Index in 1 .. Number_Of_Substrings loop
      declare
         Sub : constant String := String_Split.Slice (Substrings, Index);
      begin
         if not (Sub = "") then
            G_JSON.Append (Elements.all, G_JSON.Create (Sub));
         end if;
      end;
   end loop;

   return Elements;
end Parse_List;
