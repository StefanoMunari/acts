with Shared.String_Splitter;
-- core
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
-- library
with GNAT.String_Split;

package body Shared.JSON_Splitter is
   use Ada.Containers;

   package SU renames Ada.Strings.Unbounded;
   package SFix renames Ada.Strings.Fixed;
   package GSplit renames GNAT.String_Split;

   function Split (Input : in JSON_Format.Object)
   return String_Map.Data.Map
   is
      use Shared.String_Splitter;
      Raw_Input : String
         := Filter_Delimiters
               (Filter_Delimiters (JSON_Format.Write(Input), "{"), "}");
      Keys      : String_List.List;
      Raw_Map   : String_Map.Data.Map;
   begin
      Keys := Extract_Keys (Raw_Input);
      for Element of Keys
      loop
         declare
            Cleaned_Element : String := Filter_Delimiters
               (JSON_Format.Write
                  (JSON_Format.Get
                     (Input, Element)), """");
         begin
            Raw_Map.Insert(Element, Cleaned_Element);
         end;
      end loop;
      return Raw_Map;
   end Split;

-- private
   function Extract_Keys (Raw_Input : String) return String_List.List
   is
      use Shared.String_Splitter;
      Separator : String := ":";
      Result : String_List.List;
      Delimiter : String := """";
      Substrings : GSplit.Slice_Set;
      Number_Of_Substrings : GSplit.Slice_Number;
      First : Boolean := True;
   begin
      GSplit.Create (S               => Substrings,
                     From          => Raw_Input,
                     Separators => Separator,
                     Mode          => GSplit.Multiple);
      Number_Of_Substrings := GSplit.Slice_Count (Substrings);
      for Index in 1 .. Number_Of_Substrings loop
         declare
            Sub : constant String := GSplit.Slice (Substrings, Index);
            Aux_List : String_List.List;
            Count : Natural := 0;
         begin
            -- identify valid json key
            if not (Sub = "") and not Is_JSON_Array (Sub)
            then
               Aux_List := Filter_Separator (Sub, ",");
               declare
                  Aux_String : String := Aux_List.Last_Element;
               begin
                  if First then
                     Result.Append (Filter_Delimiters (Aux_String, Delimiter));
                     First := False;
                  elsif Aux_List.Length > 1 then
                     Result.Append (Filter_Delimiters (Aux_String, Delimiter));
                  end if;
               end;
            end if;
         end;
      end loop;
      return Result;
   end Extract_Keys;

   function Is_JSON_Array (Raw_Input : String)
   return Boolean is
   begin
      return (SFix.Head (Raw_Input, 2) = " [");
   end Is_JSON_Array;

end Shared.JSON_Splitter;
