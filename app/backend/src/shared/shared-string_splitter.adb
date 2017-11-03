-- core
with Ada.Strings.Fixed;
-- library
with GNAT.String_Split;

package body Shared.String_Splitter is

   package SFix   renames Ada.Strings.Fixed;
   package GSplit renames GNAT.String_Split;

   function Filter_Delimiters (Raw_Input : String; Delimiter : String)
   return String is
     SU_Aux : SU.Unbounded_String := SU.To_Unbounded_String (Raw_Input);
   begin
         while SFix.Count (SU.To_String (SU_Aux), Delimiter) > 0
         loop-- remove delimiters if any exists
            declare
               Pos : Natural := SFix.Index(SU.To_String (SU_Aux), Delimiter);
            begin
               SU_Aux := SU.To_Unbounded_String (SFix.Delete (SU.To_String (SU_Aux), Positive (Pos),Pos));
            end;
          end loop;
      return SU.To_String (SU_Aux);
   end Filter_Delimiters;

   function Filter_Separator (Raw_Input : String; Separator : String)
   return String_List.List
   is
     Result               : String_List.List := String_List.Empty_List;
     Substrings           : GSplit.Slice_Set;
     Number_Of_Substrings : GSplit.Slice_Number;
   begin
      GSplit.Create (S          => Substrings,
                     From       => Raw_Input,
                     Separators => Separator,
                     Mode       => GSplit.Multiple);
      Number_Of_Substrings := GSplit.Slice_Count (Substrings);
      for Index in 1 .. Number_Of_Substrings loop
        declare
           Sub : constant String := GSplit.Slice (Substrings, Index);
        begin
            -- identify valid strings
            if not (Sub = "") then
              Result.Append (Sub);
            end if;
        end;
      end loop;
      return Result;
   end Filter_Separator;

   function Add_Character (
      Input : SU.Unbounded_String;
      After : Character;
      Add   : Character)
   return SU.Unbounded_String
   is
      Output : SU.Unbounded_String := SU.To_Unbounded_String ("");
   begin
      for I in 1 .. SU.Length (Input) loop
         if SU.Element (Input, I) = After then
            SU.Append (Output, Add);
         else
            SU.Append (Output, SU.Element (Input, I));
         end if;
      end loop;
      return Output;
   end Add_Character;

end Shared.String_Splitter;
