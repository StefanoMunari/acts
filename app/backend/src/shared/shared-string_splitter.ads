-- core
with Ada.Strings.Unbounded;
-- local
with Shared.String_List;

package Shared.String_Splitter is

   package SU          renames Ada.Strings.Unbounded;
   package String_List renames Shared.String_List;

   function Filter_Delimiters (
      Raw_Input : String;
      Delimiter : String)
   return String;

   function Filter_Separator (
      Raw_Input : String;
      Separator : String)
   return String_List.List;

   function Add_Character (
      Input : SU.Unbounded_String;
      After : Character;
      Add   : Character)
   return SU.Unbounded_String;

end Shared.String_Splitter;