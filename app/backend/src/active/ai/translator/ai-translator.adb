with Interfaces.C;

package body AI.Translator is
   package IC renames Interfaces.C;

   function Sys (Arg : IC.Char_Array) return Integer;
   pragma Import(C, Sys, "system");

   procedure Translate_To_Infrastructure(Snapshot_Name : in String) is
      Return_Value : Integer;
   begin
      Return_Value :=
         Sys(IC.To_C(
            "python " & Local_Path &
            "'topology_generator.py' " & Data_Path & Snapshot_Name));
   end Translate_To_Infrastructure;

   procedure Translate_To_Costs(Snapshot_Name : in String) is
      Return_Value : Integer;
   begin
      Return_Value :=
         Sys(IC.To_C(
            "python " & Local_Path &
            "'costs_generator.py' " & Data_Path & Snapshot_Name));
   end Translate_To_Costs;
end AI.Translator;
