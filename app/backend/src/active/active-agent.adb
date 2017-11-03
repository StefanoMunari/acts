with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Active.Agent is

   use SU;

   function Create_Id_From_Natural (Id : Integer) return Agent_Id
   is
      Id_Str : String := Trim (Integer'Image (Integer (Id)), Both);
   begin
     return Agent_Id (SU.To_Unbounded_String (Id_Str));
   end Create_Id_From_Natural;

   function "=" (A, B : Agent_Id) return Boolean is
   begin
     return SU."=" (SU.Unbounded_String (A), SU.Unbounded_String (B));
   end "=";

   function "<" (A, B : Agent_Id) return Boolean is
   begin
     return SU."<" (SU.Unbounded_String (A), SU.Unbounded_String (B));
   end "<";

end Active.Agent;
