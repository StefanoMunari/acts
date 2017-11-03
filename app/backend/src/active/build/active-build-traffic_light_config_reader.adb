with Ada.Strings.Unbounded;

with Active.Traffic_Light;

with Active.Build.Exceptions;
use Active.Build.Exceptions;

package body Active.Build.Traffic_Light_Config_Reader is

   package SU renames Ada.Strings.Unbounded;
   package Traffic_Light_Pkg renames Active.Traffic_Light;
-- use
   use Traffic_Light_Pkg;

   function Read (
      This               : in out Traffic_Light_Config_Reader.Object;
      Traffic_Light_Json : in     G_JSON.JSON_Value)
   return Agent.Agent_Id
   is
      Traffic_Light     : aliased Traffic_Light_Pkg.Reference;
      Traffic_Light_Int : Integer;
      Traffic_Light_Id  : Agent.Agent_Id;
      Color_SU          : SU.Unbounded_String;
      Color             : Traffic_Light_Color;
      Period            : Integer;
   begin
   -- ID
      if not Traffic_Light_Json.Has_Field (Id_Field) then
         Raise_Missing_Field_For_Traffic_Light (Id_Field);
      end if;
      Traffic_Light_Int := Traffic_Light_Json.Get (Id_Field);
      Traffic_Light_Id  := Agent.Create_Id_From_Natural (Traffic_Light_Int);

      Color_SU := Traffic_Light_Json.Get (Color_Field);
      if SU.To_String (Color_SU) = Red_Color then
         Color := RED;
      elsif SU.To_String (Color_SU) = Green_Color then
         Color := GREEN;
      else
         Raise_Missing_Field_For_Traffic_Light (Color_Field);
      end if;

      Period := Traffic_Light_Json.Get (Period_Field);

      Traffic_Light :=
         Traffic_Light_Pkg.Create (Traffic_Light_Id, Color, Period);

      return Traffic_Light_Id;
   end Read;

end Active.Build.Traffic_Light_Config_Reader;
