-- core
with Ada.Environment_Variables;

with GNATCOLL.JSON;

with Reactive.District;
with Reactive.Infrastructure.Build.Config_Reader;

package body Reactive.System is

   package EV     renames Ada.Environment_Variables;
   package G_JSON renames GNATCOLL.JSON;
   package CR     renames Reactive.Infrastructure.Build.Config_Reader;

   procedure Init
   is
   -- Reader
   Config_Reader : CR.Reference := CR.Get_Instance;
-- City identifier
   City_Id       : String := EV.Value (Name => "CITY_NODE_ID");
   -- Configuration Path of Reactive
   Config_Path   : String :=
      (EV.Value (Name => "CITY_ROOT")) & "/etc/init/" & City_Id & "/";
   -- Get this node identifier
   District_Id   : String := EV.Value (Name => "CITY_DISTRICT_ID");
   -- Names of the configuration files
   District_File : String := "district" & District_Id & ".conf";
   Result        : Boolean;
   begin
      ---- Config and builds the necessary reactive entities
      Result := Config_Reader.all.Read_Config (Config_Path & District_File);
   end Init;

   procedure Dump
   is
      District_Ref : Reactive.District.Reference
         := Reactive.District.Get_Instance;
      JSON_Dump    : G_JSON.JSON_Value;
   begin -- Dump
      JSON_Dump := District_Ref.Dump;
      -- Dump could be written somewhere
   end Dump;

end Reactive.System;
