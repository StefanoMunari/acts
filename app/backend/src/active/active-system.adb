-- core
with Ada.Environment_Variables;

with GNATCOLL.JSON;

with Active.Build.Config_Reader;
with Active.Build.AI.Config_Reader;
with Active.Space_Master;

with Reactive.District;

package body Active.System is

   package EV renames Ada.Environment_Variables;
   package CR renames Active.Build.Config_Reader;
   package AI_CR renames Active.Build.AI.Config_Reader;
   package G_JSON renames GNATCOLL.JSON;

   procedure Init
   is
-- AI_Reader
   AI_Config_Reader : AI_CR.Object;
-- Reader
   Config_Reader    : CR.Reference := CR.Get_Instance;
-- City identifier
   City_Id    : String := EV.Value (Name => "CITY_NODE_ID");
-- Configuration Path of Active
   Config_Path      : String
      := (EV.Value (Name => "CITY_ROOT")) & "/etc/init/" & City_Id & "/";
   District_Id      : String := EV.Value (Name => "CITY_DISTRICT_ID");
-- Names of the configuration files
   City_File        : String := "district.conf";
   Active_File      : String := "active" & District_Id & ".conf";
   Result           : Boolean;
   begin
   -- Generate the necessary files for AI
      AI_Config_Reader.Read_Config (City_File);
   -- Config and builds the necessary active entities
      Result := Config_Reader.Read_Config (Config_Path & Active_File);
   end Init;

   procedure Dump
   is
      Space_Master_Ref         : Active.Space_Master.Reference
         := Active.Space_Master.Get_Instance;
      Max_Speed                : Natural
         := Natural (Space_Master_Ref.Get_Max_Speed);
      District_Ref             : Reactive.District.Reference
         := Reactive.District.Get_Instance;
      Travellers_JSON_Dump     : G_JSON.JSON_Value
         := District_Ref.Dump_Travellers;
      Traffic_Lights_JSON_Dump : G_JSON.JSON_Value
         := District_Ref.Dump_Traffic_Lights;
      Active_JSON_Dump         : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin -- Dump
      Active_JSON_Dump.Set_Field ("maxSpeed", Max_Speed);
      Active_JSON_Dump.Set_Field ("trafficLights", Traffic_Lights_JSON_Dump);
      Active_JSON_Dump.Set_Field ("travellers", Travellers_JSON_Dump);
   -- Dump could be written somewhere
   end Dump;

end Active.System;