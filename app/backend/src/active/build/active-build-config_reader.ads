------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-config_reader
-- @purpose Reads the configuration for the whole active subsystem
-- @interface Get_Instance() -> Reference:
--              returns an instance of the active configuration reader
--            Read_Config (Object, String) -> Boolean
--              reads the configuration for the active subsystem. Returns true
--              iff there were no errors during the process
-- @dependencies application-backend::active-build-traffic_light-config_reader
--               application-backend::active-build-traveller-config_reader
--               application-backend::shared-agent_id_list
--               application-backend::shared-reader
--               application-backend::shared-reader-json
-- @details -
------------------------------------------------------------------------------

-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Build.Traffic_Light_Config_Reader;
with Active.Build.Traveller.Config_Reader;

with Shared.Agent_Id_List;
with Shared.Reader;
with Shared.Reader.JSON;

package Active.Build.Config_Reader is

   package G_JSON        renames GNATCOLL.JSON;
   package Agent_Id_List renames Shared.Agent_Id_List;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   function Get_Instance (
      File_Reader :
        access Shared.Reader.Object'Class := null;
      Traffic_Light_Reader   :
        access Traffic_Light_Config_Reader.Object'Class := null;
      Traveller_Reader   :
        access Traveller.Config_Reader.Object'Class := null)
   return Config_Reader.Reference;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String) return Boolean;

private
   type Object is new Ada.Finalization.Controlled with record
      File_Reader          :
         access Shared.Reader.Object'Class := null;
      Traffic_Light_Reader :
         access Traffic_Light_Config_Reader.Object'Class := null;
      Traveller_Reader     :
         access Traveller.Config_Reader.Object'Class := null;
   end record;

   Instance : Config_Reader.Reference;

   function Read_Travellers (This       : in out Config_Reader.Object;
                             Travellers :        G_JSON.JSON_Array)
   return Agent_Id_List.List;

   function Read_Traffic_Lights (This           : in out Config_Reader.Object;
                                 Traffic_Lights :        G_JSON.JSON_Array)
   return Agent_Id_List.List;

end Active.Build.Config_Reader;
