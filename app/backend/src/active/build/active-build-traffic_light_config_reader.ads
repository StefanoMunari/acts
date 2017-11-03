------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-traffic_light_config_reader
-- @purpose Reads the configuration for creating a single traffic light
-- @interface Read (Object, JSON) -> Agent_Id
--              creates of a traffic light and returns its id
-- @dependencies application-backend::active-agent
-- @details -
------------------------------------------------------------------------------

-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

package Active.Build.Traffic_Light_Config_Reader is

   package G_JSON renames GNATCOLL.JSON;
   package Agent  renames Active.Agent;
   use Agent;

   type Object is new Ada.Finalization.Controlled with null record;
   type Reference is access all Object'Class;

   not overriding
   function Read (
      This               : in out Traffic_Light_Config_Reader.Object;
      Traffic_Light_Json : in     G_JSON.JSON_Value)
   return Agent.Agent_Id;

private

   Id_Field     : constant String := "id";
   Color_Field  : constant String := "color";
   Period_Field : constant String := "period";

   Red_Color    : constant String := "RED";
   Green_Color  : constant String := "GREEN";

end Active.Build.Traffic_Light_Config_Reader;
