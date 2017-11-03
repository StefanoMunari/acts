------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::passive-road_sign-speed_limit
-- @purpose Represents a speed limit signal
-- @interface Apply (Object):
--              @inherit
--            Get_All_Bus_Stops (Object) -> Map:
--              returns a map composed in a fashion like `Bus -> Bus stops`
--            Register_For_Bus_Waiting (Object, Agent_Id, Agent_Id):
--              commits a pedestrian to wait for a specific bus
--            Get_Waiting_For_Bus (Object, Agent_Id) -> List:
--              returns the list of pedestrians waiting for a given bus
-- @dependencies application-backend::active-bus_service-utils
--               application-backend::shared-agent_id_list
--               application-backend::shared-infra_id_list
--               application-backend::shared-agent_id_to_agent_id_list_map
--               application-backend::shared-agent_id_to_infra_id_list_map
-- @details Implementation of Road_Sign. Half of the methods are not listed in
--          the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Bus_Service.Utils;

with Shared.Agent_Id_List;
with Shared.Infra_Id_List;
with Shared.Agent_Id_To_Agent_Id_List_Map;
with Shared.Agent_Id_To_Infra_Id_List_Map;

package Passive.Road_Sign.Bus_Stop is

   package Bus_Service renames Active.Bus_Service;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Agent_Id_To_Agent_Id_List_Map
      renames Shared.Agent_Id_To_Agent_Id_List_Map;
   package Agent_Id_To_Infra_Id_List_Map
      renames Shared.Agent_Id_To_Infra_Id_List_Map;

   type Object is
      new Road_Sign.Object
   with private;
   type Reference is access all Bus_Stop.Object'Class;

   not overriding
   function Create (
      Buses             : in     Agent_Id_List.List;
      Waiting_Ids       : in     Agent_Id_To_Agent_Id_List_Map.Map;
      Stops             : in     Agent_Id_To_Infra_Id_List_Map.Map;
      Bus_Service_Utils : access Bus_Service.Utils.Object'Class := null)
   return Passive.Road_Sign.Bus_Stop.Reference;

   overriding
   procedure Apply (This       : in out Bus_Stop.Object;
                    Traveller  : in     Agent.Agent_Id);

   function Get_All_Bus_Stops (This : in Bus_Stop.Object)
   return Agent_Id_To_Infra_Id_List_Map.Map;

   procedure Register_For_Bus_Waiting (
      This       : in out Bus_Stop.Object;
      Waiting_Id : in     Agent.Agent_Id;
      Bus_Id     : in     Agent.Agent_Id);

   function Get_Waiting_For_Bus (
      This   : in out Bus_Stop.Object;
      Bus_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

   procedure Remove_From_Waiting_List (
      This                   : in out Bus_Stop.Object;
      Bus_Id                 : in     Agent.Agent_Id;
      Not_Waiting_Anymore_Id : in     Agent.Agent_Id);

   not overriding
   function Is_A_Stop_For_Bus (This : in out Bus_Stop.Object;
                               Bus  : in     Agent.Agent_Id)
   return Boolean;

   overriding
   function Dump (This : Bus_Stop.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Bus_Stop_Field     return String is ("busStop");
   function Id_Field           return String is ("id");
   function Waiting_List_Field return String is ("waitingList");
   function Stops_Field        return String is ("stops");

private

   type Object is
     new Road_Sign.Object
   with record
     Buses       : Agent_Id_List.List;
     Waiting_Ids : Agent_Id_To_Agent_Id_List_Map.Map;
     Stops       : Agent_Id_To_Infra_Id_List_Map.Map;
     Bus_Service_Utils : Bus_Service.Utils.Reference;
   end record;

end Passive.Road_Sign.Bus_Stop;
