------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-people_carrier
-- @purpose Interface for entities which transport people
-- @interface Board (Object, Agent_Id) -> Boolean:
--              makes a traveller board the people carrier
--            Free (Object, Agent_Id) -> Boolean:
--              lands a traveller
--            Get_Passengers (Object) -> List:
--              returns the list of current passengers
-- @dependencies application-backend::active-agent
--               application-backend::shared-agent_id_list
-- @details Interface
------------------------------------------------------------------------------

with Active.Agent;

with Shared.Agent_Id_List;

package Active.People_Carrier is

   package Agent renames Active.Agent;
   package Agent_Id_List renames Shared.Agent_Id_List;

   type Object is interface;
   type Reference is access all People_Carrier.Object'Class;

   not overriding
   procedure Board (This    : in out People_Carrier.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean) is abstract;

   not overriding
   procedure Free (This      : in out People_Carrier.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean) is abstract;

   not overriding
   function Get_Passengers (This : in People_Carrier.Object)
   return Agent_Id_List.List is abstract;

   not overriding
   function Get_Max_Number_Of_Passengers (This : in People_Carrier.Object)
   return Natural is abstract;

   not overriding
   function Count_Passengers (This : in People_Carrier.Object)
   return Natural is abstract;

end Active.People_Carrier;
