------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-people_carrier-utils
-- @purpose Helper package which provides methods to access people carriers
-- @interface Get_Instance (Object, Agent_id):
--              returns the singleton instance
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton. Most of the API is omitted since self-explanatory, it
--          just consists of forwarded invocations to the actual travellers
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.People_Carrier.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all People_Carrier.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return People_Carrier.Utils.Reference;

   not overriding
   function Is_A_People_Carrier (
      This         : in     People_Carrier.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean;

   not overriding
   function Get_Passengers (
      This       : in     People_Carrier.Utils.Object;
      Carrier_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

   not overriding
   procedure Board (
      This       : in     People_Carrier.Utils.Object;
      Carrier_Id : in     Agent.Agent_Id;
      Incomer_Id : in     Agent.Agent_Id;
      Boarded    :    out Boolean);

    not overriding
    procedure Free (
      This         : in out People_Carrier.Utils.Object;
      Carrier_Id   : in     Agent.Agent_Id;
      Passenger_Id : in     Agent.Agent_Id;
      Freed        :    out Boolean);

   not overriding
   function Get_Max_Number_Of_Passengers (
      This       : in People_Carrier.Utils.Object;
      Carrier_Id : in Agent.Agent_Id)
   return Natural;

   function Is_Carrier_Full (This       : in out People_Carrier.Utils.Object;
                             Carrier_Id : in     Agent.Agent_Id)
   return Boolean;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : People_Carrier.Utils.Reference := null;

end Active.People_Carrier.Utils;
