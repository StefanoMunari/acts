------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-bus-utils
-- @purpose Helper package which provides methods to access buses
-- @interface Get_Instance () -> Reference:
--              returns the singleton instance
--            Is_A_Bus (Object, Agent_Id) -> Boolean:
--              true iff the provided belongs to a bus
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Traveller.Vehicle.Bus.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Bus.Utils.Object'Class;

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
    return Bus.Utils.Reference;

   not overriding
   function Is_A_Bus (
      This         : in out Bus.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

   not overriding
   function Get_Route_Stops (
      This   : in Bus.Utils.Object;
      Bus_Id : in Agent.Agent_Id)
   return Infra_Id_List.List;

private
   type Object is tagged limited record
      District_Ref : access Reactive.District.Object'Class;
   end record;

   Instance : Bus.Utils.Reference := null;

end Active.Traveller.Vehicle.Bus.Utils;
