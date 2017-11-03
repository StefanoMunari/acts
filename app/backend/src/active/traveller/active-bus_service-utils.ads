------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-bus_service-utils
-- @purpose Helper package which provides methods to access bus services
-- @interface Get_Instance (Object):
--              returns the singleton instance
--            Is_A_Bus_Service (Object, Agent_Id):
--              true iff the provided belongs to a bus service
--            On_Bus_Stop (Object, Agent_Id):
--              forwards the call to the intended bus service
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Bus_Service.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Bus_Service.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Bus_Service.Utils.Reference;

   not overriding
   function Is_A_Bus_Service (
      This         : in out Bus_Service.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean;

   not overriding
   procedure On_Bus_Stop (This         : in out Bus_Service.Utils.Object;
                          Traveller_Id : in     Agent.Agent_Id);

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Bus_Service.Utils.Reference := null;

end Active.Bus_Service.Utils;
