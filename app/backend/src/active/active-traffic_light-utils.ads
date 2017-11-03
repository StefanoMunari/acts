------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traffic_light-utils
-- @purpose Helper package which provides methods to access traffic lights
-- @interface Get_Instance (Object):
--              returns the singleton instance
--            Is_A_Traffic_Light (Object, Agent_Id):
--              true iff the provided belongs to a traffic light
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Traffic_Light.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Traffic_Light.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Traffic_Light.Utils.Reference;

   not overriding
   function Is_A_Traffic_Light (
      This      : in out Traffic_Light.Utils.Object;
      Active_Id : in     Agent.Agent_Id)
   return Boolean;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Traffic_Light.Utils.Reference := null;

end Active.Traffic_Light.Utils;

