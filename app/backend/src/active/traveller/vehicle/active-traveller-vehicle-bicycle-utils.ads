------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-vehicle-bicycle-utils
-- @purpose Helper package which provides methods to access bicycles
-- @interface Get_Instance (Object):
--              returns the singleton instance
--            Is_A_Bicycle (Object, Agent_Id):
--              true iff the provided belongs to a bicycle
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Traveller.Vehicle.Bicycle.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Bicycle.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
    return Bicycle.Utils.Reference;

   not overriding
   function Is_A_Bicycle (
      This         : in out Bicycle.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Bicycle.Utils.Reference := null;

end Active.Traveller.Vehicle.Bicycle.Utils;
