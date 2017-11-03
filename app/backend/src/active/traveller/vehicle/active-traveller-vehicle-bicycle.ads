------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-vehicle-bicycle
-- @purpose Represents a bicycle
-- @interface Get_Stretch_Type (Object) -> Boolean:
--              returns BIKE
--            Is_Affected_By_Traffic_Lights (Object) -> Boolean:
--              returns False
-- @dependencies application-backend::active-agent
--               application-backend::active-traveller-vehicle
-- @details Implementation of Vehicle. Most of the methods are not listed in
--          the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Agent;
with Active.Traveller.Vehicle;

package Active.Traveller.Vehicle.Bicycle is

   package Agent renames Active.Agent;
   package Vehicle renames Active.Traveller.Vehicle;

   type Object is new Vehicle.Object with null record;
   type Reference is access all Bicycle.Object'Class;

   not overriding
   function Create (
      Id             : in     Agent.Agent_Id;
      Maximum_Speed  : in     Natural;
      Max_Passengers : in     Natural;
      Travel_Ref     : access Active.Travel.Object'Class;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null)
   return Bicycle.Reference;

   overriding
   function Get_Stretch_Type (This : in Bicycle.Object)
   return Stretch_Type;

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Bicycle.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Bicycle.Object) return Natural;

   overriding
   function Dump (This : in Bicycle.Object)
   return G_JSON.JSON_Value;

end Active.Traveller.Vehicle.Bicycle;
