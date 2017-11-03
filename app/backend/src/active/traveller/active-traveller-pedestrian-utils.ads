------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-pedestrian-utils
-- @purpose Helper package which provides methods to access pedestrians
-- @interface Get_Instance (Object, Agent_id):
--              returns the singleton instance
--            Is_A_Pedestrian (Object, Agent_Id) -> Boolean:
--              true iff the provided belongs to a pedestrian
--            Stop_Waiting (Object, Agent_Id):
--              makes a pedestrian stop waiting for a bus
--            Recompute_Travel (Object, Agent_Id):
--              makes a pedestrian recompute the route starting from the
--              current position
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Traveller.Pedestrian.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Pedestrian.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Pedestrian.Utils.Reference;

   not overriding
   function Is_A_Pedestrian (
      This         : in Pedestrian.Utils.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean;

   not overriding
   procedure Stop_Waiting (
      This         : in out Pedestrian.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id);

   not overriding
   procedure Recompute_Travel (
      This          : Pedestrian.Utils.Object;
      Traveller_Id  : Agent.Agent_Id);

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Pedestrian.Utils.Reference := null;

end Active.Traveller.Pedestrian.Utils;
