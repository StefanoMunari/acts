------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::passive-road_sign-speed_limit
-- @purpose Represents a speed limit signal
-- @interface Apply (Object)
--              @inherit
--            Get_Limit (Object) -> Natural
--              returns the speed limit
-- @dependencies application-backend::active-traveller-utils
-- @details Implementation of Road_Sign. Most of the methods are not listed in
--          the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Traveller.Utils;

package Passive.Road_Sign.Speed_Limit is

   package Agent renames Active.Agent;
   package Traveller renames Active.Traveller;
   package Traveller_Utils renames Traveller.Utils;

   type Object is
     new Road_Sign.Object
   with private;
   type Reference is access all Speed_Limit.Object'Class;

   not overriding
   function Create (
      Limit           : in Natural;
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
   return Passive.Road_Sign.Speed_Limit.Reference;

   overriding
   procedure Apply (This       : in out Speed_Limit.Object;
                    Traveller  : in     Agent.Agent_Id);

   not overriding
   function Get_Limit (This : in Speed_Limit.Object) return Natural;

   overriding
   function Dump (This : Speed_Limit.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Speed_Limit_Field return String is ("speedLimit");

private

   type Object is
     new Road_Sign.Object
   with record
     Limit           : Natural;
     Traveller_Utils : Traveller.Utils.Reference;
   end record;

end Passive.Road_Sign.Speed_Limit;
