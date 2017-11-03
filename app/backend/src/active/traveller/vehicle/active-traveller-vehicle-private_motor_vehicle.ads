------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-vehicle-private_motor_vehicle
-- @purpose Represents a private motor vehicle
-- @interface Travel (Object) -> Boolean:
--              attempts an overtake if certain conditions are met
-- @dependencies application-backend::active-agent
--               application-backend::reactive-infrastructure-lane-utils
-- @details Implementation of Vehicle. Most of the methods are not listed in
--          the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Agent;

with Reactive.Infrastructure.Lane.Utils;

package Active.Traveller.Vehicle.Private_Motor_Vehicle is

   package Agent   renames Active.Agent;
   package Vehicle renames Active.Traveller.Vehicle;
   package Lane    renames Reactive.Infrastructure.Lane;

   type Private_Motor_Vehicle_Type is
      (CAR, MOTORCYCLE, SIDECAR);

   type Object is new Vehicle.Object with private;
   type Reference is access all Private_Motor_Vehicle.Object'Class;

   function Create (
      Id             : in Agent.Agent_Id;
      Maximum_Speed  : in Natural;
      Max_Passengers : in Natural;
      Vehicle_Type   : in Private_Motor_Vehicle_Type;
      Travel_Ref     : access Active.Travel.Object'Class;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Lane_Utils           : access Lane.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null)
   return Private_Motor_Vehicle.Reference;

   overriding
   procedure Travel (This : in out Private_Motor_Vehicle.Object);

   overriding
   function Is_Affected_By_Traffic_Lights (
      This : in Private_Motor_Vehicle.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Private_Motor_Vehicle.Object) return Natural;

   overriding
   function Dump (This : in Private_Motor_Vehicle.Object)
   return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function PVT_Type_Field return String is ("pvtType");

private
   type Object is new Vehicle.Object with record
      Vehicle_Type          : Private_Motor_Vehicle_Type;
      Will_Attempt_Overtake : Boolean := False;
      Lane_Utils            : access Lane.Utils.Object'Class := null;
   end record;

end Active.Traveller.Vehicle.Private_Motor_Vehicle;
