------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-bus_travel
-- @purpose Extends a generic travel, adding specific behaviour for buses
-- @interface Advance (Object):
--              makes the travel advance, but handles the planning state in an
--              ad-hoc manner, by composing the stops of a bus and thus
--              obtaining the whole travel of a bus with multiple AI
--              invocations
--            Consume_Step (Object):
--              places the first step of the travel at the end of the steps'
--              list
-- @dependencies application-backend::active-space_master
--               application-backend::active-traveller-vehicle-bus-utils
-- @details this class is not thread safe (as Travel), because we expect only
--          one thread to access the travel field of a given traveller at a
--          time. The API is incomplete for the sake of clarity (most of the
--          methods are self-explanatory)
------------------------------------------------------------------------------

with Active.Space_Master;
with Active.Traveller.Vehicle.Bus.Utils;

package Active.Travel.Bus_Travel is

   package Space_Master_Pkg renames Active.Space_Master;
   package Bus_Utils_Pkg    renames Active.Traveller.Vehicle.Bus.Utils;

   type Object is new Travel.Object with private;
   type Reference is access all Bus_Travel.Object'Class;

   function Create (
      Route_Source      : in Slice.Map;
      Route_Destination : in Slice.Map;
      Traveller_Id      : in Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class
        := null;
      Bus_Utils         : access Bus_Utils_Pkg.Object'Class
        := null;
      Space_Master      : access Space_Master_Pkg.Object'Class := null)
    return Bus_Travel.Reference;

   overriding
   procedure Advance (This : in out Bus_Travel.Object);

   overriding
   function Get_Current_Step_Id (This : Bus_Travel.Object) return Infra_Id;

   overriding
   function Get_Previous_Step_Id (This : Bus_Travel.Object) return Infra_Id;

   overriding
   function Get_Next_Step_Id (This : Bus_Travel.Object) return Infra_Id;

   overriding
   function Has_Next_Step (This : in Bus_Travel.Object) return Boolean;

   overriding
   procedure Consume_Step (This : in out Bus_Travel.Object);

private
   type Object is new Travel.Object with record
      Bus_Utils    : access Bus_Utils_Pkg.Object'Class := null;
      Space_Master : access Space_Master_Pkg.Object'Class := null;
   end record;

end Active.Travel.Bus_Travel;
