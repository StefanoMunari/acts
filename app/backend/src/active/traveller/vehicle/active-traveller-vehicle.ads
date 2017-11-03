------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-vehicle
-- @purpose Represents a vehicle
-- @interface Board (Object, Agent_Id) -> Boolean:
--              makes a traveller board the vehicle
--            Free (Object, Agent_Id) -> Boolean:
--              lands a traveller
--            Get_Passengers (Object) -> List:
--              returns the list of present passengers
-- @dependencies application-backend::active-agent
--               application-backend::active-people_carrier
--               application-backend::active-traveller
--               application-backend::active-traveller-utils
--               application-backend::reactive-infrastructure-building-host-utils
--               application-backend::reactive-street_related_infrastructure
--               application-backend::reactive-street-utils
--               application-backend::reactive-stretch-utils
--               application-backend::reactive-utils
--               application-backend::shared-agent_id_list
--               application-backend::shared-slice
-- @details Abstract class, subclass of Traveller. Most of the methods are not
--          listed in the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Agent;
with Active.People_Carrier;
with Active.Traveller;
with Active.Traveller.Utils;

with Reactive.Infrastructure.Building.Host.Utils;
with Reactive.Infrastructure.Street_Related_Infrastructure;
with Reactive.Infrastructure.Street.Utils;
with Reactive.Infrastructure.Stretch.Utils;
with Reactive.Infrastructure.Utils;

with Shared.Agent_Id_List;

package Active.Traveller.Vehicle is

   -- active
   package Agent renames Active.Agent;
   -- reactive
   package Infrastructure renames Reactive.Infrastructure;
   package Host           renames Reactive.Infrastructure.Building.Host;
   package Street         renames Reactive.Infrastructure.Street;
   package Stretch        renames Reactive.Infrastructure.Stretch;
   -- shared
   package Agent_Id_List renames Shared.Agent_Id_List;

   type Object is
     abstract new Traveller.Object
     and People_Carrier.Object with private;
   type Reference is access all Vehicle.Object'Class;

   overriding
   function Get_Stretch_Type (This : in Vehicle.Object)
   return Stretch_Type;

   overriding
   procedure Travel (This : in out Vehicle.Object);

   overriding
   procedure Board (This    : in out Vehicle.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean);

   overriding
   procedure Free (This      : in out Vehicle.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean);

   overriding
   function Get_Passengers (This : in Vehicle.Object)
   return Agent_Id_List.List;

   overriding
   function Get_Max_Number_Of_Passengers (This : in Vehicle.Object)
   return Natural;

   overriding
   function Count_Passengers (This : in Vehicle.Object) return Natural;

   overriding
   function Dump (This : in Vehicle.Object)
   return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Passengers_Field     return String is ("passengers");
   function Max_Passengers_Field return String is ("maxPassengers");

private
   type Object is
     abstract new Traveller.Object
     and People_Carrier.Object with
      record
         Max_Passengers  : Natural;
         Passengers      : Agent_Id_List.List;
         Host_Utils      : access Host.Utils.Object'Class;
         Street_Utils    : access Street.Utils.Object'Class;
         Stretch_Utils   : access Stretch.Utils.Object'Class;
         Traveller_Utils : access Traveller.Utils.Object'Class;
      end record;

   procedure Init (
      Vehicle        : in out Traveller.Vehicle.Object'Class;
      Id             : in     Agent.Agent_Id;
      Maximum_Speed  : in     Natural;
      Max_Passengers : in     Natural;
      Travel_Ref     :
         access Active.Travel.Object'Class;
      Infrastructure_Utils :
         access Infrastructure.Utils.Object'Class := null;
      Host_Utils           :
         access Host.Utils.Object'Class := null;
      Street_Utils         :
         access Street.Utils.Object'Class := null;
      Stretch_Utils        :
         access Stretch.Utils.Object'Class := null;
      Intersection_Utils   :
         access Intersection.Utils.Object'Class := null;
      Traveller_Utils      :
         access Traveller.Utils.Object'Class := null);
end Active.Traveller.Vehicle;
