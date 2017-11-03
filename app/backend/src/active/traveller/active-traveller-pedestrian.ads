------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-pedestrian
-- @purpose Represents a pedestrian
-- @interface Get_Stretch_Type (Object) -> Stretch_Type:
--              returns FOOT
--            Travel (Object):
--              check if is waiting for a bus before going on with the travel
--            Is_Affected_By_Traffic_Lights (Object) -> Boolean:
--              returns True
--            On_Bus_Stop (Object) -> Boolean:
--              decides if waiting for a bus or not
-- @dependencies application-backend::active-agent
--               application-backend::active-bus_service
--               application-backend::active-person
--               application-backend::reactive-infrastructure-utils
--               application-backend::reactive-street-utils
--               application-backend::reactive-stretch-decoration-stretch_sign_decorator-utils
-- @details Implementation of a traveller. Some methods are omitted in
--          @interface because being self-explanatory
------------------------------------------------------------------------------

with Active.Agent;
with Active.Bus_Service;
with Active.Person;
with Active.Traveller.Strategy;

with Reactive.Infrastructure.Utils;
with Reactive.Infrastructure.Street.Utils;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils;

package Active.Traveller.Pedestrian is

   package Agent    renames Active.Agent;
   package Strategy renames Active.Traveller.Strategy;
   package Infrastructure renames Reactive.Infrastructure.Utils;
   package Street         renames Reactive.Infrastructure.Street;
   package Stretch_Sign_Decorator renames
      Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;

   type Object is
     new Traveller.Object
     and Person.Object
     and Bus_Service.Object
   with private;
   type Reference is access all Pedestrian.Object'Class;

   function Create (
      Id            : in     Agent.Agent_Id;
      Maximum_Speed : in     Natural;
      Travel_Ref    : access Active.Travel.Object'Class;
      Is_Waiting    : in     Boolean := False;
      Infrastructure_Utils : access Infrastructure.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Strategy_Ref         : access Strategy.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null;
      SSD_Utils            : -- Stretch_Sign_Decorator
         access Stretch_Sign_Decorator.Utils.Object'Class := null)
   return Pedestrian.Reference;

   overriding
   function Get_Stretch_Type (This : in Pedestrian.Object)
   return Stretch_Type;

   overriding
   procedure Travel (This : in out Pedestrian.Object);

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Pedestrian.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Pedestrian.Object) return Natural;

   overriding
   procedure On_Bus_Stop (This : in out Pedestrian.Object);

   not overriding
   procedure Stop_Waiting (This : in out Pedestrian.Object);

   not overriding
   procedure Recompute_Travel (This : Traveller.Pedestrian.Object);

   overriding
   function Dump (This : in Pedestrian.Object)
   return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Is_Waiting_Field return String is ("isWaiting");

private
   type Object is
     new Traveller.Object
     and Person.Object
     and Bus_Service.Object
   with record
      Is_Waiting   : Boolean;
      Strategy_Ref : access Strategy.Object'Class;
      Street_Utils : access Street.Utils.Object'Class;
      SSD_Utils    :
         access Stretch_Sign_Decorator.Utils.Object'Class := null;
   end record;

end Active.Traveller.Pedestrian;
