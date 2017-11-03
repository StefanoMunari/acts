------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller
-- @purpose Represents a traveller
-- @interface Get_Stretch_Type (Object) -> Stretch_Type:
--              returns the type of stretches on which the traveller moves
--            Act (Object):
--              makes the travel advance
--            Is_Affected_By_Traffic_Lights (Object) -> Boolean:
--              tells if the traveller has to respect traffic lights
--              indications (e.g., pedestrians ignore traffic lights in our
--              system)
-- @dependencies application-backend::active-agent
--               application-backend::active-travel
--               application-backend::active-traveller-extractor
--               application-backend::reactive
--               application-backend::reactive-infrastructure-utils
--               application-backend::reactive-intersection-utils
--               application-backend::shared-direction
--               application-backend::shared-dumpable
--               application-backend::shared-infra_id_list
--               application-backend::shared-slice
-- @details Abstract class. Most of the methods are not listed in the
--          @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Ada.Finalization;

-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;
with Active.Travel;
limited with Active.Traveller.Extractor;

with Reactive;
with Reactive.Infrastructure.Utils;
limited with Reactive.Infrastructure.Intersection.Utils;

with Shared.Direction;
with Shared.Dumpable;
with Shared.Infra_Id_List;
with Shared.Slice;

package Active.Traveller is

   package G_JSON        renames GNATCOLL.JSON;
   package Agent         renames Active.Agent;
   package Direction     renames Shared.Direction;
   package Dumpable      renames Shared.Dumpable;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Slice         renames Shared.Slice;

   use Reactive.Infra_Id_Type;
   use Reactive.Stretch_Type_Package;

   type Object is
     abstract new Ada.Finalization.Controlled
     and Agent.Object
     and Dumpable.Object
   with private;
   type Reference is access all Traveller.Object'Class;

   overriding
   function Get_Id (This : in Traveller.Object)
   return Agent.Agent_Id;

   not overriding
   function Get_Stretch_Type (This : in Traveller.Object)
   return Stretch_Type is abstract;

   not overriding
   function Get_Position (This : in Traveller.Object)
   return Infra_Id;

   not overriding
   procedure Set_Position (This         :    out Traveller.Object;
                           New_Position : in     Infra_Id);

   not overriding
   function Get_Maximum_Speed (This : in Traveller.Object)
   return Natural;

   not overriding
   function Get_Current_Speed (This : in Traveller.Object)
   return Natural;

   not overriding
   procedure Set_Current_Speed (This      : in out Traveller.Object;
                                New_Speed : in     Natural);

   not overriding
   function Get_Scheduled_For (This : in Traveller.Object)
   return Float;

   not overriding
   procedure Set_Scheduled_For (This              : out Traveller.Object;
                                New_Scheduled_For : in Float);

   not overriding
   procedure Set_Travel (This       : in out Traveller.Object;
                         Travel_Ref : access Active.Travel.Object'Class);

   not overriding
   function Get_Travel_Source (This : in Traveller.Object)
   return Slice.Map;

   not overriding
   function Get_Travel_Destination (This : in Traveller.Object)
   return Slice.Map;

   not overriding
   function Look_Ahead_Step (
      This  : in Traveller.Object;
      Index : in Natural)
   return Infra_Id;

   function Does_Travel_Contain_Step (This : in Traveller.Object;
                                      Step : in Infra_Id)
   return Boolean;

   not overriding
   function Does_Travel_Contain_Steps (This : in Traveller.Object;
                                       Step : in Slice.Map)
   return Boolean;

   not overriding
   procedure Modify_Travel_Beginning (
      This          : Traveller.Object;
      New_Beginning : Infra_Id_List.List);

   not overriding
   procedure Erase_Route (This : in out Traveller.Object);

   not overriding
   function Get_List_From_Slice (This         : in Traveller.Object;
                                 The_Slice    : in     Slice.Map)
   return Infra_Id_List.List;

   overriding
   procedure Act (This : in out Traveller.Object);

   not overriding
   procedure Travel (This : in out Traveller.Object);

   not overriding
   function Has_Next_Step (This : in Traveller.Object)
   return Boolean;

   not overriding
   procedure Consume_Step (This : in Traveller.Object);

   not overriding
   function Is_Travelling (This : in Traveller.Object)
   return Boolean;

   not overriding
   function Is_Affected_By_Traffic_Lights (This : in Traveller.Object)
   return Boolean is abstract;

   not overriding
   function Get_Size (This : in Traveller.Object)
   return Natural is abstract;

   overriding
   function "=" (This, Other : Traveller.Object)
   return Boolean;

   overriding
   function Dump (This : in Traveller.Object)
   return G_JSON.JSON_Value;

   -- overriding
   -- procedure Initialize (This : in out Traveller.Object);
   -- overriding
   -- procedure Adjust     (This : in out Traveller.Object);
   -- overriding
   -- procedure Finalize   (This : in out Traveller.Object);

-- JSON FIELDS CONSTANTS
   function Id_Field               return String is ("id");
   function Type_Field             return String is ("type");
   function Current_Speed_Field    return String is ("curSpeed");
   function Maximum_Speed_Field    return String is ("maxSpeed");
   function Current_Position_Field return String is ("curPosition");
   function Source_Field           return String is ("src");
   function Destination_Field      return String is ("dst");
   function Residual_Route_Field   return String is ("residualTravel");
   function Travel_State_Field     return String is ("travelState");

private
   type Object is
     abstract new Ada.Finalization.Controlled
     and Agent.Object
     and Dumpable.Object
   with record
      Id               : Agent.Agent_Id;
      Maximum_Speed    : Natural;
      Current_Speed    : Natural;
      Scheduled_For    : Float;
      Current_Position : Infra_Id;
      Travel_Ref       : access Active.Travel.Object'Class;
      Infrastructure_Utils : access Reactive.Infrastructure.Utils.Object'Class;
      Intersection_Utils   :
        access Reactive.Infrastructure.Intersection.Utils.Object'Class;
   end record;

   procedure Init (
      Traveller     : in out Active.Traveller.Object'Class;
      Id            : in Agent.Agent_Id;
      Maximum_Speed : in Natural;
      Travel_Ref    : access Active.Travel.Object'Class;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Intersection_Utils :
         access Reactive.Infrastructure.Intersection.Utils.Object'Class := null
   );
end Active.Traveller;
