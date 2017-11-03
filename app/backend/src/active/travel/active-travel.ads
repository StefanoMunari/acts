------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel
-- @purpose Represents a travel of a given traveller
-- @interface Advance (Object):
--              makes the travel advance
--            Consume_Step (Object):
--              consumes the next step of the travel
--            Modify_Travel_Beginning (Object, List):
--              replaces the first k steps of the travel with the provided list
-- @dependencies application-backend::ai
--               application-backend::active-agent
--               application-backend::active-travel-travel_state
--               application-backend::active-traveller-utils
--               application-backend::reactive
--               application-backend::shared-infra_id_list
--               application-backend::shared-infra_id_list_utils
--               application-backend::shared-slice
-- @details this class is not thread safe, because we expect only one thread to
--          access the travel field of a given traveller at a time. The API is
--          incomplete for the sake of clarity (most of the methods are
--          self-explanatory)
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Finalization;

with AI;
with Active.Agent;
limited with Active.Travel.Travel_State;
limited with Active.Traveller.Utils;

with Reactive;

with Shared.Infra_Id_List;
with Shared.Infra_Id_List_Utils;
with Shared.Slice;

package Active.Travel is
   use Shared.Infra_Id_List_Utils; -- make Infra_Id_List_Ref visible

   package STP renames Reactive.Stretch_Type_Package;
   package SU            renames Ada.Strings.Unbounded;
   package Agent         renames Active.Agent;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Slice         renames Shared.Slice;
   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Travel.Object'Class;

   function Create (
      Route_Source      : in Slice.Map;
      Route_Destination : in Slice.Map;
      Traveller_Id      : in Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class
        := null)
    return Travel.Reference;

   not overriding
   procedure Advance (This : in out Travel.Object);

   not overriding
   function Has_Next_Step (This : in Travel.Object) return Boolean;

   not overriding
   procedure Consume_Step (This : in out Travel.Object);

   not overriding
   function Is_Progressing (This : in Travel.Object) return Boolean;

   not overriding
   function Get_Route_Source (This : in Travel.Object) return Slice.Map;

   not overriding
   function Get_Route_Destination (This : in Travel.Object) return Slice.Map;

   not overriding
   procedure Reverse_Source_And_Destination (This : in out Travel.Object);

   not overriding
   function Get_Traveller_Id (This : in Travel.Object) return Agent.Agent_Id;

   not overriding
   function Get_Current_Step_Id (This : Travel.Object) return Infra_Id;

   not overriding
   function Get_Next_Step_Id (This : Travel.Object) return Infra_Id;

   not overriding
   function Get_Previous_Step_Id (This : Travel.Object) return Infra_Id;

   not overriding
   procedure Set_Residual_Route (
      This           : in out Travel.Object;
      Residual_Route : in     Infra_Id_List.List);

   not overriding
   function Get_Residual_Route (This : Travel.Object)
   return Infra_Id_List.List;

   not overriding
   function Contains (This :    Travel.Object;
                      Step : in Infra_Id)
   return Boolean;

   not overriding
   function Contains (This        :    Travel.Object;
                      Steps_Slice : in Slice.Map)
   return Boolean;

   not overriding
   procedure Modify_Beginning (
      This          : in out Travel.Object;
      New_Beginning : in     Infra_Id_List.List);

   not overriding
   function Get_AI (This : in Travel.Object)
   return access AI.Object'Class;

   procedure Set_AI (
      AI_Interface_Ref : access AI.Object'Class);

   procedure Add_AI (This : in Travel.Object;
      Stretch : in STP.Stretch_Type);

   procedure Remove_AI;

   not overriding
   function Dump_State (This : Travel.Object) return SU.Unbounded_String;

   not overriding
   procedure Init_Route (This : in out Travel.Object; Route : Infra_Id_List.List);

private

   type Object is new Ada.Finalization.Controlled with record
      Route_Source      : Slice.Map;
      Route_Destination : Slice.Map;
      Traveller_Id      : Agent.Agent_Id;
      Route             : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Previous_Step     : Infra_Id;
      Travel_State      : access Travel.Travel_State.Object'Class;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class := null;
   end record;

   AI_Interface : access AI.Object'Class := null;

   not overriding
   procedure Change_Travel_State (
      This         : in out Travel.Object;
      Travel_State : access Travel.Travel_State.Object'Class);

   not overriding
   procedure Clear_Route (
      This       : in out Travel.Object;
      Clean      :    out Boolean);

   not overriding
   procedure Prepend_Step (
      This    : in out Travel.Object;
      Step_Id : in     Infra_Id);

   not overriding
   function Get_First_Step_Id (This : Travel.Object)
   return Infra_Id;

   not overriding
   function Get_Last_Step_Id (This : Travel.Object)
   return Infra_Id;

   not overriding
   function Get_Route (This : in Travel.Object) return Infra_Id_List.List;

   not overriding
   procedure Init (
      This              : in out Travel.Reference;
      Route_Source      : in     Slice.Map;
      Route_Destination : in     Slice.Map;
      Traveller_Id      : in     Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class
        := null);

   overriding
   procedure Finalize (This : in out Travel.Object);

end Active.Travel;
