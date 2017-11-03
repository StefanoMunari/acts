with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

with Active.Agent;
with Active.Traveller;
with Active.Traveller.Utils;

with Reactive.Intersectable;
with Reactive.Infrastructure.Street_Related_Infrastructure;
limited with Reactive.Infrastructure.Stretch.Utils;
limited with Reactive.Infrastructure.Way.Utils;

with Shared.Agent_Id_To_Infra_Id_Map;
with Shared.Agent_Id_List;
with Shared.Containable;
with Shared.Direction;
with Shared.Infra_Id_List;
with Shared.Infra_Id_Set;
with Shared.Natural_List;

package Reactive.Infrastructure.Lane is

   package Agent renames Active.Agent;
   package Traveller renames Active.Traveller;
   package Containable renames Shared.Containable;
   package Direction renames Shared.Direction;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Agent_Id_To_Infra_Id_Map renames Shared.Agent_Id_To_Infra_Id_Map;
   package Infra_Id_Set renames Shared.Infra_Id_Set;
   package Natural_List renames Shared.Natural_List;
   pragma Suppress (Elaboration_Check);

   type Object is
     abstract new Ada.Finalization.Controlled
     and Street_Related_Infrastructure.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object
   with private;
   type Reference is access all Lane.Object'Class;

   not overriding
   procedure Enter (
      This         : in out Lane.Object;
      Traveller_Id : in Agent.Agent_Id);

   overriding
   function "=" (This, Other : in Lane.Object) return Boolean;

   overriding
   function Find_Street (This : in Lane.Object)
   return Infra_Id;

   overriding
   function Get_Id (This : in Lane.Object) return Infra_Id;

   overriding
   function Find_Intersections (This : in Lane.Object)
   return Infra_Id_Set.Set;

   not overriding
   procedure Append_Stretch (
      This       : in out Lane.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean);

   not overriding
   function Count_Stretches (This : in Lane.Object) return Natural;

   not overriding
   procedure Find_Stretch_Position (
      This             : in     Lane.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean);

   not overriding
   function Get_Stretch_By_Position (
      This             : in Lane.Object;
      Stretch_Position : in Natural)
   return Infra_Id;

   not overriding
   function Get_Direction (This : Lane.Object)
   return Direction.Straight;

   overriding
   function Is_Contained_By (This         : in Lane.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   not overriding
   procedure Set_Way (This : in out Lane.Object; Way_Id : in Infra_Id);

   not overriding
   procedure Add_Intersection (
      This            : in out Lane.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

   not overriding
   procedure Attempt_Overtake (
      This                : in Lane.Object;
      Stretch_Id          : in Infra_Id;
      Max_Overtake_Length : in Natural;
      Traveller_Id        : in Agent.Agent_Id);

   overriding
   function Dump (This : Lane.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Id_Field          return String is ("id");
   function Direction_Field   return String is ("direction");
   function Stretches_Field   return String is ("stretches");
   function Decorations_Field return String is ("decorations");

private
   protected type Protected_Stretches is

      not overriding
      function Count_Stretches return Natural;

      not overriding
      function Contains_Stretch (Stretch_Id : Infra_Id) return Boolean;

      not overriding
      procedure Find_Stretch_Position (
         Stretch_Id       : in     Infra_Id;
         Stretch_Position :    out Natural;
         Found            :    out Boolean);

      not overriding
      procedure Append_Stretch (
         Stretch_Id : in     Infra_Id;
         Added      :    out Boolean);

      not overriding
      function Get_First_Stretch return Infra_Id;

      not overriding
      function Has_Next_Stretch (Stretch_Id : in Infra_Id) return Boolean;

      not overriding
      procedure Get_Next_Stretch (Stretch_Id      : in     Infra_Id;
                                  Next_Stretch_Id :    out Infra_Id;
                                  Found           :    out Boolean);

   private
      Stretches : Infra_Id_List.List := Infra_Id_List.Empty_List;
   end Protected_Stretches;

   type Object is
     abstract new Ada.Finalization.Controlled
     and Street_Related_Infrastructure.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object with record
      Id                     : Infra_Id;
      Direction              : Shared.Direction.Straight;
      Way_Id                 : Infra_Id;
      Intersection_Id        : Infra_Id;
      Intersection_Existence : Boolean := FALSE;
      Protected_Stretches    : access Lane.Protected_Stretches;
      Stretch_Utils          : access Stretch.Utils.Object'Class;
      Way_Utils              : access Way.Utils.Object'Class;
      Traveller_Utils        : access Traveller.Utils.Object'Class;
   end record;

   procedure Init (
      Lane      : in out Infrastructure.Lane.Object'Class;
      Id        : in Infra_Id;
      Direction : in Shared.Direction.Straight;
      Stretch_Utils : access Stretch.Utils.Object'Class := null;
      Way_Utils : access Way.Utils.Object'Class := null;
      Traveller_Utils : access Traveller.Utils.Object'Class := null);

   overriding
   procedure Initialize (This : in out Lane.Object);

   overriding
   procedure Adjust (This : in out Lane.Object);

   overriding
   procedure Finalize (This : in out Lane.Object);

end Reactive.Infrastructure.Lane;
