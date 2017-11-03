with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

with Active.Agent;
with Active.Traveller.Utils;

with Reactive.Infrastructure.Utils;
with Reactive.Intersectable;
limited with Reactive.Infrastructure.Intersection.Crossing;
with Reactive.Treadable;

with Shared.Containable;
with Shared.Direction;
with Shared.Infra_Id_List;
with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Intersection is

   package Agent         renames Active.Agent;
   package Infra_Id_Set  renames Shared.Infra_Id_Set;
   package Containable   renames Shared.Containable;
   package Direction     renames Shared.Direction;
   package Infra_Id_List renames Shared.Infra_Id_List;
   use Reactive.Infra_Id_Type;

   type Object (<>) is
     new Ada.Finalization.Controlled
     and Treadable.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object
   with private;
   type Reference is access all Intersection.Object'Class;

   type Intersection_Type is (T_JUNCTION,
                              CROSSROADS);

   overriding
   procedure Tread (
      This         : in out Intersection.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean);

   overriding
   procedure Leave (
      This         : in out Intersection.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   not overriding
   function Get_Street (This      : in     Intersection.Object;
                        Direction : in out Shared.Direction.Cardinal)
   return Infra_Id;

   not overriding
   procedure Find_Street_Direction (
      This             :        Intersection.Object;
      Street_Id        : in     Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean);

   not overriding
   function Find_Streets_Connected_With_Intersection (
      This : in Intersection.Object) return Infra_Id_Set.Set;

   overriding
   function Get_Id (This : in Intersection.Object) return Infra_Id;

   function Get_Intersection_Type (This : in Intersection.Object)
                                   return Intersection_Type;

   overriding
   function Find_Intersections (This : in Intersection.Object)
                                return Infra_Id_Set.Set;

   not overriding
   function Count_Streets (This : in Intersection.Object) return Natural;

   not overriding
   function Exists_Street_For_Direction (
      This      : in Intersection.Object;
      Direction : in Shared.Direction.Cardinal)
   return Boolean;

   overriding
   function "="(This, Other : Intersection.Object) return Boolean;

   overriding
   procedure Initialize (This : in out Intersection.Object);

   overriding
   function Is_Contained_By (This         : in Intersection.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   function Dump (This : in Intersection.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Id_Field            return String is ("id");
   function Exits_Field         return String is ("exits");
   function Street_Id_Field     return String is ("streetId");
   function Stretch_Id_Field    return String is ("stretchId");
   function Direction_Field     return String is ("direction");
   function Traffic_Light_Field return String is ("trafficLightId");
   function Traveller_Field     return String is ("traveller");

private

   type Street_Map is array (Direction.Cardinal) of Infra_Id;

   type Stretch_Map is array (Direction.Cardinal) of Infra_Id_List.List;

   type Street_Existence_Map is array (Direction.Cardinal) of Boolean;

   type Stretch_Existence_Map is array (Direction.Cardinal) of Boolean;

   type Boolean_Map is array (Direction.Cardinal) of Boolean;

   type Travellers_Map is array (Direction.Cardinal) of Agent.Agent_Id;

   protected type Protected_Entries
   is

      procedure Try_To_Enter (
         Traveller_Id    : in     Agent.Agent_Id;
         Source_Entry    : in     Direction.Cardinal;
         Entered         :    out Boolean;
         Already_In      :    out Boolean);

      procedure Leave (Traveller_Id : in Agent.Agent_Id);

      function Number_Of_Travellers return Integer;

      function Has_Traveller_Id (Source_Entry : in Direction.Cardinal)
        return Boolean;

      function Get_Traveller_Id (Source_Entry : in Direction.Cardinal)
        return Agent.Agent_Id;

   private
      Entries    : Boolean_Map;
      Travellers : Travellers_Map;
   end Protected_Entries;

   type Object is
     new Ada.Finalization.Controlled
     and Treadable.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object with record
      Id                   : Infra_Id;
      Intersection_Type    : Intersection.Intersection_Type;
      Size                 : Natural;
      Streets_Count        : Natural := 0;
      Streets              : Street_Map;
      Streets_Existence    : Street_Existence_Map;
      Stretches            : Stretch_Map;
      Stretches_Existence  : Stretch_Existence_Map;
      Entries              : access Protected_Entries;
      Traveller_Utils      : access Active.Traveller.Utils.Object'Class;
      Infrastructure_Utils : access Reactive.Infrastructure.Utils.Object'Class;
      Crossing_Strategy    : access Intersection.Crossing.Object'Class;
   end record;

   not overriding
   procedure Update_Size (
      This : in out Intersection.Object;
      Intersection_Type : in Intersection.Intersection_Type);

   not overriding
   procedure Set_Id (This : in out Intersection.Object;
                     Id   :        Infra_Id);

   not overriding
   procedure Set_Intersection_Type (
      This              : in out Intersection.Object;
      Intersection_Type : in Intersection.Intersection_Type);

   not overriding
   procedure Set_Traveller_Utils (
      This            : in out Intersection.Object;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class);

   not overriding
   procedure Set_Infrastructure_Utils (
      This                 : in out Intersection.Object;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class);

   not overriding
   procedure Set_Crossing_Strategy (
      This              : in out Intersection.Object;
      Crossing_Strategy : access Intersection.Crossing.Object'Class);

   not overriding
   procedure Increment_Streets (This : in out Intersection.Object);

   not overriding
   procedure Connect_Street (This      : in out Intersection.Object;
                             Street_Id : in     Infra_Id;
                             Stretches : in     Infra_Id_List.List;
                             Direction : in     Shared.Direction.Cardinal);

   not overriding
   function Get_Size (This : in Intersection.Object) return Natural;

   not overriding
   function Is_Fully_Connected (This : in Intersection.Object) return Boolean;

   not overriding
   function Is_Not_Fully_Connected (This : in Intersection.Object)
   return Boolean;

end Reactive.Infrastructure.Intersection;
