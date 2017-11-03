with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

with Active.Agent;
with Active.Traveller;
with Active.Traveller.Utils;

with Reactive.Infrastructure.Street_Related_Infrastructure;
with Reactive.Infrastructure.Lane.Utils;
with Reactive.Intersectable;
with Reactive.Treadable;

with Shared.Agent_Id_List;
with Shared.Containable;
with Shared.Direction;
with Shared.Natural_List;
with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Stretch is

   package Agent renames Active.Agent;
   package Containable renames Shared.Containable;
   package Direction renames Shared.Direction;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Natural_List renames Shared.Natural_List;
   package Infra_Id_Set renames Shared.Infra_Id_Set;

   use Reactive.Stretch_Type_Package;

   type Object is
     abstract new Ada.Finalization.Controlled
     and Treadable.Object
     and Street_Related_Infrastructure.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object
   with private;
   type Reference is access all Stretch.Object'Class;

   overriding
   procedure Tread (This         : in out Stretch.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   overriding
   function Find_Street (This : in Stretch.Object)
   return Infra_Id;

   overriding
   function Get_Id (This : in Stretch.Object) return Infra_Id;

   not overriding
   function Find_Lane (This : in Stretch.Object) return Infra_Id;

   not overriding
   function Calculate_Position (This : in Stretch.Object)
   return Natural;

   not overriding
   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean;

   overriding
   procedure Leave (
      This         : in out Stretch.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   not overriding
   function Is_Before (This, Other: in Stretch.Object) return Boolean;

   procedure Set_Lane (
      This    : in out Stretch.Object;
      Lane_Id : in     Infra_Id);

   not overriding
   procedure Set_Host (
      This    : in out Stretch.Object;
      Host_Id : in     Infra_Id);

   not overriding
   function Get_Host (This : in Stretch.Object)
   return Infra_Id;

   not overriding
   function Has_Host (This : in Stretch.Object)
   return Boolean;

   overriding
   function Find_Intersections (This : in Stretch.Object)
                                return Infra_Id_Set.Set;

   not overriding
   function Get_Size (This : in Stretch.Object) return Natural;

   overriding
   function Is_Contained_By (This         : in Stretch.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Is_Empty (This : in Stretch.Object) return Boolean;

   not overriding
   procedure Put_Traveller (This         : in Stretch.Object;
                            Traveller_Id : in Agent.Agent_Id);

   not overriding
   function Book (
      This         : in Stretch.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   not overriding
   procedure Unbook (This         : in Stretch.Object;
                     Traveller_Id : in Agent.Agent_Id);

   overriding
   function Dump (This : Stretch.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Id_Field          return String is ("id");
   function Size_Field        return String is ("size");
   function Decorations_Field return String is ("decorations");
   function Travellers_Field  return String is ("travellers");
   function Overtake_Field    return String is ("overtaking");


   protected type Protected_Travellers_Queue
   is
      not overriding
      procedure Enter_If_Not_Already_Inside (
         Traveller_Id : in     Agent.Agent_Id;
         Entered      :    out Boolean);

      not overriding
      function Is_Stretch_Full return Boolean;

      not overriding
      function Is_Stretch_Not_Full return Boolean;

      not overriding
      function Is_Stretch_Empty return Boolean;

      not overriding
      function Size_Of_Treading_Travellers return Natural;

      not overriding
      procedure Enter_Into_Stretch (
         Traveller_Id : in     Agent.Agent_Id;
         Entered      :    out Boolean);

      not overriding
      procedure Exit_From_Stretch (
         Traveller_Id : in     Agent.Agent_Id;
         Exited       :    out Boolean);

      not overriding
      procedure Enter_Into_Waiting_List (
         Traveller_Id : in     Agent.Agent_Id;
         Entered      :    out Boolean);

      not overriding
      procedure Exit_From_Waiting_List (
         Traveller_Id : in     Agent.Agent_Id;
         Exited       :    out Boolean);

      not overriding
      function Is_Waiting_To_Enter_Stretch (
         Traveller_Id : in Agent.Agent_Id) return Boolean;

      not overriding
      function Is_Inside_The_Stretch (Traveller_Id : in Agent.Agent_Id)
         return Boolean;

      not overriding
      procedure Put_Traveller (
         Traveller_Id : in Agent.Agent_Id);

      not overriding
      function Has_Treading return Boolean;

      not overriding
      function Get_First_Treading return Agent.Agent_Id;

      not overriding
      function Has_Next_Treading (Traveller_Id : Agent.Agent_Id)
      return Boolean;

      not overriding
      function Get_Next_Treading (Traveller_Id : Agent.Agent_Id)
      return Agent.Agent_Id;

      procedure Set_Size (New_Size : Natural);

      not overriding
      procedure Set_Traveller_Utils (
         T_U : access Active.Traveller.Utils.Object'Class := null);

      function Grant_Tread (
         Traveller_Id : in Agent.Agent_Id)
      return Boolean;

      procedure Book (
         Traveller_Id  : in     Agent.Agent_Id;
         Booked_Return :    out Boolean);

      procedure Unbook (Traveller_Id : in Agent.Agent_Id);

      procedure Get_Overtaking (
         Is_Booked    : out Boolean;
         Traveller_Id : out Agent.Agent_Id);

      not overriding
      procedure Clear;

   private
      Stretch_Size        : Natural := 0;
      Treading_Travellers : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Waiting_Travellers  : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class;
      Booker : Agent.Agent_Id;
      Booked : Boolean := False;
   end Protected_Travellers_Queue;


private
   type Object is
     abstract new Ada.Finalization.Controlled
     and Treadable.Object
     and Street_Related_Infrastructure.Object
     and Intersectable.Object
     and Infrastructure.Object
     and Containable.Object with record
      Id          : Infra_Id;
      Lane_Id     : Infra_Id;
      Host_Id     : Infra_Id;
      Size        : Natural;
      Has_Host_Id : Boolean := False;
      Protected_Travellers_Queue : access Stretch.Protected_Travellers_Queue;
      Lane_Utils                 : access Lane.Utils.Object'Class;
      Traveller_Utils            : access Active.Traveller.Utils.Object'Class;
   end record;

   not overriding
   procedure Enter_If_Not_Already_Inside (
      This         : in     Infrastructure.Stretch.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Entered      :    out Boolean);

   not overriding
   function Get_Travellers_Queue (This : in Stretch.Object)
   return access Stretch.Protected_Travellers_Queue;

   overriding
   procedure Initialize (This : in out Stretch.Object);

   overriding
   procedure Adjust (This : in out Stretch.Object);

   overriding
   procedure Finalize (This : in out Stretch.Object);

   procedure Init (
      Stretch         : in out Infrastructure.Stretch.Object'Class;
      Id              : in     Infra_Id;
      Size            : in     Natural;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null);

end Reactive.Infrastructure.Stretch;
