with Ada.Finalization;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;

with Active.Agent;
with Active.Traffic_Light;

package Reactive.Infrastructure.Intersection.Crossing is

   package Agent renames Active.Agent;
   package Direction_List renames Shared.Direction.List;
   package Direction_Set renames Shared.Direction.Set;
   package Traffic_Light renames Active.Traffic_Light;

   type Object is new Ada.Finalization.Limited_Controlled with private;
   type Reference is access all Object'Class;

   type Risky_Direction_Type is (NORTH_SOUTH,
                                 NORTH_EAST,
                                 EAST_WEST,
                                 EAST_SOUTH,
                                 SOUTH_NORTH,
                                 SOUTH_WEST,
                                 WEST_EAST,
                                 WEST_NORTH);

   type Traffic_Lights_Array is array (Direction.Cardinal)
   of Agent.Agent_Id;

   not overriding
   procedure Cross (This : in out Intersection.Crossing.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Direction : in Shared.Direction.Any;
                    Crossed : out Boolean);

   not overriding
   procedure Initialize (This           : in out Intersection.Crossing.Object;
                         Traffic_Lights : Traffic_Lights_Array);

   not overriding
   function Get_Traffic_Light (
      This           : in out Intersection.Crossing.Object;
      Exit_Direction : in     Direction.Cardinal) return Agent.Agent_Id;

   not overriding
   procedure Set_Traffic_Light (
      This           : in out Intersection.Crossing.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal);

private
   protected type Intersection_Exit (Destination : Shared.Direction.Cardinal)
   is
      not overriding
      entry Take (Traveller : in Agent.Agent_Id;
                  Direction : in Shared.Direction.Any);
   end Intersection_Exit;

   type Exits_Array is array (Direction.Cardinal) of access Intersection_Exit;
   type Exit_Array_Reference is access all Exits_Array;
   type Pass_Array is array (Risky_Direction_Type) of Boolean;

   protected type Yielding_Right_Of_Way (Exits : Exit_Array_Reference) is
      not overriding
      function Should_Wait_For_Others (Direction : in Shared.Direction.Any)
         return Boolean;

      not overriding
      entry Cross (Traveller : in Agent.Agent_Id;
                   Direction : in Shared.Direction.Any);

      not overriding
      entry Cross_Notify (Direction : in Shared.Direction.Any);

   private
      not overriding
      entry Safe_Cross (Risky_Direction_Type) (
         Traveller : in Agent.Agent_Id;
         Direction : in Shared.Direction.Any);

      Crossing_Directions : Direction_List.List;
      Pass : Pass_Array;
   end Yielding_Right_Of_Way;

   type Yielding_Right_Of_Way_Gate_Reference is
      access all Yielding_Right_Of_Way;

   protected type Entering_Crossing (
      Source                     : Direction.Cardinal;
      Exits                      : Exit_Array_Reference;
      Yielding_Right_Of_Way_Gate : Yielding_Right_Of_Way_Gate_Reference)
   is
      not overriding
      function Is_Safe (Direction : in Shared.Direction.Any) return Boolean;

      not overriding
      procedure Cross (Traveller : in Agent.Agent_Id;
                       Direction : in Shared.Direction.Any;
                       Crossed   : out Boolean);

      not overriding
      function Get_Traffic_Light return Agent.Agent_Id;

      not overriding
      procedure Set_Traffic_Light (Traffic_Light : in Agent.Agent_Id);

   private
      Traffic_Light_Id : Agent.Agent_Id;
   end Entering_Crossing;

   type Entrances_Array is array (Direction.Cardinal)
      of access Entering_Crossing;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Entrances : Entrances_Array;
      Exits : access Exits_Array := new Exits_Array;
      Yielding_Right_Of_Way_Gate : access Yielding_Right_Of_Way;
      Intersection_Exit : access Intersection.Crossing.Intersection_Exit;
   end record;

   Risky_Directions : Direction_List.List;

end Reactive.Infrastructure.Intersection.Crossing;
