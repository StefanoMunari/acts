with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

package Shared.Direction is

   type Any is (NORTH_SOUTH,
                NORTH_EAST,
                NORTH_WEST,
                EAST_WEST,
                EAST_SOUTH,
                EAST_NORTH,
                SOUTH_NORTH,
                SOUTH_WEST,
                SOUTH_EAST,
                WEST_EAST,
                WEST_SOUTH,
                WEST_NORTH);

   type Straight is (NORTH_SOUTH,
                     EAST_WEST,
                     SOUTH_NORTH,
                     WEST_EAST);

   type Cardinal is (NORTH,
                     SOUTH,
                     WEST,
                     EAST);

   type Orientation is (HORIZONTAL,
                        VERTICAL);

   package List is
     new Ada.Containers
       .Doubly_Linked_Lists (Element_Type => Direction.Any,
                             "="          => "=");

   package Straight_Direction_List is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Direction.Straight,
                                             "="          => Direction."=");

   package Cardinal_Direction_List is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Direction.Cardinal,
                                             "="          => Direction."=");

   package Set is
      new Ada.Containers.Ordered_Sets
       (Element_Type    => Direction.Any,
        "="             => "=",
        "<"             => "<");

   package Straight_Direction_Map is
      new Ada.Containers.Ordered_Maps
       (Key_Type        => Natural,
        Element_Type    => Direction.Straight,
        "="             => "=",
        "<"             => "<");

   not overriding
   function Get_Inverse_Direction (
      Straight_Direction : Direction.Straight)
   return Direction.Straight;

   not overriding
   function Get_Inverse_Direction (
      Cardinal_Direction : Direction.Cardinal)
   return Direction.Cardinal;

   not overriding
   function Calculate_Incoming_Direction (
      Cardinal_Direction : Direction.Cardinal)
   return Direction.Straight;

   not overriding
   function Calculate_Outcoming_Direction (
      Cardinal_Direction : Direction.Cardinal)
   return Direction.Straight;

   not overriding
   function Find_Possible_Straight_Directions (
      Orientation_Value : Direction.Orientation)
   return Straight_Direction_List.List;

   not overriding
   function Find_Possible_Cardinal_Directions (
      Orientation_Value : Direction.Orientation)
   return Cardinal_Direction_List.List;

   not overriding
   function Get_Source (Direction : Shared.Direction.Any) return Cardinal;

   not overriding
   function Get_Destination (Direction : Shared.Direction.Any) return Cardinal;

   not overriding
   function Combine (From, To : Cardinal) return Direction.Any;

   not overriding
   function To_Any (Direction : Straight) return Shared.Direction.Any;

   not overriding
   function To_Straight (Direction : Shared.Direction.Any)
                         return Shared.Direction.Straight;

end Shared.Direction;
