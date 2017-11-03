package body Shared.Direction is

   function Get_Inverse_Direction (Straight_Direction : Direction.Straight)
                                   return Direction.Straight is
      Inverse_Direction : Direction.Straight;
   begin
      case Straight_Direction is
         when SOUTH_NORTH
            => Inverse_Direction := NORTH_SOUTH;
         when NORTH_SOUTH
            => Inverse_Direction := SOUTH_NORTH;
         when WEST_EAST
            => Inverse_Direction := EAST_WEST;
         when EAST_WEST
            => Inverse_Direction := WEST_EAST;
      end case;
      return Inverse_Direction;
   end Get_Inverse_Direction;

   function Get_Inverse_Direction (Cardinal_Direction : Direction.Cardinal)
                                   return Direction.Cardinal is
      Inverse_Direction : Direction.Cardinal;
   begin
      case Cardinal_Direction is
         when NORTH
            => Inverse_Direction := SOUTH;
         when SOUTH
            => Inverse_Direction := NORTH;
         when WEST
            => Inverse_Direction := EAST;
         when EAST
            => Inverse_Direction := WEST;
      end case;
      return Inverse_Direction;
   end Get_Inverse_Direction;

   function Calculate_Incoming_Direction
     (Cardinal_Direction : Direction.Cardinal) return Direction.Straight is
      Incoming_Direction : Direction.Straight;
   begin
      case Cardinal_Direction is
         when NORTH
            => Incoming_Direction := NORTH_SOUTH;
         when SOUTH
            => Incoming_Direction := SOUTH_NORTH;
         when WEST
            => Incoming_Direction := WEST_EAST;
         when EAST
            => Incoming_Direction := EAST_WEST;
      end case;
      return Incoming_Direction;
   end Calculate_Incoming_Direction;

   function Calculate_Outcoming_Direction
     (Cardinal_Direction : Direction.Cardinal) return Direction.Straight is
      Outcoming_Direction : Direction.Straight;
   begin
      case Cardinal_Direction is
         when SOUTH
            => Outcoming_Direction := NORTH_SOUTH;
         when NORTH
            => Outcoming_Direction := SOUTH_NORTH;
         when EAST
            => Outcoming_Direction := WEST_EAST;
         when WEST
            => Outcoming_Direction := EAST_WEST;
      end case;
      return Outcoming_Direction;
   end Calculate_Outcoming_Direction;

   not overriding
   function Find_Possible_Straight_Directions
     (Orientation_Value : Direction.Orientation)
      return Straight_Direction_List.List
   is
      Directions : Straight_Direction_List.List;
   begin
      if Orientation_Value = HORIZONTAL then
         Directions.Append (WEST_EAST);
         Directions.Append (EAST_WEST);
      else
         Directions.Append (NORTH_SOUTH);
         Directions.Append (SOUTH_NORTH);
      end if;
      return Directions;
   end Find_Possible_Straight_Directions;

   not overriding
   function Find_Possible_Cardinal_Directions
     (Orientation_Value : Direction.Orientation)
      return Cardinal_Direction_List.List
   is
      Directions : Cardinal_Direction_List.List;
   begin
      if Orientation_Value = HORIZONTAL then
         Directions.Append (WEST);
         Directions.Append (EAST);
      else
         Directions.Append (NORTH);
         Directions.Append (SOUTH);
      end if;
      return Directions;
   end Find_Possible_Cardinal_Directions;

   function Get_Source (Direction : Shared.Direction.Any) return Cardinal is
      Source : Cardinal;
   begin
      case Direction is
         when SOUTH_NORTH | SOUTH_WEST | SOUTH_EAST
            => Source := SOUTH;
         when NORTH_SOUTH | NORTH_WEST | NORTH_EAST
            => Source := NORTH;
         when WEST_SOUTH | WEST_NORTH | WEST_EAST
            => Source := WEST;
         when EAST_SOUTH | EAST_NORTH | EAST_WEST
            => Source := EAST;
      end case;
      return Source;
   end Get_Source;

   function Get_Destination (Direction : Shared.Direction.Any) return Cardinal is
      Destination : Cardinal;
   begin
      case Direction is
         when SOUTH_NORTH | WEST_NORTH | EAST_NORTH
            => Destination := NORTH;
         when NORTH_SOUTH | EAST_SOUTH | WEST_SOUTH
            => Destination := SOUTH;
         when WEST_EAST | SOUTH_EAST | NORTH_EAST
            => Destination := EAST;
         when EAST_WEST | SOUTH_WEST | NORTH_WEST
            => Destination := WEST;
      end case;
      return Destination;
   end Get_Destination;

   not overriding
   function Combine (From, To : Cardinal) return Direction.Any is
   begin
      return Direction.Any'Value (Direction.Cardinal'Image (From)
                                  & "_" & Direction.Cardinal'Image (To));
   end Combine;

   function To_Any (Direction : Straight) return Shared.Direction.Any is
   begin
      return Shared.Direction.Any'Value
        (Shared.Direction.Straight'Image (Direction));
   end To_Any;

   function To_Straight (Direction : Shared.Direction.Any)
                         return Shared.Direction.Straight is
   begin
      return Shared.Direction.Straight'Value
        (Shared.Direction.Any'Image (Direction));
   end To_Straight;

end Shared.Direction;
