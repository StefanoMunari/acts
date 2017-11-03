with Reactive.District;

package body Reactive.Infrastructure.Intersection.Crossing is

   package District renames Reactive.District;

   procedure Cross (This         : in out Intersection.Crossing.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Direction    : in     Shared.Direction.Any;
                    Crossed      :    out Boolean)
   is
      Source : Shared.Direction.Cardinal;
   begin

      Source := Shared.Direction.Get_Source (Direction);
      This.Entrances (Source).Cross (Traveller_Id, Direction, Crossed);

      if Risky_Directions.Contains (Direction) then
         This.Yielding_Right_Of_Way_Gate.Cross_Notify (Direction);
      end if;
   end Cross;

   procedure Initialize (This           : in out Intersection.Crossing.Object;
                         Traffic_Lights : Traffic_Lights_Array) is
   begin
      if Risky_Directions.Is_Empty then
         Risky_Directions.Append (Direction.NORTH_SOUTH);
         Risky_Directions.Append (Direction.NORTH_EAST);
         Risky_Directions.Append (Direction.EAST_WEST);
         Risky_Directions.Append (Direction.EAST_SOUTH);
         Risky_Directions.Append (Direction.SOUTH_NORTH);
         Risky_Directions.Append (Direction.SOUTH_WEST);
         Risky_Directions.Append (Direction.WEST_EAST);
         Risky_Directions.Append (Direction.WEST_NORTH);
      end if;

      For D in Direction.Cardinal
      loop
         This.Exits (D) := new Intersection_Exit (D);
      end loop;

      This.Yielding_Right_Of_Way_Gate :=
        new Yielding_Right_Of_Way (Exits => This.Exits);
   end Initialize;

   function To_Risky (Direction : in Shared.Direction.Any)
   return Risky_Direction_Type
   is (Risky_Direction_Type'Value (Shared.Direction.Any'Image (Direction)));

   function Get_Traffic_Light (
      This           : in out Intersection.Crossing.Object;
      Exit_Direction : in     Direction.Cardinal)
   return Agent.Agent_Id is
   begin
      return This.Entrances (Exit_Direction).Get_Traffic_Light;
   end Get_Traffic_Light;

   procedure Set_Traffic_Light (
      This           : in out Intersection.Crossing.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal)
   is
   begin -- Set_Traffic_Light
      This.Entrances (Exit_Direction) := new Entering_Crossing (
            Source                     => Exit_Direction,
            Exits                      => This.Exits,
            Yielding_Right_Of_Way_Gate => This.Yielding_Right_Of_Way_Gate);
      This.Entrances (Exit_Direction).Set_Traffic_Light (Traffic_Light);
   end Set_Traffic_Light;

   protected body Entering_Crossing is
      function Is_Safe (Direction : in Shared.Direction.Any) return Boolean is
      begin
         return Shared.Direction
           ."=" (Direction,
              Shared.Direction.NORTH_WEST)
           or Shared.Direction
             ."=" (Direction,
                   Shared.Direction.SOUTH_EAST)
           or Shared.Direction
             ."=" (Direction,
                   Shared.Direction.EAST_NORTH)
        or Shared.Direction
             ."=" (Direction,
                   Shared.Direction.WEST_SOUTH);
      end Is_Safe;

      procedure Cross (Traveller : in Agent.Agent_Id;
                       Direction : in Shared.Direction.Any;
                       Crossed   : out Boolean) is
         Destination : Shared.Direction.Cardinal;
         District_Ref : District.Reference := District.Get_Instance;
         Traffic_Light : Active.Traffic_Light.Reference;
      begin
         Traffic_Light := District_Ref.Find_Traffic_Light_By_Id (
            Traffic_Light_Id);
         -- if traffic light is red then return false:
         -- not entering the crossing
         if not Traffic_Light.Is_Green then
            Crossed := FALSE;
         end if;
         Crossed := TRUE;
         pragma Warnings (Off, "potentially blocking operation in protected operation");
         if Is_Safe (Direction) then
            Destination := Shared.Direction.Get_Destination (Direction);
            Exits (Destination).Take (Traveller, Direction);
         else
            Yielding_Right_Of_Way_Gate.Cross (Traveller, Direction);
         end if;
         pragma Warnings (On, "potentially blocking operation in protected operation");
      end Cross;

      function Get_Traffic_Light return Agent.Agent_Id
      is (Traffic_Light_Id);

      procedure Set_Traffic_Light (Traffic_Light : in Agent.Agent_Id) is
      begin
         Traffic_Light_Id := Traffic_Light;
      end Set_Traffic_Light;
   end Entering_Crossing;

   protected body Yielding_Right_Of_Way is
      function Should_Wait_For_Others (Direction : in Shared.Direction.Any)
         return Boolean
      is
         Should_Give_The_Way_To : Direction_List.List;
      begin
         case Direction is
            when Shared.Direction.NORTH_SOUTH
               => Should_Give_The_Way_To.Append (Shared.Direction.SOUTH_WEST);
            when Shared.Direction.SOUTH_NORTH
               => Should_Give_The_Way_To.Append (Shared.Direction.NORTH_EAST);
            when Shared.Direction.EAST_WEST
               => Should_Give_The_Way_To.Append (Shared.Direction.WEST_NORTH);
            when Shared.Direction.WEST_EAST
               => Should_Give_The_Way_To.Append (Shared.Direction.EAST_SOUTH);
            when Shared.Direction.NORTH_EAST
               => Should_Give_The_Way_To.Append (Shared.Direction.SOUTH_NORTH);
                  Should_Give_The_Way_To.Append (Shared.Direction.SOUTH_WEST);
            when Shared.Direction.SOUTH_WEST
               => Should_Give_The_Way_To.Append (Shared.Direction.NORTH_SOUTH);
                  Should_Give_The_Way_To.Append (Shared.Direction.NORTH_EAST);
            when Shared.Direction.EAST_SOUTH
               => Should_Give_The_Way_To.Append (Shared.Direction.WEST_EAST);
                  Should_Give_The_Way_To.Append (Shared.Direction.WEST_NORTH);
            when Shared.Direction.WEST_NORTH
               => Should_Give_The_Way_To.Append (Shared.Direction.EAST_WEST);
                Should_Give_The_Way_To.Append (Shared.Direction.EAST_SOUTH);
            when others => null;
         end case;
         for R of Should_Give_The_Way_To
         loop
            if Crossing_Directions.Contains (R) then
               return TRUE;
            end if;
         end loop;
         return FALSE;
      end Should_Wait_For_Others;

      entry Cross (Traveller : in Agent.Agent_Id;
                   Direction : in Shared.Direction.Any)
        when Natural(Crossing_Directions.Length) < 2
      is
         Destination : Shared.Direction.Cardinal;
      begin
         if Should_Wait_For_Others (Direction) then
            Crossing_Directions.Append (Direction);

            requeue Safe_Cross (To_Risky (Direction));
         else
            Crossing_Directions.Append (Direction);
            Destination := Shared.Direction.Get_Destination (Direction);

            requeue Exits (Destination).Take;
         end if;
      end Cross;

      entry Safe_Cross (for D in Risky_Direction_Type) (
         Traveller : in Agent.Agent_Id;
         Direction : in Shared.Direction.Any)
      when Pass(D) is
         Destination : Shared.Direction.Cardinal;
      begin
         Pass (D) := FALSE;
         Destination := Shared.Direction
           .Get_Destination (Direction);
         requeue Exits (Destination).Take;
      end Safe_Cross;

      entry Cross_Notify (Direction : in Shared.Direction.Any)
        when TRUE is
         Position : Direction_List.Cursor
           := Crossing_Directions.Find (Direction);
      begin
         Crossing_Directions.Delete (Position);
         if not Crossing_Directions.Is_Empty then
            Pass (To_Risky (Crossing_Directions.First_Element)) := TRUE;
         end if;
      end Cross_Notify;
   end Yielding_Right_Of_Way;

   protected body Intersection_Exit is
      entry Take (Traveller : in Agent.Agent_Id;
                  Direction : in Shared.Direction.Any)
        when TRUE is
      begin
        null;
      end Take;
   end Intersection_Exit;

end Reactive.Infrastructure.Intersection.Crossing;
