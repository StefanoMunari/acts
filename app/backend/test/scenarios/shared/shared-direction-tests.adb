with Ada.Containers.Doubly_Linked_Lists;
with AUnit.Assertions;
with Shared.Direction;

use Ada.Containers;

package body Shared.Direction.Tests is
   package Ass renames AUnit.Assertions;
   package Direction renames Shared.Direction;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   NS       : Direction.Straight;
   SN       : Direction.Straight;
   EW       : Direction.Straight;
   WE       : Direction.Straight;
   N        : Direction.Cardinal;
   S        : Direction.Cardinal;
   W        : Direction.Cardinal;
   E        : Direction.Cardinal;

   procedure Set_Up_Case (T: in out Direction_Test) is
   begin
      null;
   end Set_Up_Case;

   procedure Set_Up (T: in out Direction_Test) is
   begin
      NS := NORTH_SOUTH;
      SN := SOUTH_NORTH;
      EW := EAST_WEST;
      WE := WEST_EAST;
      N  := NORTH;
      S  := SOUTH;
      W  := WEST;
      E  := EAST;
   end Set_Up;

   procedure Tear_Down (T: in out Direction_Test) is
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Direction_Test) is
   begin
      null;
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Reverse_NS (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight := SOUTH_NORTH;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (NS)),
                   "Does not reverse NORTH_SOUTH");
   end Reverse_NS;

   procedure Reverse_SN (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight := NORTH_SOUTH;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (SN)),
                   "Does not reverse SOUTH_NORTH");
   end Reverse_SN;

   procedure Reverse_EW (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight := WEST_EAST;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (EW)),
                   "Does not reverse EAST_WEST");
   end Reverse_EW;

   procedure Reverse_WE (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight := EAST_WEST;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (WE)),
                   "Does not reverse WEST_EAST");
   end Reverse_WE;
   
   procedure Inverse_Of_West (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Cardinal := EAST;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (WEST)),
                   "Does not inverse WEST");
   end Inverse_Of_West;

   procedure Inverse_Of_East (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Cardinal := WEST;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (EAST)),
                   "Does not inverse EAST");
   end Inverse_Of_East;

   procedure Inverse_Of_South (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Cardinal := NORTH;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (SOUTH)),
                   "Does not inverse SOUTH");
   end Inverse_Of_South;

   procedure Inverse_Of_North (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Cardinal := SOUTH;
   begin
      Ass.Assert ((Expected_Value = Get_Inverse_Direction (NORTH)),
                   "Does not inverse NORTH");
   end Inverse_Of_North;

   procedure Possible_Straight_Directions_For_Hor (T: in out TC.Test_Case'Class) is
      Possible_Dir : Straight_Direction_List.List;
   begin
      Possible_Dir := Direction.Find_Possible_Straight_Directions (HORIZONTAL);
      Ass.Assert ((Possible_Dir.Contains(WEST_EAST) and
                   Possible_Dir.Contains(EAST_WEST)),
                   "Does not contain WEST_EAST or EAST_WEST");
   end Possible_Straight_Directions_For_Hor;

   procedure Possible_Straight_Directions_For_Ver (T: in out TC.Test_Case'Class) is
      Possible_Dir : Straight_Direction_List.List;
   begin
      Possible_Dir := Direction.Find_Possible_Straight_Directions (VERTICAL);
      Ass.Assert ((Possible_Dir.Contains(NORTH_SOUTH) and
                   Possible_Dir.Contains(SOUTH_NORTH)),
                   "Does not contain NORTH_SOUTH or SOUTH_NORTH");
   end Possible_Straight_Directions_For_Ver;
   
   procedure Possible_Cardinal_Directions_For_Hor (T: in out TC.Test_Case'Class) is
      Possible_Dir : Cardinal_Direction_List.List;
   begin
      Possible_Dir := Direction.Find_Possible_Cardinal_Directions (HORIZONTAL);
      Ass.Assert ((Possible_Dir.Contains(WEST) and
                   Possible_Dir.Contains(EAST)),
                   "Does not contain WEST_EAST or EAST_WEST");
   end Possible_Cardinal_Directions_For_Hor;

   procedure Possible_Cardinal_Directions_For_Ver (T: in out TC.Test_Case'Class) is
      Possible_Dir : Cardinal_Direction_List.List;
   begin
      Possible_Dir := Direction.Find_Possible_Cardinal_Directions (VERTICAL);
      Ass.Assert ((Possible_Dir.Contains(NORTH) and
                   Possible_Dir.Contains(SOUTH)),
                   "Does not contain NORTH_SOUTH or SOUTH_NORTH");
   end Possible_Cardinal_Directions_For_Ver;

   procedure Calculates_Incoming_Directions (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight;
   begin
      Expected_Value := NORTH_SOUTH;
      Ass.Assert ((Direction.Calculate_Incoming_Direction (NORTH) = Expected_Value),
                   "Does not infer NORTH_SOUTH from NORTH");
      Expected_Value := SOUTH_NORTH;
      Ass.Assert ((Direction.Calculate_Incoming_Direction (SOUTH) = Expected_Value),
                   "Does not infer SOUTH_NORTH from SOUTH");
      Expected_Value := WEST_EAST;
      Ass.Assert ((Direction.Calculate_Incoming_Direction (WEST) = Expected_Value),
                   "Does not infer WEST_EAST from WEST");
      Expected_Value := EAST_WEST;
      Ass.Assert ((Direction.Calculate_Incoming_Direction (EAST) = Expected_Value),
                   "Does not infer EAST_WEST from EAST");
   end Calculates_Incoming_Directions;
   
   procedure Calculates_Outcoming_Directions (T: in out TC.Test_Case'Class) 
   is
      Expected_Value : Direction.Straight;
   begin
      Expected_Value := NORTH_SOUTH;
      Ass.Assert ((Direction.Calculate_Outcoming_Direction (SOUTH) = Expected_Value),
                   "Does not infer NORTH_SOUTH from SOUTH");
      Expected_Value := SOUTH_NORTH;
      Ass.Assert ((Direction.Calculate_Outcoming_Direction (NORTH) = Expected_Value),
                   "Does not infer SOUTH_NORTH from NORTH");
      Expected_Value := WEST_EAST;
      Ass.Assert ((Direction.Calculate_Outcoming_Direction (EAST) = Expected_Value),
                   "Does not infer WEST_EAST from EAST");
      Expected_Value := EAST_WEST;
      Ass.Assert ((Direction.Calculate_Outcoming_Direction (WEST) = Expected_Value),
                   "Does not infer EAST_WEST from WEST");
   end Calculates_Outcoming_Directions;

   -----------------------------------------------------
   --                  REGISTRATION 
   -----------------------------------------------------
   procedure Register_Tests (T: in out Direction_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Reverse_NS'Access,
                        Name    => "Reverse_North_South");
      Register_Routine (Test    => T,
                        Routine => Reverse_SN'Access,
                        Name    => "Reverse_South_North");
      Register_Routine (Test    => T,
                        Routine => Reverse_EW'Access,
                        Name    => "Reverse_East_West");
      Register_Routine (Test    => T,
                        Routine => Reverse_WE'Access,
                        Name    => "Reverse_West_East");
      Register_Routine (Test    => T,
                        Routine => Inverse_Of_West'Access,
                        Name    => "Inverse_North_South");
      Register_Routine (Test    => T,
                        Routine => Inverse_Of_East'Access,
                        Name    => "Inverse_South_North");
      Register_Routine (Test    => T,
                        Routine => Inverse_Of_South'Access,
                        Name    => "Inverse_East_West");
      Register_Routine (Test    => T,
                        Routine => Inverse_Of_North'Access,
                        Name    => "Inverse_West_East");
      Register_Routine (Test    => T,
                        Routine => Possible_Straight_Directions_For_Hor'Access,
                        Name    => "Possible_Straight_Directions_For_Hor");
      Register_Routine (Test    => T,
                        Routine => Possible_Straight_Directions_For_Ver'Access,
                        Name    => "Possible_Straight_Directions_For_Ver");
      Register_Routine (Test    => T,
                        Routine => Possible_Cardinal_Directions_For_Hor'Access,
                        Name    => "Possible_Cardinal_Directions_For_Hor");
      Register_Routine (Test    => T,
                        Routine => Possible_Cardinal_Directions_For_Ver'Access,
                        Name    => "Possible_Cardinal_Directions_For_Ver");
      Register_Routine (Test    => T,
                        Routine => Calculates_Incoming_Directions'Access,
                        Name    => "Calculates_Incoming_Directions");
      Register_Routine (Test    => T,
                        Routine => Calculates_Outcoming_Directions'Access,
                        Name    => "Calculates_Outcoming_Directions");
   end Register_Tests;

   function Name(T: Direction_Test) return AU.Message_String is
   begin
      return AU.Format ("Shared.Direction");
   end Name;
end Shared.Direction.Tests;
