with AUnit;
with AUnit.Test_Cases;

package Shared.Direction.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Direction_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Direction_Test);
   overriding procedure Tear_Down_Case (T: in out Direction_Test);
   overriding procedure Set_Up (T: in out Direction_Test);
   overriding procedure Tear_Down (T: in out Direction_Test);

   procedure Register_Tests (T: in out Direction_Test);
   function Name (T: in Direction_Test) return AU.Message_String;

   -- Test Routines:
   procedure Reverse_NS (T: in out TC.Test_Case'Class);
   procedure Reverse_SN (T: in out TC.Test_Case'Class);
   procedure Reverse_EW (T: in out TC.Test_Case'Class);
   procedure Reverse_WE (T: in out TC.Test_Case'Class);
   procedure Inverse_Of_West (T: in out TC.Test_Case'Class);
   procedure Inverse_Of_East (T: in out TC.Test_Case'Class);
   procedure Inverse_Of_South (T: in out TC.Test_Case'Class);
   procedure Inverse_Of_North (T: in out TC.Test_Case'Class);
   procedure Possible_Straight_Directions_For_Hor (T: in out TC.Test_Case'Class);
   procedure Possible_Straight_Directions_For_Ver (T: in out TC.Test_Case'Class);
   procedure Possible_Cardinal_Directions_For_Hor (T: in out TC.Test_Case'Class);
   procedure Possible_Cardinal_Directions_For_Ver (T: in out TC.Test_Case'Class);
   procedure Calculates_Incoming_Directions (T: in out TC.Test_Case'Class);
   procedure Calculates_Outcoming_Directions (T: in out TC.Test_Case'Class);
end Shared.Direction.Tests;
