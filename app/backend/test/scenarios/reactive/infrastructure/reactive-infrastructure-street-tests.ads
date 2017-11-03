with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Street.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Street_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Street_Test);

   procedure Register_Tests (T: in out Street_Test);
   overriding function Name (T: in Street_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Find_Lanes_By_Direction (T : in out TC.Test_Case'Class);
   procedure Test_Street_Id (T: in out TC.Test_Case'Class);
   procedure Test_Street_Orientation (T: in out TC.Test_Case'Class);
   procedure Test_Roadway_Setter (T: in out TC.Test_Case'Class);
   procedure Test_Bikeway_Setter (T: in out TC.Test_Case'Class);
   procedure Test_Footway_Setter (T: in out TC.Test_Case'Class);
   procedure Test_Horizontal_Street_Is_Treadable_Towards_East (T: in out TC.Test_Case'Class);
   procedure Test_Horizontal_Street_Is_Treadable_Towards_West (T: in out TC.Test_Case'Class);
   procedure Test_Horizontal_Street_Is_Not_Treadable_Towards_North (T: in out TC.Test_Case'Class);
   procedure Test_Horizontal_Street_Is_Not_Treadable_Towards_South (T: in out TC.Test_Case'Class);
   procedure Test_Vertical_Street_Is_Treadable_Towards_North (T: in out TC.Test_Case'Class);
   procedure Test_Vertical_Street_Is_Treadable_Towards_South (T: in out TC.Test_Case'Class);
   procedure Test_Vertical_Street_Is_Not_Treadable_Towards_East (T: in out TC.Test_Case'Class);
   procedure Test_Vertical_Street_Is_Not_Treadable_Towards_West (T: in out TC.Test_Case'Class);
   procedure Test_Find_Street (T: in out TC.Test_Case'Class);
   procedure Test_Equality_Of_Two_Streets (T : in out TC.Test_Case'Class);
   procedure Test_Inequality_Of_Two_Streets (T : in out TC.Test_Case'Class);

end Reactive.Infrastructure.Street.Tests;
