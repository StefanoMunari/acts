with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Lane.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Lane_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Lane_Utils_Test);

   -- Test Routines:
   procedure Test_Intersections_Finder (T: in out TC.Test_Case'Class);
   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class);
   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class);
   procedure Test_Street_Finder (T: in out TC.Test_Case'Class);
   procedure Test_Stretches_Counter (T : in out TC.Test_Case'Class);
   procedure Test_Direction_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Enter (T: in out TC.Test_Case'Class);
   procedure Test_Not_Enter (T: in out TC.Test_Case'Class);
   procedure Test_Find_Stretch_Position (T: in out TC.Test_Case'Class);
   procedure Test_Intersection_Adder (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Lane_Utils_Test);
   overriding function Name (T : in Lane_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Lane.Utils.Tests;

