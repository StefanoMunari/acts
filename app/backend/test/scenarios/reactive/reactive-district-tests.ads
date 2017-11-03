with AUnit;
with AUnit.Test_Cases;

package Reactive.District.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type District_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out District_Test);

   procedure Register_Tests (T: in out District_Test);
   overriding function Name (T: in District_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Infrastructure_Contains (T : in out TC.Test_Case'Class);
   procedure Test_Infrastructure_Find (T : in out TC.Test_Case'Class);
   procedure Test_Treadable_Find (T : in out TC.Test_Case'Class);
   procedure Test_Intersection_Find (T : in out TC.Test_Case'Class);
   procedure Test_Way_Find (T : in out TC.Test_Case'Class);
   procedure Test_Roadway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Footway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Bikeway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Lane_Find (T : in out TC.Test_Case'Class);
   procedure Test_Stretch_Find (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Find (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Add (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Remove (T : in out TC.Test_Case'Class);
   -- Add_Infrastructure tests
   procedure Test_Intersection_Add (T : in out TC.Test_Case'Class);
   procedure Test_Street_Add       (T : in out TC.Test_Case'Class);
   procedure Test_Roadway_Add      (T : in out TC.Test_Case'Class);
   procedure Test_Footway_Add      (T : in out TC.Test_Case'Class);
   procedure Test_Bikeway_Add      (T : in out TC.Test_Case'Class);
   procedure Test_Lane_Add         (T : in out TC.Test_Case'Class);
   procedure Test_Stretch_Add      (T : in out TC.Test_Case'Class);
   -- TODO: Should we do some Try_To_Tread_Infrastructure with false POST?
   procedure Test_Try_To_Tread_Loc (T : in out TC.Test_Case'Class);
   procedure Test_Try_To_Tread_Rem (T : in out TC.Test_Case'Class);

end Reactive.District.Tests;
