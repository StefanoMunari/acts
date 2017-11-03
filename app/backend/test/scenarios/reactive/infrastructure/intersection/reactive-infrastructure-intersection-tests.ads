with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Intersection.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Intersection_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Intersection_Test);

   procedure Register_Tests (T : in out Intersection_Test);
   overriding function Name (T : in Intersection_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Intersection_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Intersection_Size_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Intersection_Street_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Find_Intersections (T : in out TC.Test_Case'Class);
   procedure Test_Streets_Count (T : in out TC.Test_Case'Class);
   procedure Test_Equality_Of_Two_Intersections (T : in out TC.Test_Case'Class);
   procedure Test_Inequality_Of_Two_Intersections (T : in out TC.Test_Case'Class);
   procedure Test_Find_Street_Direction (T : in out TC.Test_Case'Class);
   procedure Test_Not_Find_Street_Direction (T : in out TC.Test_Case'Class);
   procedure Test_Find_Some_Street_Connected_With_Intersection (T : in out TC.Test_Case'Class);
   procedure Test_Find_No_Street_Connected_With_Intersection (T : in out TC.Test_Case'Class);
   procedure Test_Street_Existence (T : in out TC.Test_Case'Class);
   procedure Test_No_Street_Existence (T : in out TC.Test_Case'Class);
   procedure Test_Street_Connection (T : in out TC.Test_Case'Class);
   procedure Test_Increment_Streets (T : in out TC.Test_Case'Class);
   procedure Test_Is_Fully_Connected (T : in out TC.Test_Case'Class);
   procedure Test_Is_Not_Fully_Connected (T : in out TC.Test_Case'Class);
   procedure Test_Dump (T : in out TC.Test_Case'Class);
end Reactive.Infrastructure.Intersection.Tests;
