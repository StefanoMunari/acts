with AUnit;
with AUnit.Test_Cases;

with Active.Agent;

package Reactive.Infrastructure.Intersection.Crossing.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;

   type Intersection_Crossing_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Intersection_Crossing_Test);

   -- internal mock for crossing tests
   task type TX (Traveller_Id : Natural;
                 Intersection_Crossing_Ref : Intersection.Crossing.Reference) is
      entry Start_Crossing (D : Shared.Direction.Any);
      entry Wait_For_Crossing;
   end TX;

   -- Test Routines:
   procedure Test_Safe_Cross (T : in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Intersection_Crossing_Test);
   overriding function Name (T : in Intersection_Crossing_Test) return AU.Message_String;

end Reactive.Infrastructure.Intersection.Crossing.Tests;
