with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Way.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package Reactive.Infrastructure.Lane.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;

   type Lane_Test is abstract new TC.Test_Case with record
      Lane            : Infrastructure.Lane.Reference;
      Traveller_Utils : Active.Traveller.Utils.Mock.Reference;
      Way_Utils       : Reactive.Infrastructure.Way.Utils.Mock.Reference;
      Stretch_Utils   : Reactive.Infrastructure.Stretch.Utils.Mock.Reference;
   end record;

   overriding procedure Set_Up (T : in out Lane_Test);
   overriding procedure Tear_Down (T: in out Lane_Test);

   -- Test Routines:
   procedure Test_Find_Street (T : in out Lane_Test);
   procedure Test_Intersections_Finder (T : in out Lane_Test);
   procedure Test_Lane_Id_Getter (T : in out Lane_Test);
   procedure Test_Stretch_Adding (T : in out Lane_Test);
   procedure Test_Way_Setter (T : in out Lane_Test);
   procedure Test_Lane_Direction_Getter (T : in out Lane_Test);
   procedure Test_Find_Stretch_Position (T : in out Lane_Test);
   procedure Test_Not_Find_Stretch_Position (T : in out Lane_Test);
   procedure Test_Equality_Of_Two_Lanes (T : in out Lane_Test);
   procedure Test_Inequality_Of_Two_Lanes (T : in out Lane_Test);
   procedure Test_Stretches_Counter (T : in out Lane_Test);
   procedure Test_Intersection_Adding (T : in out Lane_Test);
   procedure Test_No_Stretch_Next_To_A_Missing_Stretch (T : in out Lane_Test);

   procedure Register_Tests (T : in out Lane_Test);

private

   procedure Append_Stretch (T          : in out Lane_Test;
                             Stretch_Id : in     Infra_Id;
                             Added      :    out Boolean);

   procedure Set_Intersection (T               : in out Lane_Test;
                               Intersection_Id : in Infra_Id);

end Reactive.Infrastructure.Lane.Tests;
