with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Lane.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;

package Reactive.Infrastructure.Way.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;

   type Way_Test is abstract new TC.Test_Case with record
      Way : Infrastructure.Way.Reference;
      Traveller_Utils : Active.Traveller.Utils.Mock.Reference;
      Lane_Utils : Reactive.Infrastructure.Lane.Utils.Mock.Reference;
      Street_Utils : Reactive.Infrastructure.Street.Utils.Mock.Reference;
   end record;

   overriding procedure Set_Up (T : in out Way_Test);

   -- Test Routines:
   procedure Test_Find_Street (T : in out Way_Test);
   procedure Test_Way_Id_Getter (T : in out Way_Test);
   procedure Test_Direct_Lane_Adding (T : in out Way_Test);
   procedure Test_Inverse_Lane_Adding (T : in out Way_Test);
   procedure Test_Find_Direct_Lane_By_Direction (T : in out Way_Test);
   procedure Test_Find_Inverse_Lane_By_Direction (T : in out Way_Test);
   procedure Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes
     (T : in out Way_Test);
   procedure Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes
     (T : in out Way_Test);
   procedure Test_Street_Setter (T : in out Way_Test);

   procedure Register_Tests (T : in out Way_Test);

private

   procedure Add_Lane (T                  : in out Way_Test;
                       Lane_Id            : in     Infra_Id;
                       Lane_Direction     : in     Direction.Straight;
                       Street_Orientation : in     Direction.Orientation;
                       Added              :    out Boolean);

end Reactive.Infrastructure.Way.Tests;
