with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Tests;

with Active.Traveller.Strategy.Mock;

with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;

package Active.Traveller.Pedestrian.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Strategy_Mock_Pkg renames Active.Traveller.Strategy.Mock;
   package Stretch_Sign_Decorator_Utils_Mock
      renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;

   type Pedestrian_Test is new Traveller.Tests.Traveller_Test with record
      Pedestrian_Ref : Active.Traveller.Pedestrian.Reference;
      Strategy_Ref   : access Strategy_Mock_Pkg.Object'Class;
      Street_Utils   : access Street.Utils.Mock.Object;
      SSD_Utils      : access Stretch_Sign_Decorator_Utils_Mock.Object;
   end record;

   overriding procedure Set_Up (T: in out Pedestrian_Test);
   -- Pedestrian-specific test routines:
   procedure Test_Is_Affected_By_Traffic_Lights (T : in out Pedestrian_Test);
   procedure Test_On_Bus_Stop (T : in out Pedestrian_Test);
   procedure Test_Stop_Waiting (T : in out Pedestrian_Test);

   procedure Register_Tests (T: in out Pedestrian_Test);
   overriding function Name (T: in Pedestrian_Test) return AU.Message_String;

end Active.Traveller.Pedestrian.Tests;
