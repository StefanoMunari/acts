with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Tests;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package Active.Traveller.Vehicle.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Host_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;

   type Vehicle_Test is abstract new Traveller.Tests.Traveller_Test with record
   -- initialize in all tests
      Stretch_Utils     : access Stretch.Utils.Mock.Object;
      Street_Utils      : access Street.Utils.Mock.Object;
      Traveller_Utils   : access Traveller_Utils_Mock_Pkg.Object;
      Host_Utils        : access Host_Utils_Mock_Pkg.Object;
      With_Travel_Tests : Boolean := True;
   end record;

   -- Test Routines:
   procedure Test_Board_Not_On_Board_Passenger (T : in out Vehicle_Test);
   procedure Test_Not_Board_Already_On_Board_Passenger (
      T : in out Vehicle_Test);
   procedure Test_Free_On_Board_Passenger (T : in out Vehicle_Test);
   procedure Test_Not_Free_Not_On_Board_Passenger (T : in out Vehicle_Test);
   procedure Test_Count_On_Board_Passengers (T : in out Vehicle_Test);
   procedure Test_Travel_With_People_Landing (T : in out Vehicle_Test);
   procedure Test_Travel_With_No_People_Landing (T : in out Vehicle_Test);

   procedure Register_Tests (T : in out Vehicle_Test);
   procedure Register_Tests_Without_Travel_Tests (T : in out Vehicle_Test);

end Active.Traveller.Vehicle.Tests;
