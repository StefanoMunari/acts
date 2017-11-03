with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.People_Carrier.Utils;

package Reactive.Infrastructure.Building.Host.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;
   package People_Carrier_Utils renames Active.People_Carrier.Utils;

   type Host_Utils_Test is new TC.Test_Case with record
      Host_Utils_Ref  : Host.Utils.Reference;
      PC_Utils        : People_Carrier_Utils.Reference;
      District_Ref    : access Reactive.District.Object'Class;
      Stub            : Stub_Pkg.Reference;
      Wrapper_Factory : Wrapper_Fac_Pkg.Reference;
   end record;

   overriding
   procedure Set_Up (T : in out Host_Utils_Test);
   overriding
   procedure Tear_Down (T : in out Host_Utils_Test);

   -- Test Routines:
   procedure Test_Pedestrian_Stops_Over (T : in out Host_Utils_Test);
   procedure Test_Vehicle_Stops_Over (T : in out Host_Utils_Test);
   procedure Test_Exit_Building_Defer (T : in out Host_Utils_Test);
   procedure Test_Exit_Building_Retry (T : in out Host_Utils_Test);
   procedure Test_Exit_Building_Do_Not_Defer (T : in out Host_Utils_Test);

   procedure Register_Tests (T : in out Host_Utils_Test);
   overriding
   function Name (T : in Host_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Building.Host.Utils.Tests;
