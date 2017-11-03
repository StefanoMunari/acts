with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.Traveller;

with Reactive.District;
with Reactive.Infrastructure.Building.Host.Utils;
with Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy;
with Reactive.Infrastructure.Building.Parking_Manager;
with Reactive.Infrastructure.Stretch.Mock;

package Reactive.Infrastructure.Building.Host.Facility.Tests is
   package AU           renames AUnit;
   package TC           renames AUnit.Test_Cases;
   package Agent        renames Active.Agent;
   package District_Pkg renames Reactive.District;
   package Stretch_Mock renames Reactive.Infrastructure.Stretch.Mock;

   type Facility_Test is new TC.Test_Case with record
      Facility_Ref : Facility.Reference;
      Id           : Infra_Id;
      District     : District_Pkg.Reference;
      Host_Utils   : Host.Utils.Reference;
      Parking_Ref  : Parking_Manager.Reference;
      PC_Utils     : Active.People_Carrier.Utils.Reference;
      UC_Strategy  : Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Reference;
      Traveller_Utils : Active.Traveller.Utils.Reference;
      Stub            : Remote_Stub.Reference;
      Query_Builder   : Remote_Query_Builder.Reference;
      Wrapper_Factory : App_Wrapper.Abstract_Factory.Reference;
   end record;

   overriding
   procedure Set_Up (T : in out Facility_Test);
   overriding
   procedure Tear_Down (T : in out Facility_Test);

   -- Test Routines:
   procedure Test_Pedestrian_Stops_Over (T : in out Facility_Test);
   procedure Test_Vehicle_Stops_Over (T : in out Facility_Test);
   procedure Test_Exit_Building_Without_Vehicle (T : in out Facility_Test);
   procedure Test_Exit_Building_With_Vehicle (T : in out Facility_Test);
   procedure Test_Exit_Building_Not_Full_Without_Vehicle (
      T : in out Facility_Test);
   procedure Test_Exit_Building_Not_Full_With_Vehicle (
      T : in out Facility_Test);
   procedure Test_Exit_Building_Two_Passengers_With_Vehicle (
      T : in out Facility_Test);
   procedure Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle (
      T : in out Facility_Test);

   procedure Register_Tests (T : in out Facility_Test);
   overriding
   function Name (T : in Facility_Test) return AU.Message_String;

end Reactive.Infrastructure.Building.Host.Facility.Tests;
