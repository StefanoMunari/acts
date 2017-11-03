with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.People_Carrier.Utils;
with Active.Traveller.Utils;

with Reactive.District;

package Reactive.Infrastructure.Building.Parking_Manager.Garage.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;

   type Garage_Test is new TC.Test_Case with record
      Garage_Ref      : Garage.Reference;
      PC_Utils        : Active.People_Carrier.Utils.Reference;
      Traveller_Utils : Active.Traveller.Utils.Reference;
   end record;

   overriding
   procedure Set_Up (T : in out Garage_Test);
   overriding
   procedure Tear_Down (T : in out Garage_Test);

   -- Test Routines:
   procedure Test_Park_Vehicle_With_Room (T : in out Garage_Test);
   procedure Test_Park_Vehicle_Without_Room (T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage (
      T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_With_Vehicle_In_Garage (
      T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination (
      T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination_Full (
      T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination (
      T : in out Garage_Test);
   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle (
      T : in out Garage_Test);
   procedure Test_Leave_Without_Boarding (
      T : in out Garage_Test);
   procedure Test_Leave_After_Boarding (
      T : in out Garage_Test);
   procedure Test_Leave_Garage_Two_Travellers (
      T : in out Garage_Test);
   procedure Test_Leave_Garage_Is_Idempotent (
      T : in out Garage_Test);

   procedure Register_Tests (T : in out Garage_Test);
   overriding
   function Name (T : in Garage_Test) return AU.Message_String;

end Reactive.Infrastructure.Building.Parking_Manager.Garage.Tests;
