with AUnit.Assertions;
with Ada.Text_IO;

-- no mock needed: simple instance of dependency
with Active.Traveller.Vehicle.Bicycle.Mock;
with Active.Traveller.Vehicle.Bus.Mock;
with Active.Traveller.Vehicle.Private_Motor_Vehicle.Mock;
with Active.Traveller.Pedestrian.Mock;
with Active.Traveller.Vehicle.Bicycle.Utils;

with Reactive.District;
with Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;

package body
   Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing.Tests is

   package Ass renames AUnit.Assertions;
   package Traveller renames Active.Traveller;
   package Pedestrian_Mock renames Active.Traveller.Pedestrian.Mock;
   package Bicycle_Mock renames Active.Traveller.Vehicle.Bicycle.Mock;
   package Bus_Mock renames Active.Traveller.Vehicle.Bus.Mock;
   package PMV_Mock
      renames Active.Traveller.Vehicle.Private_Motor_Vehicle.Mock;
   package B_Utils renames Active.Traveller.Vehicle.Bicycle.Utils;
   package District renames Reactive.District;

   Bicycle_Utils : B_Utils.Reference;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T : in out Bicycle_Crossing_Test) is
      Bicycle_Ref      : Bicycle_Mock.Reference;
      Pedestrian_Ref   : Pedestrian_Mock.Reference;
   begin
      Zebra_Crossing.Tests.Set_Up
         (Zebra_Crossing.Tests.Zebra_Crossing_Test (T));

   -- privileged
      Bicycle_Ref := Bicycle_Mock.Create;
      Bicycle_Ref.Set_Return_Value_For_Get_Id (T.Privileged_Id);
      T.Privileged := Traveller.Reference (Bicycle_Ref);
   -- unprivileged
      Pedestrian_Ref := Pedestrian_Mock.Create;
      Pedestrian_Ref.Set_Id (T.Unprivileged_Id);
      T.Unprivileged := Traveller.Reference (Pedestrian_Ref);

      Bicycle_Utils := B_Utils.Get_Instance (T.District_Ref);
      T.Crossing := Zebra_Crossing.Reference (
         Bicycle_Crossing.Create (Stretch.Reference (T.Stretch_Ref),
                                  Bicycle_Utils));
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Bicycle_Has_Priority (T : in out Bicycle_Crossing_Test)
   is
      Bicycle_Ref : Bicycle_Mock.Reference;
      Traveller_Ref  : Active.Traveller.Reference;
      Bicycle_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority   : Boolean;
      Ped_Added      : Boolean;
      Ped_Removed    : Boolean;
   begin
      Bicycle_Ref := Bicycle_Mock.Create;
      Bicycle_Ref.Set_Return_Value_For_Get_Id (Bicycle_Id);
      Traveller_Ref := Active.Traveller.Reference (Bicycle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Ped_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (Bicycle_Id);
      Ass.Assert (Has_Priority, "Bicycles have no priority");
      T.District_Ref.Remove_Traveller (Bicycle_Id, Ped_Removed);
   end Test_Bicycle_Has_Priority;

   procedure Test_Pedestrian_Does_Not_Have_Priority (
      T : in out Bicycle_Crossing_Test)
   is
      Pedestrian_Ref   : Pedestrian_Mock.Reference;
      Traveller_Ref    : Active.Traveller.Reference;
      Pedestrian_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority     : Boolean;
      Ped_Added        : Boolean;
      Ped_Removed      : Boolean;
   begin
      Pedestrian_Ref := Pedestrian_Mock.Create;
      Pedestrian_Ref.Set_Id (Pedestrian_Id);
      Traveller_Ref := Active.Traveller.Reference (Pedestrian_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Ped_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (Pedestrian_Id);
      Ass.Assert (not Has_Priority, "Pedestrians have priority");
      T.District_Ref.Remove_Traveller (Pedestrian_Id, Ped_Removed);
   end Test_Pedestrian_Does_Not_Have_Priority;

   procedure Test_Bus_Does_Not_Have_Priority (
      T : in out Bicycle_Crossing_Test)
   is
      Bus_Ref       : Bus_Mock.Reference;
      Traveller_Ref : Active.Traveller.Reference;
      Bus_Id        : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority  : Boolean;
      Bus_Added     : Boolean;
      Bus_Removed   : Boolean;
   begin
      Bus_Ref := Bus_Mock.Create;
      Bus_Ref.Set_Return_Value_For_Get_Id (Bus_Id);
      Traveller_Ref  := Active.Traveller.Reference (Bus_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Bus_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (Bus_Id);
      Ass.Assert (not Has_Priority, "Buss have priority");
      T.District_Ref.Remove_Traveller (Bus_Id, Bus_Removed);
   end Test_Bus_Does_Not_Have_Priority;

   procedure Test_PMV_Does_Not_Have_Priority (
      T : in out Bicycle_Crossing_Test)
   is
      PMV_Ref       : PMV_Mock.Reference;
      Traveller_Ref : Active.Traveller.Reference;
      PMV_Id        : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority  : Boolean;
      PMV_Added     : Boolean;
      PMV_Removed   : Boolean;
   begin
      PMV_Ref := PMV_Mock.Create;
      PMV_Ref.Set_Return_Value_For_Get_Id (PMV_Id);
      Traveller_Ref  := Active.Traveller.Reference (PMV_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, PMV_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (PMV_Id);
      Ass.Assert (not Has_Priority, "PMVs have priority");
      T.District_Ref.Remove_Traveller (PMV_Id, PMV_Removed);
   end Test_PMV_Does_Not_Have_Priority;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Bicycle_Has_Priority_Wrapper (
      T : in out Bicycle_Crossing_Test'Class)
   is
   begin
      Test_Bicycle_Has_Priority (T);
   end Test_Bicycle_Has_Priority_Wrapper;

   procedure Test_Pedestrian_Does_Not_Have_Priority_Wrapper (
      T : in out Bicycle_Crossing_Test'Class)
   is
   begin
      Test_Pedestrian_Does_Not_Have_Priority (T);
   end Test_Pedestrian_Does_Not_Have_Priority_Wrapper;

   procedure Test_Bus_Does_Not_Have_Priority_Wrapper (
      T : in out Bicycle_Crossing_Test'Class)
   is
   begin
      Test_Bus_Does_Not_Have_Priority (T);
   end Test_Bus_Does_Not_Have_Priority_Wrapper;

   procedure Test_PMV_Does_Not_Have_Priority_Wrapper (
      T : in out Bicycle_Crossing_Test'Class)
   is
   begin
      Test_PMV_Does_Not_Have_Priority (T);
   end Test_PMV_Does_Not_Have_Priority_Wrapper;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T : in out Bicycle_Crossing_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Bicycle_Crossing_Test);
      use Register_Specific;
   begin

      Zebra_Crossing.Tests.Register_Tests
         (Zebra_Crossing.Tests.Zebra_Crossing_Test (T));

      Register_Wrapper
        (Test    => T,
         Routine => Test_Bicycle_Has_Priority_Wrapper'Access,
         Name    => "Tests a bicycle has priority");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Pedestrian_Does_Not_Have_Priority_Wrapper'Access,
         Name    => "Tests a pedestrian does not have priority");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Bus_Does_Not_Have_Priority_Wrapper'Access,
         Name    => "Tests a bus does not have priority");

      Register_Wrapper
        (Test    => T,
         Routine => Test_PMV_Does_Not_Have_Priority_Wrapper'Access,
         Name    => "Tests a private motor vehicle does not have priority");

   end Register_Tests;

   function Name(T : Bicycle_Crossing_Test) return AU.Message_String is
   begin
      return AU.Format ("Decoration.Bicycle_Crossing");
   end Name;

end Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing.Tests;
