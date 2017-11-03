with AUnit.Assertions;
with Ada.Text_IO;

-- no mock needed: simple instance of dependency
with Active.Traveller.Vehicle.Mock;
with Active.Traveller.Pedestrian.Mock;
with Active.Traveller.Pedestrian.Utils;

with Reactive.District;
with Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;

package body
   Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing.Tests is

   package Ass renames AUnit.Assertions;
   package Traveller renames Active.Traveller;
   package Pedestrian_Mock renames Active.Traveller.Pedestrian.Mock;
   package Vehicle_Mock renames Active.Traveller.Vehicle.Mock;
   package P_Utils renames Active.Traveller.Pedestrian.Utils;
   package District renames Reactive.District;

   Pedestrian_Utils : P_Utils.Reference;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T : in out Pedestrian_Crossing_Test) is
      Pedestrian_Ref : Pedestrian_Mock.Reference;
      Vehicle_Ref    : Vehicle_Mock.Reference;
   begin
      Zebra_Crossing.Tests.Set_Up (
         Zebra_Crossing.Tests.Zebra_Crossing_Test (T));

   -- privileged
      Pedestrian_Ref := Pedestrian_Mock.Create;
      Pedestrian_Ref.Set_Id (T.Privileged_Id);
      T.Privileged := Traveller.Reference (Pedestrian_Ref);
   -- unprivileged
      Vehicle_Ref := Vehicle_Mock.Create;
      Vehicle_Ref.Set_Return_Value_For_Get_Id (T.Unprivileged_Id);
      T.Unprivileged := Traveller.Reference (Vehicle_Ref);

      Pedestrian_Utils := P_Utils.Get_Instance (T.District_Ref);
      T.Crossing := Zebra_Crossing.Reference (
         Pedestrian_Crossing.Create (Stretch.Reference (T.Stretch_Ref),
                                     Pedestrian_Utils));
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Pedestrian_Has_Priority (T : in out Pedestrian_Crossing_Test)
   is
      Pedestrian_Ref : Pedestrian_Mock.Reference;
      Traveller_Ref  : Active.Traveller.Reference;
      Pedestrian_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority   : Boolean;
      Ped_Added      : Boolean;
      Ped_Removed    : Boolean;
   begin
      Pedestrian_Ref := Pedestrian_Mock.Create;
      Pedestrian_Ref.Set_Id (Pedestrian_Id);
      Traveller_Ref  := Active.Traveller.Reference (Pedestrian_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Ped_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (Pedestrian_Id);
      Ass.Assert (Has_Priority, "A vehicle does not have priority");
      T.District_Ref.Remove_Traveller (Pedestrian_Id, Ped_Removed);
   end Test_Pedestrian_Has_Priority;

   procedure Test_Vehicle_Does_Not_Have_Priority (
      T : in out Pedestrian_Crossing_Test)
   is
      Vehicle_Ref   : Vehicle_Mock.Reference;
      Traveller_Ref : Active.Traveller.Reference;
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Has_Priority  : Boolean;
      Veh_Added     : Boolean;
      Veh_Removed   : Boolean;
   begin
      Vehicle_Ref := Vehicle_Mock.Create;
      Vehicle_Ref.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Traveller_Ref  := Active.Traveller.Reference (Vehicle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Veh_Added);
      Has_Priority := T.Crossing.Has_Priority_Over (Vehicle_Id);
      Ass.Assert (not Has_Priority, "A vehicle has priority");
      T.District_Ref.Remove_Traveller (Vehicle_Id, Veh_Removed);
   end Test_Vehicle_Does_Not_Have_Priority;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Pedestrian_Has_Priority_Wrapper (
      T : in out Pedestrian_Crossing_Test'Class)
   is
   begin
      Test_Pedestrian_Has_Priority (T);
   end Test_Pedestrian_Has_Priority_Wrapper;

   procedure Test_Vehicle_Does_Not_Have_Priority_Wrapper (
      T : in out Pedestrian_Crossing_Test'Class)
   is
   begin
      Test_Vehicle_Does_Not_Have_Priority (T);
   end Test_Vehicle_Does_Not_Have_Priority_Wrapper;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T : in out Pedestrian_Crossing_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Pedestrian_Crossing_Test);
      use Register_Specific;
   begin

      Zebra_Crossing.Tests.Register_Tests
         (Zebra_Crossing.Tests.Zebra_Crossing_Test (T));

      Register_Wrapper
        (Test    => T,
         Routine => Test_Pedestrian_Has_Priority_Wrapper'Access,
         Name    => "Tests a pedestrian has priority");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Vehicle_Does_Not_Have_Priority_Wrapper'Access,
         Name    => "Tests a vehicle does not have priority");

   end Register_Tests;

   function Name(T : Pedestrian_Crossing_Test) return AU.Message_String is
   begin
      return AU.Format ("Decoration.Pedestrian_Crossing");
   end Name;

end Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing.Tests;
