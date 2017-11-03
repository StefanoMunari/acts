with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Travel.Mock;

with Passive.Road_Sign.Bus_Stop.Mock;

with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;

package body Active.Traveller.Pedestrian.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Road_Sign_Pkg renames Passive.Road_Sign;
   package Bus_Stop_Pkg renames Passive.Road_Sign.Bus_Stop;
   package Bus_Stop_Mock_Pkg renames Bus_Stop_Pkg.Mock;
   package Infrastructure_Utils renames Reactive.Infrastructure.Utils.Mock;
   package Street_Utils renames Reactive.Infrastructure.Street.Utils.Mock;
   package Intersection_Utils
      renames Reactive.Infrastructure.Intersection.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Pedestrian_Id : Agent.Agent_Id;
   Maximum_Speed, Route_Source_Id, Route_Destination_Id : Natural;

   procedure Set_Up (T: in out Pedestrian_Test) is
   begin
      Pedestrian_Id := Agent.Create_Id_From_Natural (2);
      Maximum_Speed := 20;
      Route_Source_Id := 2;
      Route_Destination_Id := 5;
      T.Infrastructure_Utils := Infrastructure_Utils.Create;
      T.Street_Utils := Street_Utils.Create;
      T.Intersection_Utils := Intersection_Utils.Create;
      T.Travel_Ref := Active.Travel.Mock.Create;
      T.Strategy_Ref := Strategy_Mock_Pkg.Create;
      T.SSD_Utils := Stretch_Sign_Decorator_Utils_Mock.Create;

      T.Traveller
         := Traveller.Reference (
            Pedestrian.Create (Id                   => Pedestrian_Id,
                               Maximum_Speed        => Maximum_Speed,
                               Travel_Ref           => T.Travel_Ref,
                               Infrastructure_Utils => T.Infrastructure_Utils,
                               Street_Utils         => T.Street_Utils,
                               Strategy_Ref         => T.Strategy_Ref,
                               Intersection_Utils   => T.Intersection_Utils,
                               SSD_Utils            => T.SSD_Utils)
            );
      T.Pedestrian_Ref := Pedestrian.Reference (T.Traveller);
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Is_Affected_By_Traffic_Lights (T: in out Pedestrian_Test) is
   begin
      Ass.Assert (
        not T.Pedestrian_Ref.Is_Affected_By_Traffic_Lights,
        "A pedestrian is affected by traffic lights");
   end Test_Is_Affected_By_Traffic_Lights;

   procedure Test_On_Bus_Stop (T: in out Pedestrian_Test)
   is
      Bus_Stop_Mock : Bus_Stop_Mock_Pkg.Reference
         := Bus_Stop_Mock_Pkg.Create;
   begin
      T.Travel_Ref.Set_Return_Value_For_Get_Current_Step_Id (42);
      T.SSD_Utils.Set_Return_Value_For_Get_Sign (
         Road_Sign_Pkg.Reference (Bus_Stop_Mock));
      T.SSD_Utils.Set_Return_Value_For_Is_A_Stretch_Sign_Decorator (42, True);
      T.Strategy_Ref.Set_Return_Value_For_Wait_For_Bus_Or_Not (True);

      T.Pedestrian_Ref.On_Bus_Stop;
      Ass.Assert (
        T.Pedestrian_Ref.Is_Waiting,
        "A pedestrian is not waiting for the bus");
   end Test_On_Bus_Stop;

   procedure Test_Stop_Waiting (T: in out Pedestrian_Test) is
   begin
      T.Pedestrian_Ref.Is_Waiting := True;
      T.Pedestrian_Ref.Stop_Waiting;
      Ass.Assert (
        not T.Pedestrian_Ref.Is_Waiting,
        "A pedestrian is still waiting");

      T.Pedestrian_Ref.Is_Waiting := False;
      T.Pedestrian_Ref.Stop_Waiting;
      Ass.Assert (
        not T.Pedestrian_Ref.Is_Waiting,
        "A pedestrian is now waiting");
   end Test_Stop_Waiting;


   -----------------------------------------------------
   --                  WRAPPERS
   -----------------------------------------------------
   procedure Test_Is_Affected_By_Traffic_Lights_Wrapper (
      T : in out Pedestrian_Test'Class) is
   begin
      Test_Is_Affected_By_Traffic_Lights (T);
   end Test_Is_Affected_By_Traffic_Lights_Wrapper;

   procedure Test_On_Bus_Stop_Wrapper (
      T : in out Pedestrian_Test'Class) is
   begin
      Test_On_Bus_Stop (T);
   end Test_On_Bus_Stop_Wrapper;

   procedure Test_Stop_Waiting_Wrapper (
      T : in out Pedestrian_Test'Class) is
   begin
      Test_Stop_Waiting (T);
   end Test_Stop_Waiting_Wrapper;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Pedestrian_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Pedestrian_Test);
      use Register_Specific;
   begin

      Traveller.Tests.Register_Tests (Traveller.Tests.Traveller_Test (T));

      Register_Wrapper (
         Test => T,
         Routine => Test_Is_Affected_By_Traffic_Lights_Wrapper'Access,
         Name    =>
            "Test a pedestrian isn't effectively affected by traffic lights");

      Register_Wrapper (
         Test => T,
         Routine => Test_On_Bus_Stop_Wrapper'Access,
         Name    =>
            "Test a pedestrian start waiting for the bus on a bus stop");

      Register_Wrapper (
         Test => T,
         Routine => Test_Stop_Waiting_Wrapper'Access,
         Name    => "Test a pedestrian can stop waiting");

   end Register_Tests;

   function Name(T: Pedestrian_Test) return AU.Message_String is
   begin
      return AU.Format ("Pedestrian");
   end Name;
end Active.Traveller.Pedestrian.Tests;
