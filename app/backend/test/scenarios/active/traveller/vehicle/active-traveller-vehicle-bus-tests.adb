with AUnit.Assertions;

with Active.Agent;
with Active.Travel.Mock;
with Active.Traveller.Pedestrian.Utils.Mock;
with Active.Traveller.Utils.Mock;

with Interface_Layer.Remote.Stub.Mock;
with Interface_Layer.Wrappers.Application.Mock_Factory;

with Passive.Road_Sign.Bus_Stop.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package body Active.Traveller.Vehicle.Bus.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;
   package Stub_Mock_Pkg
      renames Interface_Layer.Remote.Stub.Mock;
   package Wrapper_Factory_Mock_Pkg
      renames Interface_Layer.Wrappers.Application.Mock_Factory;
   package Bus_Stop_Mock_Pkg renames Passive.Road_Sign.Bus_Stop.Mock;
   package Host_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Infrastructure_Utils renames Reactive.Infrastructure.Utils.Mock;
   package Street_Utils renames Reactive.Infrastructure.Street.Utils.Mock;
   package Stretch_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Stretch.Utils.Mock;
   package Intersection_Utils
      renames Reactive.Infrastructure.Intersection.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Bus_Id : Agent.Agent_Id;
   Maximum_Speed, Route_Source_Id, Route_Destination_Id,
   Max_Passengers : Natural;
   Bus_Stop_1 : Infra_Id := 66;
   Bus_Stop_2 : Infra_Id := 67;
   Bus_Stop_3 : Infra_Id := 68;
   Route_Stop_1 : Infra_Id := 76;
   Route_Stop_2 : Infra_Id := 77;
   Route_Stop_3 : Infra_Id := 78;

   procedure Set_Up (T: in out Bus_Test) is
   begin
      Bus_Id := Agent.Create_Id_From_Natural (2);
      Maximum_Speed := 20;
      Max_Passengers := 6;
      Route_Source_Id := 2;
      Route_Destination_Id := 5;
      T.Bus_Stops.Append (Bus_Stop_1);
      T.Bus_Stops.Append (Bus_Stop_2);
      T.Bus_Stops.Append (Bus_Stop_3);
      T.Route_Stops.Append (Route_Stop_1);
      T.Route_Stops.Append (Route_Stop_2);
      T.Route_Stops.Append (Route_Stop_3);
      T.Infrastructure_Utils := Infrastructure_Utils.Create;
      T.Street_Utils := Street_Utils.Create;
      T.Stretch_Utils := Stretch_Utils_Mock_Pkg.Create;
      T.Intersection_Utils := Intersection_Utils.Create;
      T.Travel_Ref := Active.Travel.Mock.Create;
      T.Traveller_Utils := Traveller_Utils_Mock_Pkg.Create;
      T.Host_Utils := Host_Utils_Mock_Pkg.Create;
      T.SSD_Utils := SSD_Utils_Mock_Pkg.Create;
      T.Pedestrian_Utils := Pedestrian_Utils_Mock_Pkg.Create;
      T.Space_Master := Space_Master_Mock_Pkg.Create;

      T.Traveller := Traveller.Reference (
         Bus.Create (
            Id                   => Bus_Id,
            Maximum_Speed        => Maximum_Speed,
            Max_Passengers       => Max_Passengers,
            Travel_Ref           => T.Travel_Ref,
            Bus_Stops            => T.Bus_Stops,
            Route_Stops          => T.Route_Stops,
            Infrastructure_Utils => T.Infrastructure_Utils,
            Street_Utils         => T.Street_Utils,
            Stretch_Utils        => T.Stretch_Utils,
            Traveller_Utils      => T.Traveller_Utils,
            Host_Utils           => T.Host_Utils,
            SSD_Utils            => T.SSD_Utils,
            Stub                 => Stub_Mock_Pkg.Create,
            App_Wrapper_Factory  => Wrapper_Factory_Mock_Pkg.Create,
            Pedestrian_Utils     => T.Pedestrian_Utils,
            Space_Master_Ref     => T.Space_Master,
            Intersection_Utils   => T.Intersection_Utils));
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Is_Affected_By_Traffic_Lights (T: in out Bus_Test)
   is
      Bus_Ref : Bus.Reference := Bus.Reference(T.Traveller);
   begin
      Ass.Assert (
        Bus_Ref.Is_Affected_By_Traffic_Lights,
        "A bus is not affected by traffic lights");
   end Test_Is_Affected_By_Traffic_Lights;

   procedure Test_On_Bus_Stop (T : in out Bus_Test)
   is
      Bus_Ref       : Bus.Reference := Bus.Reference(T.Traveller);
      Bus_Stop_Ref  : Bus_Stop_Mock_Pkg.Reference :=
         Bus_Stop_Mock_Pkg.Create;
      Boarding_List : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Boarding1     : Agent.Agent_Id := Agent.Create_Id_From_Natural (130);
      Boarding2     : Agent.Agent_Id := Agent.Create_Id_From_Natural (131);
      Boarding3     : Agent.Agent_Id := Agent.Create_Id_From_Natural (132);
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (140);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (141);
      Passenger3_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (142);
   begin
      Bus_Ref.Set_Position (Route_Stop_1);
      T.SSD_Utils.Set_Return_Value_For_Get_Sign (
         Road_Sign.Reference (Bus_Stop_Ref));
      Boarding_List.Append (Boarding1);
      Boarding_List.Append (Boarding2);
      Boarding_List.Append (Boarding3);
      Bus_Ref.Passengers.Append (Passenger1_Id);
      Bus_Ref.Passengers.Append (Passenger2_Id);
      Bus_Ref.Passengers.Append (Passenger3_Id);
      T.Traveller_Utils.Set_Return_Value_For_Get_Next_Step (
         Passenger1_Id, Bus_Stop_1);
      T.Traveller_Utils.Set_Return_Value_For_Get_Next_Step (
         Passenger2_Id, Bus_Stop_3);
      T.Traveller_Utils.Set_Return_Value_For_Get_Next_Step (
         Passenger3_Id, Bus_Stop_1);
      Bus_Stop_Ref.Set_Return_Value_For_Get_Waiting_For_Bus (Boarding_List);
      T.Stretch_Utils.Set_Return_Value_For_Tread (True);
      T.Stretch_Utils.Set_Return_Value_For_Leave (True);

      Bus_Ref.On_Bus_Stop;

      Ass.Assert (
        T.Pedestrian_Utils.Get_Stop_Waiting_For_Id (Boarding1),
        "First passenger did not board the bus");

      Ass.Assert (
        T.Pedestrian_Utils.Get_Stop_Waiting_For_Id (Boarding2),
        "Second passenger did not board the bus");

      Ass.Assert (
        T.Pedestrian_Utils.Get_Stop_Waiting_For_Id (Boarding3),
        "Third passenger did not board the bus");

      Ass.Assert (
        not Bus_Ref.Passengers.Contains (Passenger1_Id),
        "Passenger 1 is still on the bus");

      Ass.Assert (
        Bus_Ref.Passengers.Contains (Passenger2_Id),
        "Passenger 2 is not on the bus");

      Ass.Assert (
        not Bus_Ref.Passengers.Contains (Passenger3_Id),
        "Passenger 3 is still on the bus");

      Ass.Assert (
        Bus_Ref.Passengers.Contains (Boarding1),
        "Boarder 1 did not board the bus");

      Ass.Assert (
        Bus_Ref.Passengers.Contains (Boarding2),
        "Boarder 2 did not board the bus");

      Ass.Assert (
        Bus_Ref.Passengers.Contains (Boarding3),
        "Boarder 3 did not board the bus");
   end Test_On_Bus_Stop;


   -----------------------------------------------------
   --                  WRAPPERS
   -----------------------------------------------------
   procedure Test_Is_Affected_By_Traffic_Lights_Wrapper (
      T : in out Bus_Test'Class) is
   begin
      Test_Is_Affected_By_Traffic_Lights (T);
   end Test_Is_Affected_By_Traffic_Lights_Wrapper;

   procedure Test_On_Bus_Stop_Wrapper (
      T : in out Bus_Test'Class) is
   begin
      Test_On_Bus_Stop (T);
   end Test_On_Bus_Stop_Wrapper;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Bus_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Bus_Test);
      use Register_Specific;
   begin

      Vehicle.Tests.Register_Tests_Without_Travel_Tests (
         Vehicle.Tests.Vehicle_Test(T));

      Register_Wrapper (
         Test => T,
         Routine => Test_Is_Affected_By_Traffic_Lights_Wrapper'Access,
         Name    =>
            "Test a bus is effectively affected by traffic lights");

      Register_Wrapper (
         Test => T,
         Routine => Test_On_Bus_Stop_Wrapper'Access,
         Name    => "Test a bus land passengers and board some on a bus stop");

   end Register_Tests;

   function Name(T: Bus_Test) return AU.Message_String is
   begin
      return AU.Format ("Bus");
   end Name;
end Active.Traveller.Vehicle.Bus.Tests;
