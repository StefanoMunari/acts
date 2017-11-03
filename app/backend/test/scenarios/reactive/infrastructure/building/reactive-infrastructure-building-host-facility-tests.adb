with AUnit.Assertions;

with Active.People_Carrier.Utils.Mock;
with Active.Traveller;
with Active.Traveller.Mock;
with Active.Traveller.Pedestrian.Mock;
with Active.Traveller.Utils.Mock;
with Active.Traveller.Vehicle.Mock;

with Interface_Layer.Remote.Stub.Mock;
with Interface_Layer.Remote.Query_Builder.Mock;
with Interface_Layer.Wrappers.Application.Mock_Factory;

with Reactive.District.Mock;
with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Building.Parking_Manager.Garage.Mock;
with Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock;

with Shared.Agent_Id_List;
with Shared.Infra_Id_List;

package body Reactive.Infrastructure.Building.Host.Facility.Tests is

   package Ass renames AUnit.Assertions;
-- active
   package Traveller_Pkg renames Active.Traveller;
   package Traveller_Mock renames Active.Traveller.Mock;
   package Pedestrian_Mock renames Active.Traveller.Pedestrian.Mock;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;
   package Vehicle_Mock renames Active.Traveller.Vehicle.Mock;
   package People_Carrier_Mock renames Active.People_Carrier.Utils.Mock;
-- interface layer packages
   package Remote_Stub_Mock
      renames Interface_Layer.Remote.Stub.Mock;
   package Remote_Query_Builder_Mock
      renames Interface_Layer.Remote.Query_Builder.Mock;
-- reactive
   package District_Mock_Pkg renames Reactive.District.Mock;
   package Host_Utils_Mock
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Parking_Manager_Mock
      renames Reactive.Infrastructure.Building.Parking_Manager.Garage.Mock;
   package Use_Carrier_Strategy_Mock renames
   Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;

   use Active.Space_Master.Next_Action_Type;
   use Agent;
   use Reactive.Stretch_Type_Package;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   -- TODO: Use proper id
   procedure Set_Up (T : in out Facility_Test) is
   begin
      T.Host_Utils  :=
         Host.Utils.Reference (Host_Utils_Mock.Create);
      T.Id          := 44;
      T.Parking_Ref := Parking_Manager_Mock.Create;
      T.Traveller_Utils    :=
         Active.Traveller.Utils.Reference (Traveller_Utils_Mock_Pkg.Create);
      T.PC_Utils    :=
         Active.People_Carrier.Utils.Reference (People_Carrier_Mock.Create);
      T.UC_Strategy := Use_Carrier_Strategy_Mock.Create;
      T.Stub            :=
         Remote_Stub.Reference (Remote_Stub_Mock.Create);
      T.Query_Builder   :=
         Remote_Query_Builder.Reference (Remote_Query_Builder_Mock.Create);
      T.Wrapper_Factory :=
         App_Wrapper.Abstract_Factory.Reference (
            App_Wrapper.Mock_Factory.Create);
      T.District        :=
         District_Pkg.Reference (District_Mock_Pkg.Create);
      T.Facility_Ref :=
         Facility.Create (
            Id                   => T.Id,
            Parking_Ref          => T.Parking_Ref,
            Host_Utils_Ref       => T.Host_Utils,
            PC_Utils             => T.PC_Utils,
            Traveller_Utils      => T.Traveller_Utils,
            Use_Carrier_Strategy => T.UC_Strategy,
            Stub                 => T.Stub,
            Query_Builder        => T.Query_Builder,
            District             => T.District,
            Wrapper_Factory      => T.Wrapper_Factory);
   end Set_Up;

   procedure Tear_Down (T: in out Facility_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Pedestrian_Stops_Over (T : in out Facility_Test) is
      Pedestrian : aliased Pedestrian_Mock.Object'Class
         := Pedestrian_Mock.Create.all;
      Pedestrian_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Travellers : Agent_Id_List.List;
      Host_Utils_Mock_Ref : Host_Utils_Mock.Reference
         := Host_Utils_Mock.Reference (T.Host_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
   begin
      Pedestrian.Set_Id (Pedestrian_Id);
      Travellers.Append (Pedestrian_Id);
      Host_Utils_Mock_Ref.Set_Travellers_List_For_Stop_Over (Travellers);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Pedestrian_Id, False);

      T.Facility_Ref.Stop_Over (Travellers);
      Ass.Assert (
         T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Pedestrian_Id),
          "The pedestrian did not stop in the facility");
   end Test_Pedestrian_Stops_Over;

   procedure Test_Vehicle_Stops_Over (T : in out Facility_Test) is
      Vehicle : aliased Vehicle_Mock.Object'Class
         := Vehicle_Mock.Create.all;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Travellers : Agent_Id_List.List;
      Host_Utils_Mock_Ref : Host_Utils_Mock.Reference
         := Host_Utils_Mock.Reference (T.Host_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (802);
      Passenger3_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (803);
      Passenger4_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (804);
   begin
      Vehicle.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Travellers.Append (Vehicle_Id);
      Travellers.Append (Passenger1_Id);
      Travellers.Append (Passenger2_Id);
      Travellers.Append (Passenger3_Id);
      Travellers.Append (Passenger4_Id);
      Host_Utils_Mock_Ref.Set_Travellers_List_For_Stop_Over (Travellers);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Passenger1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Passenger2_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Passenger3_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Passenger4_Id, False);

      T.Facility_Ref.Stop_Over (Travellers);
      Ass.Assert (
         T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Passenger1_Id),
          "The first passenger did not stop in the facility");
      Ass.Assert (
         T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Passenger2_Id),
          "The second passenger did not stop in the facility");
      Ass.Assert (
         T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Passenger3_Id),
          "The third passenger did not stop in the facility");
      Ass.Assert (
         T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Passenger4_Id),
          "The fourth passenger did not stop in the facility");
      Ass.Assert (
         not T.Facility_Ref.Hosted_Travellers.Contains_Traveller (Vehicle_Id),
          "The vehicle was added as a person in the facility");
   end Test_Vehicle_Stops_Over;

   procedure Test_Exit_Building_Without_Vehicle (T : in out Facility_Test) is
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref  : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy  : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Traveller    : Traveller_Mock.Reference := Traveller_Mock.Create;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (810);
      Added        : Boolean;
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (811);
      Travellers   : Agent_Id_List.List;
      Exiting_Id   : Agent.Agent_Id;
      Next         : Next_Action;
   begin
      Travellers.Append (Traveller_Id);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller_Id, True);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (False);
      T.Facility_Ref.Stop_Over (Travellers);
      Traveller.Set_Id (Traveller_Id);
      T.District.Add_Traveller (Traveller_Pkg.Reference (Traveller), Added);

      Next := T.Facility_Ref.Exit_Building (Traveller_Id, Exiting_Id);
      Ass.Assert (Next = DEFER,
         "Traveller did not exit");
      Ass.Assert (Exiting_Id = Traveller_Id,
         "Traveller did not exit");
   end Test_Exit_Building_Without_Vehicle;

   procedure Test_Exit_Building_With_Vehicle (T : in out Facility_Test) is
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Traveller    : Traveller_Mock.Reference := Traveller_Mock.Create;
      Vehicle      : Traveller_Mock.Reference := Traveller_Mock.Create;
      Added        : Boolean;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (810);
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (811);
      Travellers   : Agent_Id_List.List;
      Exiting_Id   : Agent.Agent_Id;
      Next         : Next_Action;
   begin
      Travellers.Append (Traveller_Id);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller_Id, True);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (True);
      T.Facility_Ref.Stop_Over (Travellers);
      Traveller.Set_Id (Traveller_Id);
      Vehicle.Set_Id (Vehicle_Id);
      T.District.Add_Traveller (Traveller_Pkg.Reference (Traveller), Added);
      T.District.Add_Traveller (Traveller_Pkg.Reference (Vehicle), Added);

      Next := T.Facility_Ref.Exit_Building (Traveller_Id, Exiting_Id);
      Ass.Assert (Next = DEFER,
         "Traveller did not exit");
      Ass.Assert (Exiting_Id = Vehicle_Id,
         "Traveller did not exit");
   end Test_Exit_Building_With_Vehicle;

   procedure Test_Exit_Building_Not_Full_Without_Vehicle (
      T : in out Facility_Test)
   is
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Traveller    : Traveller_Mock.Reference := Traveller_Mock.Create;
      Added        : Boolean;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (430);
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (431);
      Travellers   : Agent_Id_List.List;
      Exiting_Id   : Agent.Agent_Id;
      Next         : Next_Action;
   begin
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, False);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller_Id, True);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (False);
      Travellers.Append (Traveller_Id);
      T.Facility_Ref.Stop_Over (Travellers);
      Traveller.Set_Id (Traveller_Id);
      T.District.Add_Traveller (Traveller_Pkg.Reference (Traveller), Added);
      Parking_Ref.Set_Values_For_Is_A_Driver (True);

      Next := T.Facility_Ref.Exit_Building (Traveller_Id, Exiting_Id);

      Ass.Assert (
         Next = DEFER,
         "Traveller did not exit the facility");
      Ass.Assert (
         Exiting_Id = Traveller_Id,
         "Something else than the traveller is exiting");
   end Test_Exit_Building_Not_Full_Without_Vehicle;

   procedure Test_Exit_Building_Not_Full_With_Vehicle (
      T : in out Facility_Test)
   is
      Traveller_Utils_Ref : Traveller_Utils_Mock_Pkg.Reference
         := Traveller_Utils_Mock_Pkg.Reference (T.Traveller_Utils);
      PC_Utils_Ref        : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref         : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy         : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Stub_Ref            : Remote_Stub_Mock.Reference
         := Remote_Stub_Mock.Reference (T.Stub);
      Traveller    : Traveller_Mock.Reference := Traveller_Mock.Create;
      Added        : Boolean;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (820);
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (821);
      Dest         : Slice.Map := Slice.Empty_Map;
      Dest_List    : Infra_Id_List.List;
      Travellers   : Agent_Id_List.List;
      Exiting_Id   : Agent.Agent_Id;
      Next         : Next_Action;
   begin
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, False);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (True);
      Travellers.Append (Traveller_Id);
      T.Facility_Ref.Stop_Over (Travellers);
      Stub_Ref.Set_Return_Value_For_Query (True);
      Traveller.Set_Id (Traveller_Id);
      T.District.Add_Traveller (Traveller_Pkg.Reference (Traveller), Added);
      Parking_Ref.Set_Values_For_Is_A_Driver (True);

      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (1);
      Dest.Include (FOOT, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (2);
      Dest.Include (BIKE, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (3);
      Dest.Include (ROAD, Dest_List);
      Traveller_Utils_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller_Id, Dest);

      Next := T.Facility_Ref.Exit_Building (Traveller_Id, Exiting_Id);

      Ass.Assert (
         Next = DO_NOT_DEFER,
         "Exit building has not been delegated to a pair of callbacks");
      Ass.Assert (
         Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");

   -- Try again to exit: at this second call it will exit
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Vehicle_Id, True);
      Next := T.Facility_Ref.Exit_Building (Traveller_Id, Exiting_Id);

      Ass.Assert (Next = DEFER,
         "Traveller did not exit the facility");
      Ass.Assert (Exiting_Id = Vehicle_Id,
         "Something else than a vehicle is exiting");
   end Test_Exit_Building_Not_Full_With_Vehicle;

   procedure Test_Exit_Building_Two_Passengers_With_Vehicle (
      T : in out Facility_Test)
   is
      Traveller_Utils_Ref : Traveller_Utils_Mock_Pkg.Reference
         := Traveller_Utils_Mock_Pkg.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Stub_Ref            : Remote_Stub_Mock.Reference
         := Remote_Stub_Mock.Reference (T.Stub);
      Traveller : aliased Traveller_Mock.Object'Class
         := Traveller_Mock.Create.all;
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (830);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (831);
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (832);
      Travellers    : Agent_Id_List.List;
      Dest         : Slice.Map := Slice.Empty_Map;
      Dest_List    : Infra_Id_List.List;
      Exiting_Id    : Agent.Agent_Id;
      Next_1        : Next_Action;
      Next_2        : Next_Action;
   begin
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller2_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, False);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller1_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller2_Id, False);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (True);
      Travellers.Append (Traveller1_Id);
      Travellers.Append (Traveller2_Id);
      T.Facility_Ref.Stop_Over (Travellers);
      Stub_Ref.Set_Return_Value_For_Query (True);
      Parking_Ref.Set_Values_For_Is_A_Driver (True);

      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (1);
      Dest.Include (FOOT, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (2);
      Dest.Include (BIKE, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (3);
      Dest.Include (ROAD, Dest_List);
      Traveller_Utils_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Dest);
      Traveller_Utils_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Dest);

      Next_1 := T.Facility_Ref.Exit_Building (Traveller1_Id, Exiting_Id);
      Ass.Assert (
         Next_1 = DO_NOT_DEFER,
         "Traveller is not waiting to exit facility");
      Ass.Assert (
         Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");

   -- make carrier become full (2 seats)
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, True);

      Next_2 := T.Facility_Ref.Exit_Building (Traveller2_Id, Exiting_Id);
      Ass.Assert (Next_2 = DO_NOT_DEFER,
         "Traveller will exit facility");
      Ass.Assert (Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");
   end Test_Exit_Building_Two_Passengers_With_Vehicle;

   procedure Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle (
      T : in out Facility_Test)
   is
      Traveller_Utils_Ref : Traveller_Utils_Mock_Pkg.Reference
         := Traveller_Utils_Mock_Pkg.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Parking_Ref : Parking_Manager_Mock.Reference
         := Parking_Manager_Mock.Reference (T.Parking_Ref);
      UC_Strategy : Use_Carrier_Strategy_Mock.Reference
         := Use_Carrier_Strategy_Mock.Reference (T.UC_Strategy);
      Stub_Ref            : Remote_Stub_Mock.Reference
         := Remote_Stub_Mock.Reference (T.Stub);
      Traveller : aliased Traveller_Mock.Object'Class
         := Traveller_Mock.Create.all;
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (830);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (831);
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (832);
      Travellers    : Agent_Id_List.List;
      Dest          : Slice.Map := Slice.Empty_Map;
      Dest_List     : Infra_Id_List.List;
      Exiting_Id    : Agent.Agent_Id;
      Next_1        : Next_Action;
      Next_2        : Next_Action;
   begin
      Travellers.Append (Traveller1_Id);
      Travellers.Append (Traveller2_Id);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Traveller2_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, False);
      Parking_Ref.Set_Values_For_Ask_For_Vehicle (Vehicle_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller1_Id, True);
      Parking_Ref.Set_Values_For_Leave_Parking (Traveller2_Id, False);
      UC_Strategy.Set_Return_Value_For_Use_Carrier_Or_Not (True);
      T.Facility_Ref.Stop_Over (Travellers);
      Parking_Ref.Set_Values_For_Is_A_Driver (True);
      Stub_Ref.Set_Return_Value_For_Query (True);

      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (1);
      Dest.Include (FOOT, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (2);
      Dest.Include (BIKE, Dest_List);
      Dest_List := Infra_Id_List.Empty_List;
      Dest_List.Append (3);
      Dest.Include (ROAD, Dest_List);
      Traveller_Utils_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Dest);
      Traveller_Utils_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Dest);

      Next_1 := T.Facility_Ref.Exit_Building (Traveller1_Id, Exiting_Id);
      Ass.Assert (Next_1 = DO_NOT_DEFER,
         "Traveller 1 is not waiting to exit facility");
      Ass.Assert (Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");

   -- make carrier not already full with the second traveller (>2 seats)

      Next_2 := T.Facility_Ref.Exit_Building (Traveller2_Id, Exiting_Id);
      Ass.Assert (Next_2 = DO_NOT_DEFER,
         "Traveller 2 is not waiting to exit facility");
      Ass.Assert (Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");

      Next_2 := T.Facility_Ref.Exit_Building (Traveller2_Id, Exiting_Id);
      Ass.Assert (Next_2 = DO_NOT_DEFER,
         "Traveller will exit facility");
      Ass.Assert (Exiting_Id /= Vehicle_Id,
         "The vehicle is exiting");

      Next_1 := T.Facility_Ref.Exit_Building (Traveller1_Id, Exiting_Id);
      Ass.Assert (Next_1 = DEFER,
         "Traveller is not exiting facility");
      Ass.Assert (Exiting_Id = Vehicle_Id,
         "The vehicle is not exiting");
   end Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Pedestrian_Stops_Over_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Pedestrian_Stops_Over (T);
   end Test_Pedestrian_Stops_Over_Wrapper;

   procedure Test_Vehicle_Stops_Over_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Vehicle_Stops_Over (T);
   end Test_Vehicle_Stops_Over_Wrapper;

   procedure Test_Exit_Building_Without_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Without_Vehicle (T);
   end Test_Exit_Building_Without_Vehicle_Wrapper;

   procedure Test_Exit_Building_With_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Without_Vehicle (T);
   end Test_Exit_Building_With_Vehicle_Wrapper;

   procedure Test_Exit_Building_Not_Full_Without_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Not_Full_Without_Vehicle (T);
   end Test_Exit_Building_Not_Full_Without_Vehicle_Wrapper;

   procedure Test_Exit_Building_Not_Full_With_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Not_Full_With_Vehicle (T);
   end Test_Exit_Building_Not_Full_With_Vehicle_Wrapper;

   procedure Test_Exit_Building_Two_Passengers_With_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Two_Passengers_With_Vehicle (T);
   end Test_Exit_Building_Two_Passengers_With_Vehicle_Wrapper;

   procedure Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle_Wrapper (
      T : in out Facility_Test'Class)
   is
   begin
      Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle (T);
   end Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Facility_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Facility_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Pedestrian_Stops_Over_Wrapper'Access,
         Name    => "Tests a pedestrian stopping in a facility");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Vehicle_Stops_Over_Wrapper'Access,
         Name    => "Tests a vehicle stopping in a facility");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Without_Vehicle_Wrapper'Access,
         Name    => "Tests a traveller can exit a building without a vehicle");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_With_Vehicle_Wrapper'Access,
         Name    => "Tests a traveller can exit a building with a vehicle");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Not_Full_Without_Vehicle_Wrapper'Access,
         Name    => "Tests a traveller can exit a building with non-full"
                  & " vehicle without boarding it");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Not_Full_With_Vehicle_Wrapper'Access,
         Name    => "Tests a traveller can exit a building with non-full"
                  & " vehicle after boarding it");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Exit_Building_Two_Passengers_With_Vehicle_Wrapper'Access,
         Name    => "Tests two passengers can exit together a facility");

   -- DEPRECATED: Not meaningful at the time being
      --Register_Wrapper (
      --   Test    => T,
      --   Routine =>
      --      Test_Exit_Building_Two_Passengers_With_Non_Full_Vehicle_Wrapper'Access,
      --   Name    => "Tests two passengers can exit together a facility in a"
      --            & " non full vehicle");

   end Register_Tests;

   function Name(T : Facility_Test) return AU.Message_String is
   begin
      return AU.Format ("Reactive.Infrastructure.Building.Host.Facility");
   end Name;

end Reactive.Infrastructure.Building.Host.Facility.Tests;
