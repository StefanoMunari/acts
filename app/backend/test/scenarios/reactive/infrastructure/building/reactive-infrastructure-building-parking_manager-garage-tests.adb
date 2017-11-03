with AUnit.Assertions;

with Active.People_Carrier.Utils.Mock;
with Active.Traveller.Utils.Mock;

with Shared.Agent_Id_List;
with Shared.Infra_Id_List;
with Shared.Slice;

package body Reactive.Infrastructure.Building.Parking_Manager.Garage.Tests is

   package Ass renames AUnit.Assertions;
   -- active packages
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package People_Carrier_Mock renames Active.People_Carrier.Utils.Mock;
   -- shared packages
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Slice renames Shared.Slice;
   -- use clauses
   use Agent;
   use Infra_Id_List;
   use Reactive.Stretch_Type_Package;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T : in out Garage_Test) is
   begin
      T.Traveller_Utils :=
         Active.Traveller.Utils.Reference (Traveller_Utils_Mock.Create);
      T.PC_Utils        :=
         Active.People_Carrier.Utils.Reference (People_Carrier_Mock.Create);
      T.Garage_Ref :=
         Garage.Create (T.Traveller_Utils, T.PC_Utils);
   end Set_Up;

   procedure Tear_Down (T: in out Garage_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Park_Vehicle_With_Room (T : in out Garage_Test) is
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (750);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
   begin
      T.Garage_Ref.Set_Size (1);
      T.Garage_Ref.Park_Vehicle (Vehicle_Id);

      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The vehicle was not parked in the garage");
   end Test_Park_Vehicle_With_Room;

   procedure Test_Park_Vehicle_Without_Room (T : in out Garage_Test) is
      Vehicle1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (750);
      Vehicle2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (751);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
   begin
      T.Garage_Ref.Set_Size (1);

      T.Garage_Ref.Park_Vehicle (Vehicle1_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle1_Id),
         "The first vehicle was not parked in the garage");

      T.Garage_Ref.Park_Vehicle (Vehicle2_Id);
      Ass.Assert (
         not T.Garage_Ref.Parked_Vehicles.Contains (Vehicle2_Id),
         "The second vehicle was parked in the garage");
   end Test_Park_Vehicle_Without_Room;

   procedure Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage (
      T : in out Garage_Test)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result_Id : Agent.Agent_Id;
      Boarded   : Boolean;
   begin
      Destination_List.Append (Destination_Id);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);

      T.Garage_Ref.Ask_For_Vehicle (Traveller_Id, Result_Id, Boarded);

      Ass.Assert (not Boarded, "The traveller did not board any vehicle");
   end Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage;

   procedure Test_Ask_For_Vehicle_With_Vehicle_In_Garage (
      T : in out Garage_Test)
   is
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      --Stub_Mock_Ref : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result_Id : Agent.Agent_Id;
      Boarded   : Boolean;
   begin
      T.Garage_Ref.Set_Size (1);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);
      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller_Id, Result_Id, Boarded);

      Ass.Assert (Vehicle_Id = Result_Id,
                 "The traveller did not board any vehicle");
      Ass.Assert (Boarded, "The traveller did not board any vehicle");
      Ass.Assert (
         not T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The vehicle is still present in the garage");
   end Test_Ask_For_Vehicle_With_Vehicle_In_Garage;

   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination (
      T : in out Garage_Test)
   is
      Vehicle1_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Vehicle2_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (469);
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (443);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result1_Id : Agent.Agent_Id;
      Result2_Id : Agent.Agent_Id;
      Boarded   : Boolean;
   begin
      T.Garage_Ref.Set_Size (2);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);

      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle2_Id, False);
      PC_Utils_Ref.Set_Value_For_Board (Traveller1_Id, True);
      PC_Utils_Ref.Set_Value_For_Board (Traveller2_Id, True);

      T.Garage_Ref.Park_Vehicle (Vehicle1_Id);
      T.Garage_Ref.Park_Vehicle (Vehicle2_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle1_Id),
         "The first vehicle was not parked in the garage");
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle2_Id),
         "The second vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller1_Id, Result1_Id, Boarded);

      Ass.Assert (Vehicle1_Id = Result1_Id or Vehicle2_Id = Result1_Id,
                 "The first traveller did not board any vehicle");
      Ass.Assert (Boarded, "The first traveller did not board any vehicle");

   -- simulate success for Book_Parking callback
      T.Garage_Ref.Put_Leaving_Vehicle (
         Vehicle_Id => Result1_Id,
         Driver_Id  => Traveller1_Id);
      T.Garage_Ref.Remove_Pending_Vehicle (Result1_Id, Traveller1_Id);

      T.Garage_Ref.Ask_For_Vehicle (Traveller2_Id, Result2_Id, Boarded);

      Ass.Assert (Result1_Id = Result2_Id,
                 "The second traveller did not board any vehicle");
      Ass.Assert (Boarded, "The second traveller did not board any vehicle");
   end Test_Ask_For_Vehicle_Two_People_Same_Destination;

   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination_Full (
      T : in out Garage_Test)
   is
      Vehicle1_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Vehicle2_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (469);
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (443);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      --Stub_Mock_Ref : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result1_Id : Agent.Agent_Id;
      Result2_Id : Agent.Agent_Id;
      Boarded   : Boolean;
   begin
      T.Garage_Ref.Set_Size (2);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);

      T.Garage_Ref.Park_Vehicle (Vehicle1_Id);
      T.Garage_Ref.Park_Vehicle (Vehicle2_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle1_Id),
         "The first vehicle was not parked in the garage");
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle2_Id),
         "The second vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller1_Id, Result1_Id, Boarded);

      Ass.Assert (
         Boarded, "The first traveller did not board any vehicle");
      Ass.Assert (
         Vehicle1_Id = Result1_Id or Vehicle2_Id = Result1_Id,
         "The first traveller did not board any vehicle");

   -- simulate success for Book_Parking callback
      T.Garage_Ref.Put_Leaving_Vehicle (
         Vehicle_Id => Result1_Id,
         Driver_Id  => Traveller1_Id);
      T.Garage_Ref.Remove_Pending_Vehicle (Result1_Id, Traveller1_Id);

      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle1_Id, True);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle2_Id, True);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Does_Travel_Contain_Steps (
         Traveller2_Id);

      T.Garage_Ref.Ask_For_Vehicle (Traveller2_Id, Result2_Id, Boarded);

      Ass.Assert (Boarded, "The second traveller did board a vehicle");
      Ass.Assert (Result1_Id /= Result2_Id,
                 "The second traveller boarded the other vehicle");
   end Test_Ask_For_Vehicle_Two_People_Same_Destination_Full;

   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination (
      T : in out Garage_Test)
   is
      Vehicle1_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Vehicle2_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (469);
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (443);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      PC_Utils_Ref       : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      --Stub_Mock_Ref      : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination1_Slice : Slice.Map := Slice.Empty_Map;
      Destination1_List  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination1_Id    : Infra_Id := 1;
      Destination2_Slice : Slice.Map := Slice.Empty_Map;
      Destination2_List  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination2_Id    : Infra_Id := 2;
      -- out variables
      Result1_Id : Agent.Agent_Id;
      Result2_Id : Agent.Agent_Id;
      Boarded    : Boolean;
   begin
      T.Garage_Ref.Set_Size (2);

      Destination1_List.Append (Destination1_Id);
      Destination1_Slice.Include (FOOT, Destination1_List);
      Destination1_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination1_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Destination2_List.Append (Destination2_Id);
      Destination2_Slice.Include (FOOT, Destination2_List);
      Destination2_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination2_Slice.Include (ROAD, Infra_Id_List.Empty_List);

   -- more a sanity check than an actual test assertion
      Ass.Assert (Destination1_List /= Destination2_List,
                 "The two lists are equal");

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle1_Id, Destination1_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Destination1_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice
         (Destination1_List);
      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle1_Id);
      T.Garage_Ref.Park_Vehicle (Vehicle2_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle1_Id),
         "The first vehicle was not parked in the garage");
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle2_Id),
         "The second vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller1_Id, Result1_Id, Boarded);

      Ass.Assert (Boarded, "The first traveller did not board any vehicle");
      Ass.Assert (Vehicle1_Id = Result1_Id or Vehicle2_Id = Result1_Id,
                 "The first traveller did not board any vehicle");

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle2_Id, Destination2_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Destination2_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination2_List);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle2_Id, False);
      PC_Utils_Ref.Set_Value_For_Board (Traveller2_Id, False);

      T.Garage_Ref.Ask_For_Vehicle (Traveller2_Id, Result2_Id, Boarded);

      Ass.Assert (Boarded, "The second traveller did not board any vehicle");
      Ass.Assert (Result1_Id /= Result2_Id,
                 "The second traveller boarded the same vehicle");
   end Test_Ask_For_Vehicle_Two_People_Different_Destination;

   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle
      (T : in out Garage_Test)
   is
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (443);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      --Stub_Mock_Ref : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination1_Slice : Slice.Map := Slice.Empty_Map;
      Destination1_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination1_Id : Infra_Id := 1;
      Destination2_Slice : Slice.Map := Slice.Empty_Map;
      Destination2_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination2_Id : Infra_Id := 2;
      -- out variables
      Result1_Id : Agent.Agent_Id;
      Result2_Id : Agent.Agent_Id;
      Boarded   : Boolean;
   begin
      T.Garage_Ref.Set_Size (1);

      Destination1_List.Append (Destination1_Id);
      Destination1_Slice.Include (FOOT, Destination1_List);
      Destination1_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination1_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Destination2_List.Append (Destination2_Id);
      Destination2_Slice.Include (FOOT, Destination2_List);
      Destination2_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination2_Slice.Include (ROAD, Infra_Id_List.Empty_List);
   -- more a sanity check than an actual test assertion
      Ass.Assert (Destination1_List /= Destination2_List,
                 "The two lists are equal");

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle_Id, Destination1_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Destination1_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination1_List);
      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The first vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller1_Id, Result1_Id, Boarded);

      Ass.Assert (Boarded, "The first traveller did not board any vehicle");
      Ass.Assert (Vehicle_Id = Result1_Id,
                 "The first traveller did not board any vehicle");

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle_Id, Destination2_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Destination2_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination2_List);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle_Id, False);
      PC_Utils_Ref.Set_Value_For_Board (Traveller2_Id, False);

      T.Garage_Ref.Ask_For_Vehicle (Traveller2_Id, Result2_Id, Boarded);

      Ass.Assert (not Boarded, "The second traveller boarded a vehicle");
   end Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle;

   procedure Test_Leave_Without_Boarding (
      T : in out Garage_Test)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      -- out variables
      Vehicle_Id : Agent.Agent_Id;
      Leaving   : Boolean;
   begin
      T.Garage_Ref.Set_Size (1);
      Leaving := T.Garage_Ref.Leave_Parking (Traveller_Id, Vehicle_Id);

      Ass.Assert (not Leaving, "The traveller left the garage");
   end Test_Leave_Without_Boarding;

   procedure Test_Leave_After_Boarding (
      T : in out Garage_Test)
   is
      Vehicle_Id        : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Traveller_Id      : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      --Stub_Mock_Ref     : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id    : Infra_Id := 1;
      -- out variables
      Result_Id : Agent.Agent_Id;
      Boarded   : Boolean;
      Leaving   : Boolean;
   begin
      T.Garage_Ref.Set_Size (1);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);
      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle_Id);

      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller_Id, Result_Id, Boarded);

      Ass.Assert (Vehicle_Id = Result_Id,
                 "The traveller did not board any vehicle");
      Ass.Assert (Boarded, "The traveller did not board any vehicle");

   -- simulate success for Book_Parking callback
      T.Garage_Ref.Put_Leaving_Vehicle (
         Vehicle_Id => Result_Id,
         Driver_Id  => Traveller_Id);
      T.Garage_Ref.Remove_Pending_Vehicle (Result_Id, Traveller_Id);

      Leaving := T.Garage_Ref.Leave_Parking (Traveller_Id, Vehicle_Id);

      Ass.Assert (Leaving, "The traveller did not leave the garage");
   end Test_Leave_After_Boarding;

   procedure Test_Leave_Garage_Two_Travellers (
      T : in out Garage_Test)
   is
      Vehicle1_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Vehicle2_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (469);
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (443);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      PC_Utils_Ref : People_Carrier_Mock.Reference
         := People_Carrier_Mock.Reference (T.PC_Utils);
      --Stub_Mock_Ref : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result1_Id : Agent.Agent_Id;
      Result2_Id : Agent.Agent_Id;
      Leaving_Id : Agent.Agent_Id;
      Boarded    : Boolean;
      Leaving    : Boolean;
   begin
      T.Garage_Ref.Set_Size (2);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller1_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Vehicle2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller2_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);

      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle1_Id, False);
      PC_Utils_Ref.Set_Value_For_Is_Carrier_Full (Vehicle2_Id, False);
      PC_Utils_Ref.Set_Value_For_Board (Traveller1_Id, True);
      PC_Utils_Ref.Set_Value_For_Board (Traveller2_Id, True);

      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle1_Id);
      T.Garage_Ref.Park_Vehicle (Vehicle2_Id);
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle1_Id),
         "The first vehicle was not parked in the garage");
      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle2_Id),
         "The second vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller1_Id, Result1_Id, Boarded);

      Ass.Assert (Vehicle1_Id = Result1_Id or Vehicle2_Id = Result1_Id,
                 "The first traveller did not board any vehicle");
      Ass.Assert (Boarded, "The first traveller did not board any vehicle");

   -- simulate success for Book_Parking callback
      T.Garage_Ref.Put_Leaving_Vehicle (
         Vehicle_Id => Result1_Id,
         Driver_Id  => Traveller1_Id);
      T.Garage_Ref.Remove_Pending_Vehicle (Result1_Id, Traveller1_Id);

      T.Garage_Ref.Ask_For_Vehicle (Traveller2_Id, Result2_Id, Boarded);

      Ass.Assert (Result1_Id = Result2_Id,
                 "The second traveller did not board any vehicle");
      Ass.Assert (Boarded, "The second traveller did not board any vehicle");

      Leaving := T.Garage_Ref.Leave_Parking (Traveller2_Id, Leaving_Id);
      Ass.Assert (
         not Leaving,
         "The second traveller is making the vehicle leave the garage");

      Leaving := T.Garage_Ref.Leave_Parking (Traveller1_Id, Leaving_Id);
      Ass.Assert (Leaving,
         "The travellers did not leave the garage");
      Ass.Assert (Leaving_Id = Result1_Id,
         "The travellers did not leave the garage with the car they boarded");
   end Test_Leave_Garage_Two_Travellers;

   procedure Test_Leave_Garage_Is_Idempotent (
      T : in out Garage_Test)
   is
      Vehicle_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (468);
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (442);
      Traveller_Utils_Mock_Ref : Traveller_Utils_Mock.Reference
         := Traveller_Utils_Mock.Reference (T.Traveller_Utils);
      --Stub_Mock_Ref : Remote_Stub_Mock.Reference
      --   := Remote_Stub_Mock.Reference (T.Stub);
      Destination_Slice : Slice.Map := Slice.Empty_Map;
      Destination_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Destination_Id : Infra_Id := 1;
      -- out variables
      Result_Id : Agent.Agent_Id;
      Boarded   : Boolean;
      Leaving   : Boolean;
   begin
      T.Garage_Ref.Set_Size (1);

      Destination_List.Append (Destination_Id);
      Destination_Slice.Include (FOOT, Destination_List);
      Destination_Slice.Include (BIKE, Infra_Id_List.Empty_List);
      Destination_Slice.Include (ROAD, Infra_Id_List.Empty_List);

      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_Travel_Destination (
         Traveller_Id, Destination_Slice);
      Traveller_Utils_Mock_Ref.Set_Return_Value_For_Get_List_From_Slice (
         Destination_List);
      --Stub_Mock_Ref.Set_Return_Value_For_Query (True);

      T.Garage_Ref.Park_Vehicle (Vehicle_Id);

      Ass.Assert (
         T.Garage_Ref.Parked_Vehicles.Contains (Vehicle_Id),
         "The vehicle was not parked in the garage");

      T.Garage_Ref.Ask_For_Vehicle (Traveller_Id, Result_Id, Boarded);

   -- simulate success for Book_Parking callback
      T.Garage_Ref.Put_Leaving_Vehicle (
         Vehicle_Id => Result_Id,
         Driver_Id  => Traveller_Id);
      T.Garage_Ref.Remove_Pending_Vehicle (Result_Id, Traveller_Id);

      Ass.Assert (Vehicle_Id = Result_Id,
                 "The traveller did not board any vehicle");
      Ass.Assert (Boarded, "The traveller did not board any vehicle");

      Leaving := T.Garage_Ref.Leave_Parking (Traveller_Id, Vehicle_Id);
      Ass.Assert (Leaving, "The traveller did not leave the garage");

      Leaving := T.Garage_Ref.Leave_Parking (Traveller_Id, Vehicle_Id);
      Ass.Assert (not Leaving, "The traveller left the garage again");
   end Test_Leave_Garage_Is_Idempotent;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Park_Vehicle_With_Room_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Park_Vehicle_With_Room (T);
   end Test_Park_Vehicle_With_Room_Wrapper;

   procedure Test_Park_Vehicle_Without_Room_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Park_Vehicle_Without_Room (T);
   end Test_Park_Vehicle_Without_Room_Wrapper;

   procedure Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage (T);
   end Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage_Wrapper;

   procedure Test_Ask_For_Vehicle_With_Vehicle_In_Garage_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_With_Vehicle_In_Garage (T);
   end Test_Ask_For_Vehicle_With_Vehicle_In_Garage_Wrapper;

   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_Two_People_Same_Destination (T);
   end Test_Ask_For_Vehicle_Two_People_Same_Destination_Wrapper;

   procedure Test_Ask_For_Vehicle_Two_People_Same_Destination_Full_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_Two_People_Same_Destination_Full (T);
   end Test_Ask_For_Vehicle_Two_People_Same_Destination_Full_Wrapper;

   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_Two_People_Different_Destination (T);
   end Test_Ask_For_Vehicle_Two_People_Different_Destination_Wrapper;

   procedure Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle (T);
   end Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle_Wrapper;

   procedure Test_Leave_Without_Boarding_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Leave_Without_Boarding (T);
   end Test_Leave_Without_Boarding_Wrapper;

   procedure Test_Leave_After_Boarding_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Leave_After_Boarding (T);
   end Test_Leave_After_Boarding_Wrapper;

   procedure Test_Leave_Garage_Two_Travellers_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Leave_Garage_Two_Travellers (T);
   end Test_Leave_Garage_Two_Travellers_Wrapper;

   procedure Test_Leave_Garage_Is_Idempotent_Wrapper (
      T : in out Garage_Test'Class)
   is
   begin
      Test_Leave_Garage_Is_Idempotent (T);
   end Test_Leave_Garage_Is_Idempotent_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Garage_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Garage_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Park_Vehicle_With_Room_Wrapper'Access,
         Name    => "Tests a vehicle parking when there is room in a garage");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Park_Vehicle_Without_Room_Wrapper'Access,
         Name    =>
            "Tests a vehicle parking when there is not room in a garage");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Ask_For_Vehicle_With_No_Vehicle_In_Garage_Wrapper'Access,
         Name    => "Tests you can not get a vehicle if there is no one");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Ask_For_Vehicle_With_Vehicle_In_Garage_Wrapper'Access,
         Name    => "Tests you can get a vehicle if there is a parked one");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Ask_For_Vehicle_Two_People_Same_Destination_Wrapper'Access,
         Name    => "Tests two people with the same destination can share the"
                  & " same vehicle");

      Register_Wrapper (
         Test    => T,
         Routine =>
      Test_Ask_For_Vehicle_Two_People_Same_Destination_Full_Wrapper'Access,
         Name    => "Tests two people with the same destination can not share"
                  & " the same vehicle if it is full after the first one"
                  & " boards it");

      Register_Wrapper (
         Test    => T,
         Routine =>
      Test_Ask_For_Vehicle_Two_People_Different_Destination_Wrapper'Access,
         Name    => "Tests two people with different destination can not share"
                  & " the same vehicle");

      Register_Wrapper (
         Test    => T,
         Routine =>
      Test_Ask_For_Vehicle_Two_People_Different_Destination_One_Vehicle_Wrapper'Access,
         Name    => "Tests two people with different destination can not share"
                  & " the same vehicle; the second traveller won't board"
                  & " anything");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Leave_Without_Boarding_Wrapper'Access,
         Name    => "Tests a traveller can not leave the garage if he did not"
                  & " board a vehicle before");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Leave_After_Boarding_Wrapper'Access,
         Name    => "Tests a traveller can leave the garage after boarding a"
                  & " vehicle");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Leave_Garage_Two_Travellers_Wrapper'Access,
         Name    => "Tests two travellers can leave the garage together");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Leave_Garage_Is_Idempotent_Wrapper'Access,
         Name    => "Test that leaving operation is idempotent");

   end Register_Tests;

   function Name(T : Garage_Test) return AU.Message_String is
   begin
      return AU.Format (
         "Reactive.Infrastructure.Building.Parking_Manager.Garage");
   end Name;

end Reactive.Infrastructure.Building.Parking_Manager.Garage.Tests;
