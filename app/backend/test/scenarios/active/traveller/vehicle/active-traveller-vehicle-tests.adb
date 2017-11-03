with AUnit.Assertions;

with Active.Agent;

package body Active.Traveller.Vehicle.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   use Reactive.Stretch_Type_Package;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Board_Not_On_Board_Passenger (T : in out Vehicle_Test)
   is
      Incomer : Agent.Agent_Id := Agent.Create_Id_From_Natural (34);
      Boarded : Boolean := FALSE;
      Vehicle : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
   begin
      Vehicle.Board (Incomer => Incomer,
                     Boarded => Boarded);

      Ass.Assert (Boarded,
                  "The passenger is not accepted on board");
   end Test_Board_Not_On_Board_Passenger;

   procedure Test_Not_Board_Already_On_Board_Passenger (
      T : in out Vehicle_Test)
   is
      Incomer : Agent.Agent_Id := Agent.Create_Id_From_Natural (35);
      Boarded : Boolean := FALSE;
      Vehicle : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
   begin
      Vehicle.Passengers.Append (New_Item => Incomer);

      Vehicle.Board (Incomer => Incomer,
                     Boarded => Boarded);

      Ass.Assert (not Boarded,
                  "The passenger is not already on board");
   end Test_Not_Board_Already_On_Board_Passenger;

   procedure Test_Free_On_Board_Passenger (T : in out Vehicle_Test)
   is
      Passenger_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (36);
      Freed : Boolean := FALSE;
      Vehicle : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
   begin
      Vehicle.Passengers.Append (New_Item => Passenger_Id);

      Vehicle.Free (Passenger => Passenger_Id,
                    Freed => Freed);

      Ass.Assert (Freed,
                  "The passenger is not freed");
   end Test_Free_On_Board_Passenger;

   procedure Test_Not_Free_Not_On_Board_Passenger (T : in out Vehicle_Test)
   is
      Passenger_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (35);
      Freed : Boolean := FALSE;
      Vehicle : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
   begin
      Vehicle.Free (Passenger => Passenger_Id,
                    Freed => Freed);

      Ass.Assert (not Freed,
                  "The passenger is freed");
   end Test_Not_Free_Not_On_Board_Passenger;

   procedure Test_Count_On_Board_Passengers (T : in out Vehicle_Test)
   is
      Vehicle : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
   begin
      Ass.Assert (
         Vehicle.Count_Passengers = Natural(Vehicle.Passengers.Length),
         "The passenger is not freed");
   end Test_Count_On_Board_Passengers;

   procedure Test_Travel_With_People_Landing (T : in out Vehicle_Test)
   is
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (30);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (31);
      Vehicle_Ref   : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
      Position      : Infra_Id := 42;
      Dummy_Id      : Infra_Id := 77;
      Slice1        : Slice.Map := Slice.Empty_Map;
      Slice2        : Slice.Map := Slice.Empty_Map;
      Foot_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Foot_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Host_Id       : Infra_Id := 65;
      Dummy_List    : Agent_Id_List.List := Agent_Id_List.Empty_List;
   begin
      Vehicle_Ref.Passengers.Append (New_Item => Passenger1_Id);
      Vehicle_Ref.Passengers.Append (New_Item => Passenger2_Id);
      Vehicle_Ref.Current_Position := Position;
      T.Stretch_Utils.Set_Return_Value_For_Has_Host (Position, True);
      T.Travel_Ref.Set_Return_Value_For_Has_Next_Step (True);

      Road_List1.Append (Position);
      Slice1.Include (FOOT, Foot_List1);
      Slice1.Include (BIKE, Bike_List1);
      Slice1.Include (ROAD, Road_List1);
      Road_List2.Append (Dummy_Id);
      Slice2.Include (FOOT, Foot_List2);
      Slice2.Include (BIKE, Bike_List2);
      Slice2.Include (ROAD, Road_List2);
      T.Traveller_Utils.Set_Return_Value_For_Get_Travel_Destination (
         Passenger1_Id, Slice1);
      T.Traveller_Utils.Set_Return_Value_For_Get_Travel_Destination (
         Passenger2_Id, Slice2);
      T.Host_Utils.Set_Travellers_List_For_Stop_Over (Dummy_List);

      T.Stretch_Utils.Set_Return_Value_For_Get_Host (Position, Host_Id);

      Vehicle.Travel (Vehicle_Ref.all);

      Ass.Assert (
         not Vehicle_Ref.Passengers.Contains (Passenger1_Id),
         "Passenger 1 is still present");
      Ass.Assert (
         Vehicle_Ref.Passengers.Contains (Passenger2_Id),
         "Passenger 2 is not present anymore");
      Ass.Assert (
         T.Travel_Ref.Get_Advance_Called,
         "Travel did not advance");
   end Test_Travel_With_People_Landing;

   procedure Test_Travel_With_No_People_Landing (T : in out Vehicle_Test)
   is
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (30);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (31);
      Vehicle_Ref   : Traveller.Vehicle.Reference
        := Traveller.Vehicle.Reference(T.Traveller);
      Position      : Infra_Id := 42;
      Dummy_Id1     : Infra_Id := 77;
      Dummy_Id2     : Infra_Id := 78;
      Slice1        : Slice.Map := Slice.Empty_Map;
      Slice2        : Slice.Map := Slice.Empty_Map;
      Foot_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Foot_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List1    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List2    : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Host_Id       : Infra_Id := 65;
      Dummy_List    : Agent_Id_List.List := Agent_Id_List.Empty_List;
   begin
      Vehicle_Ref.Passengers.Append (New_Item => Passenger1_Id);
      Vehicle_Ref.Passengers.Append (New_Item => Passenger2_Id);
      Vehicle_Ref.Current_Position := Position;
      T.Stretch_Utils.Set_Return_Value_For_Has_Host (Position, True);
      T.Travel_Ref.Set_Return_Value_For_Has_Next_Step (True);

      Road_List1.Append (Dummy_Id1);
      Slice1.Include (FOOT, Foot_List1);
      Slice1.Include (BIKE, Bike_List1);
      Slice1.Include (ROAD, Road_List1);
      Road_List2.Append (Dummy_Id2);
      Slice2.Include (FOOT, Foot_List2);
      Slice2.Include (BIKE, Bike_List2);
      Slice2.Include (ROAD, Road_List2);
      T.Traveller_Utils.Set_Return_Value_For_Get_Travel_Destination (
         Passenger1_Id, Slice1);
      T.Traveller_Utils.Set_Return_Value_For_Get_Travel_Destination (
         Passenger2_Id, Slice2);
      T.Host_Utils.Set_Travellers_List_For_Stop_Over (Dummy_List);

      T.Stretch_Utils.Set_Return_Value_For_Get_Host (Position, Host_Id);

      Vehicle.Travel (Vehicle_Ref.all);

      Ass.Assert (
         Vehicle_Ref.Passengers.Contains (Passenger1_Id),
         "Passenger 1 is not present anymore");
      Ass.Assert (
         Vehicle_Ref.Passengers.Contains (Passenger2_Id),
         "Passenger 2 is not present anymore");
      Ass.Assert (
         T.Travel_Ref.Get_Advance_Called,
         "Travel did not advance");
   end Test_Travel_With_No_People_Landing;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Board_Not_On_Board_Passenger_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Board_Not_On_Board_Passenger (T);
   end Test_Board_Not_On_Board_Passenger_Wrapper;

   procedure Test_Not_Board_Already_On_Board_Passenger_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Not_Board_Already_On_Board_Passenger (T);
   end Test_Not_Board_Already_On_Board_Passenger_Wrapper;

   procedure Test_Free_On_Board_Passenger_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Free_On_Board_Passenger (T);
   end Test_Free_On_Board_Passenger_Wrapper;

   procedure Test_Not_Free_Not_On_Board_Passenger_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Not_Free_Not_On_Board_Passenger (T);
   end Test_Not_Free_Not_On_Board_Passenger_Wrapper;

   procedure Test_Count_On_Board_Passengers_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Count_On_Board_Passengers (T);
   end Test_Count_On_Board_Passengers_Wrapper;

   procedure Test_Travel_With_People_Landing_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Travel_With_People_Landing (T);
   end Test_Travel_With_People_Landing_Wrapper;

   procedure Test_Travel_With_No_People_Landing_Wrapper (
      T : in out Vehicle_Test'Class) is
   begin
      Test_Travel_With_No_People_Landing (T);
   end Test_Travel_With_No_People_Landing_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Vehicle_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Vehicle_Test);
      use Register_Specific;
   begin
      Traveller.Tests.Register_Tests (Traveller.Tests.Traveller_Test (T));

      Register_Wrapper (
         Test => T,
         Routine => Test_Board_Not_On_Board_Passenger_Wrapper'Access,
         Name => "Test board not on board passenger");

      Register_Wrapper (
         Test => T,
         Routine => Test_Not_Board_Already_On_Board_Passenger_Wrapper'Access,
         Name => "Test not board on board passenger");

      Register_Wrapper (
         Test => T,
         Routine => Test_Free_On_Board_Passenger_Wrapper'Access,
         Name => "Test free on board passenger passenger");

      Register_Wrapper (
         Test => T,
         Routine => Test_Not_Free_Not_On_Board_Passenger_Wrapper'Access,
         Name => "Test not free not on board passenger passenger");

      Register_Wrapper (
         Test => T,
         Routine => Test_Count_On_Board_Passengers_Wrapper'Access,
         Name => "Test count passengers");

      if T.With_Travel_Tests then

         Register_Wrapper (
            Test => T,
            Routine => Test_Travel_With_People_Landing_Wrapper'Access,
            Name => "Test land passengers when travelling");

         Register_Wrapper (
            Test => T,
            Routine => Test_Travel_With_No_People_Landing_Wrapper'Access,
            Name => "Test do not land passengers when travelling");

      end if;
   end Register_Tests;

   procedure Register_Tests_Without_Travel_Tests (T : in out Vehicle_Test)
   is
   begin
      T.With_Travel_Tests := False;
      T.Register_Tests;
   end Register_Tests_Without_Travel_Tests;

end Active.Traveller.Vehicle.Tests;
