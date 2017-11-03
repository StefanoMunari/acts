with AUnit.Assertions;

with Active.Traveller;
with Active.Traveller.Pedestrian.Mock;
with Active.Traveller.Vehicle.Mock;

with Reactive.District.Mock;

with Shared.Agent_Id_List;

package body Active.People_Carrier.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package District_Mock renames Reactive.District.Mock;
   package Pedestrian renames Active.Traveller.Pedestrian;
   package Pedestrian_Mock renames Pedestrian.Mock;
   package Vehicle renames Active.Traveller.Vehicle;
   package Vehicle_Mock renames Vehicle.Mock;
   package Agent_Id_List   renames Shared.Agent_Id_List;
   use Agent_Id_List;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out People_Carrier_Utils_Test) is
   begin
      T.District_Ref := Reactive.District.Reference (District_Mock.Create);
      T.People_Carrier_Utils_Ref
         := People_Carrier_Utils.Get_Instance (T.District_Ref);
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Pedestrian_Is_Not_A_People_Carrier (
      T: in out People_Carrier_Utils_Test)
   is
      Pedestrian_Ref : Pedestrian_Mock.Reference := Pedestrian_Mock.Create;
      Traveller_Ref : Active.Traveller.Reference;
      Pedestrian_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (255);
      Added : Boolean;
      Result : Boolean;
   begin
      Pedestrian_Ref.Set_Id (Pedestrian_Id);
      Traveller_Ref := Active.Traveller.Reference (Pedestrian_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Added);

      Result := T.People_Carrier_Utils_Ref.Is_A_People_Carrier (Pedestrian_Id);
      Ass.Assert (not Result,
                  "Pedestrian is seen as a people carrier");
   end Test_Pedestrian_Is_Not_A_People_Carrier;

   procedure Test_Vehicle_Is_A_People_Carrier (
      T: in out People_Carrier_Utils_Test)
   is
      Vehicle_Ref : Vehicle_Mock.Reference := Vehicle_Mock.Create;
      Traveller_Ref : Active.Traveller.Reference;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (256);
      Added : Boolean;
      Result : Boolean;
   begin
      Vehicle_Ref.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Traveller_Ref := Active.Traveller.Reference (Vehicle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Added);

      Result := T.People_Carrier_Utils_Ref.Is_A_People_Carrier (Vehicle_Id);
      Ass.Assert (Result,
                  "Vehicle is seen as a people carrier");
   end Test_Vehicle_Is_A_People_Carrier;

   procedure Test_Get_Passengers (T: in out People_Carrier_Utils_Test)
   is
      Vehicle_Ref : Vehicle_Mock.Reference := Vehicle_Mock.Create;
      Traveller_Ref : Active.Traveller.Reference;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (257);
      Added : Boolean;
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (258);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (259);
      Passenger3_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (260);
      Passengers : Agent_Id_List.List;
      Result : Agent_Id_List.List;
   begin
      Vehicle_Ref.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Traveller_Ref := Active.Traveller.Reference (Vehicle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Added);
      Passengers.Append (Passenger1_Id);
      Passengers.Append (Passenger2_Id);
      Passengers.Append (Passenger3_Id);
      Vehicle_Ref.Set_Return_Value_For_Get_Passengers (Passengers);

      Result := T.People_Carrier_Utils_Ref.Get_Passengers (Vehicle_Id);
      Ass.Assert (Result = Passengers,
                  "Utils was not been able to get the right passengers");
   end Test_Get_Passengers;

   procedure Test_Board (T: in out People_Carrier_Utils_Test)
   is
      Vehicle_Ref : Vehicle_Mock.Reference := Vehicle_Mock.Create;
      Traveller_Ref : Active.Traveller.Reference;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (261);
      Added : Boolean;
      Passenger_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (262);
      Result : Boolean;
      Board_Called : Boolean;
   begin
      Vehicle_Ref.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Traveller_Ref := Active.Traveller.Reference (Vehicle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Added);
      Vehicle_Ref.Set_Return_Value_For_Board (True);

      T.People_Carrier_Utils_Ref.Board (Vehicle_Id, Passenger_Id, Result);
      Ass.Assert (Result,
                  "Passenger did not board the vehicle");
      Board_Called := Vehicle_Ref.Get_Board_Called;
      Ass.Assert (Board_Called,
                  "Vehicle's Board operation was not invoked");
   end Test_Board;

   procedure Test_Free (T: in out People_Carrier_Utils_Test)
   is
      Vehicle_Ref : Vehicle_Mock.Reference := Vehicle_Mock.Create;
      Traveller_Ref : Active.Traveller.Reference;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (261);
      Added : Boolean;
      Passenger_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (262);
      Result : Boolean;
      Free_Called : Boolean;
   begin
      Vehicle_Ref.Set_Return_Value_For_Get_Id (Vehicle_Id);
      Traveller_Ref := Active.Traveller.Reference (Vehicle_Ref);
      T.District_Ref.Add_Traveller (Traveller_Ref, Added);
      Vehicle_Ref.Set_Return_Value_For_Free (True);

      T.People_Carrier_Utils_Ref.Free (Vehicle_Id, Passenger_Id, Result);
      Ass.Assert (Result,
                  "Passenger did not leave the vehicle");
      Free_Called := Vehicle_Ref.Get_Free_Called;
      Ass.Assert (Free_Called,
                  "Vehicle's Free operation was not invoked");
   end Test_Free;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Pedestrian_Is_Not_A_People_Carrier_Wrapper (
      T : in out People_Carrier_Utils_Test'Class)
   is
   begin
      Test_Pedestrian_Is_Not_A_People_Carrier (T);
   end Test_Pedestrian_Is_Not_A_People_Carrier_Wrapper;

   procedure Test_Vehicle_Is_A_People_Carrier_Wrapper (
      T : in out People_Carrier_Utils_Test'Class)
   is
   begin
      Test_Vehicle_Is_A_People_Carrier (T);
   end Test_Vehicle_Is_A_People_Carrier_Wrapper;

   procedure Test_Get_Passengers_Wrapper (
      T : in out People_Carrier_Utils_Test'Class)
   is
   begin
      Test_Get_Passengers (T);
   end Test_Get_Passengers_Wrapper;

   procedure Test_Board_Wrapper (
      T : in out People_Carrier_Utils_Test'Class)
   is
   begin
      Test_Board (T);
   end Test_Board_Wrapper;

   procedure Test_Free_Wrapper (
      T : in out People_Carrier_Utils_Test'Class)
   is
   begin
      Test_Free (T);
   end Test_Free_Wrapper;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out People_Carrier_Utils_Test) is
      use TC.Registration;
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (People_Carrier_Utils_Test);
      use Register_Specific;
   begin

      Register_Wrapper
        (Test    => T,
         Routine => Test_Pedestrian_Is_Not_A_People_Carrier_Wrapper'Access,
         Name    => "Tests a pedestrian is not a people carrier");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Vehicle_Is_A_People_Carrier_Wrapper'Access,
         Name    => "Tests a vehicle is a people carrier");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Get_Passengers_Wrapper'Access,
         Name    => "Tests utils can get the passengers");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Board_Wrapper'Access,
         Name    => "Tests utils can make a passenger board a vehicle");

      Register_Wrapper
        (Test    => T,
         Routine => Test_Free_Wrapper'Access,
         Name    => "Tests utils can make a passenger get off from a vehicle");

   end Register_Tests;

   function Name(T: People_Carrier_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Traveller_Utils");
   end Name;
end Active.People_Carrier.Utils.Tests;
