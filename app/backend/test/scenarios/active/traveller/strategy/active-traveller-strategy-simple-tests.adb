with AUnit.Assertions;

with Passive.Road_Sign.Bus_Stop.Mock;

with Shared.Agent_Id_To_Infra_Id_List_Map;

package body Active.Traveller.Strategy.Simple.Tests is
   package Ass renames AUnit.Assertions;
   package Bus_Stop_Pkg renames Passive.Road_Sign.Bus_Stop;
   package Bus_Stop_Mock_Pkg renames Passive.Road_Sign.Bus_Stop.Mock;
   package Agent_Id_To_Infra_Id_List_Map
      renames Shared.Agent_Id_To_Infra_Id_List_Map;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Simple_Test)
   is
   begin
      T.Traveller_Utils := Traveller_Utils_Mock_Pkg.Create;
      T.Simple_Strategy := Simple.Get_Instance (T.Traveller_Utils);
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Wait_For_Bus_Or_Not (T : in out Simple_Test)
   is
      Bus_Stop_Mock : Bus_Stop_Mock_Pkg.Reference
         := Bus_Stop_Mock_Pkg.Create;
      Buses_Map       : Agent_Id_To_Infra_Id_List_Map.Map
         := Agent_Id_To_Infra_Id_List_Map.Empty_Map;
      Bus_No_1_Id     : Agent.Agent_Id := Agent.Create_Id_From_Natural (1);
      Bus_No_1_Stops  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Pedestrian_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (10);
      Current_Stretch : Infra_Id := 42;
   begin
      Bus_No_1_Stops.Append (50);
      Bus_No_1_Stops.Append (51);
      Bus_No_1_Stops.Append (52);
      Bus_No_1_Stops.Append (53);
      Bus_No_1_Stops.Append (54);
      Buses_Map.Include (Key => Bus_No_1_Id, New_Item => Bus_No_1_Stops);
      Bus_Stop_Mock.Set_Return_Value_For_Get_All_Bus_Stops (Buses_Map);

      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         50, True);
      T.Traveller_Utils.Set_Return_Value_For_Get_Next_Step (
         Pedestrian_Id, 50);

      Ass.Assert (
         T.Simple_Strategy.Wait_For_Bus_Or_Not (
            Pedestrian_Id   => Pedestrian_Id,
            Current_Stretch => Current_Stretch,
            Bus_Stop_Ref    => Bus_Stop_Pkg.Reference (Bus_Stop_Mock)
         ),
         "Strategy did not decide to wait for the bus");
      Ass.Assert (
         Bus_Stop_Mock.Get_Register_For_Bus_Waiting_Called,
         "Register for bus has not been called");
   end Test_Wait_For_Bus_Or_Not;

   procedure Test_Wait_For_Bus_Or_Not_Second_Stop (T : in out Simple_Test)
   is
      Bus_Stop_Mock : Bus_Stop_Mock_Pkg.Reference
         := Bus_Stop_Mock_Pkg.Create;
      Buses_Map       : Agent_Id_To_Infra_Id_List_Map.Map
         := Agent_Id_To_Infra_Id_List_Map.Empty_Map;
      Bus_No_1_Id     : Agent.Agent_Id := Agent.Create_Id_From_Natural (1);
      Bus_No_1_Stops  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Pedestrian_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (10);
      Current_Stretch : Infra_Id := 42;
   begin
      Bus_No_1_Stops.Append (50);
      Bus_No_1_Stops.Append (51);
      Bus_No_1_Stops.Append (52);
      Bus_No_1_Stops.Append (53);
      Bus_No_1_Stops.Append (54);
      Buses_Map.Include (Key => Bus_No_1_Id, New_Item => Bus_No_1_Stops);
      Bus_Stop_Mock.Set_Return_Value_For_Get_All_Bus_Stops (Buses_Map);

      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         50, False);
      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         51, True);
      T.Traveller_Utils.Set_Return_Value_For_Get_Next_Step (
         Pedestrian_Id, 51);

      Ass.Assert (
         T.Simple_Strategy.Wait_For_Bus_Or_Not (
            Pedestrian_Id   => Pedestrian_Id,
            Current_Stretch => Current_Stretch,
            Bus_Stop_Ref    => Bus_Stop_Pkg.Reference (Bus_Stop_Mock)
         ),
         "Strategy did not decide to wait for the bus");
      Ass.Assert (
         Bus_Stop_Mock.Get_Register_For_Bus_Waiting_Called,
         "Register for bus has not been called");
   end Test_Wait_For_Bus_Or_Not_Second_Stop;

   procedure Test_Wait_For_Bus_Or_Not_Do_Not_Stop (T : in out Simple_Test)
   is
      Bus_Stop_Mock : Bus_Stop_Mock_Pkg.Reference
         := Bus_Stop_Mock_Pkg.Create;
      Buses_Map       : Agent_Id_To_Infra_Id_List_Map.Map
         := Agent_Id_To_Infra_Id_List_Map.Empty_Map;
      Bus_No_1_Id     : Agent.Agent_Id := Agent.Create_Id_From_Natural (1);
      Bus_No_1_Stops  : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Pedestrian_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (10);
      Current_Stretch : Infra_Id := 42;
   begin
      Bus_No_1_Stops.Append (50);
      Bus_No_1_Stops.Append (51);
      Bus_No_1_Stops.Append (52);
      Bus_No_1_Stops.Append (53);
      Bus_No_1_Stops.Append (54);
      Buses_Map.Include (Key => Bus_No_1_Id, New_Item => Bus_No_1_Stops);
      Bus_Stop_Mock.Set_Return_Value_For_Get_All_Bus_Stops (Buses_Map);

      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         50, False);
      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         51, False);
      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         52, False);
      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         53, False);
      T.Traveller_Utils.Set_Return_Value_For_Does_Travel_Contain_Step (
         54, False);

      Ass.Assert (
         not T.Simple_Strategy.Wait_For_Bus_Or_Not (
            Pedestrian_Id   => Pedestrian_Id,
            Current_Stretch => Current_Stretch,
            Bus_Stop_Ref    => Bus_Stop_Pkg.Reference (Bus_Stop_Mock)
         ),
         "Strategy decided to wait for the bus");
      Ass.Assert (
         not Bus_Stop_Mock.Get_Register_For_Bus_Waiting_Called,
         "Register for bus has been called");
   end Test_Wait_For_Bus_Or_Not_Do_Not_Stop;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Wait_For_Bus_Or_Not_Wrapper (
      T : in out Simple_Test'Class)
   is
   begin
      Test_Wait_For_Bus_Or_Not (T);
   end Test_Wait_For_Bus_Or_Not_Wrapper;

   procedure Test_Wait_For_Bus_Or_Not_Second_Stop_Wrapper (
      T : in out Simple_Test'Class)
   is
   begin
      Test_Wait_For_Bus_Or_Not_Second_Stop (T);
   end Test_Wait_For_Bus_Or_Not_Second_Stop_Wrapper;

   procedure Test_Wait_For_Bus_Or_Not_Do_Not_Stop_Wrapper (
      T : in out Simple_Test'Class)
   is
   begin
      Test_Wait_For_Bus_Or_Not_Do_Not_Stop (T);
   end Test_Wait_For_Bus_Or_Not_Do_Not_Stop_Wrapper;


   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Simple_Test) is
      use TC.Registration;
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Simple_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Wait_For_Bus_Or_Not_Wrapper'Access,
         Name    => "Tests wait for bus strategy");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Wait_For_Bus_Or_Not_Second_Stop_Wrapper'Access,
         Name    => "Tests wait for bus strategy: true at second stop");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Wait_For_Bus_Or_Not_Do_Not_Stop_Wrapper'Access,
         Name    => "Tests wait for bus strategy: false");

   end Register_Tests;

   function Name(T: Simple_Test) return AU.Message_String is
   begin
      return AU.Format ("Strategy.Simple");
   end Name;

end Active.Traveller.Strategy.Simple.Tests;
