with AUnit.Assertions;

with Active.People_Carrier.Utils.Mock;
with Active.Traveller;
with Active.Traveller.Pedestrian.Mock;
with Active.Traveller.Vehicle.Mock;

with Interface_Layer.Remote.Stub.Mock;
with Interface_Layer.Wrappers.Application.Mock_Factory;

with Reactive.District.Mock;
with Reactive.Infrastructure.Building.Host.Mock;

with Shared.Agent_Id_List;

package body Reactive.Infrastructure.Building.Host.Utils.Tests is

   package Ass             renames AUnit.Assertions;
   package PC_Utils_Mock   renames Active.People_Carrier.Utils.Mock;
   package Traveller_Pkg   renames Active.Traveller;
   package Pedestrian_Mock renames Active.Traveller.Pedestrian.Mock;
   package Vehicle_Mock    renames Active.Traveller.Vehicle.Mock;
   package Stub_Mock_Pkg   renames Interface_Layer.Remote.Stub.Mock;
   package District_Mock   renames Reactive.District.Mock;
   package Host_Mock
      renames Reactive.Infrastructure.Building.Host.Mock;
   package Agent_Id_List   renames Shared.Agent_Id_List;
   use Agent;
   use Agent_Id_List;
   use Active.Space_Master.Next_Action_Type;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T : in out Host_Utils_Test) is
   begin
      T.District_Ref := District_Mock.Create;
      T.PC_Utils :=
         Active.People_Carrier.Utils.Reference (PC_Utils_Mock.Create);
      T.Stub := Stub_Pkg.Reference (Stub_Mock_Pkg.Create);
      T.Wrapper_Factory :=
         Wrapper_Fac_Pkg.Reference (App_Wrapper_Pkg.Mock_Factory.Create);
      T.Host_Utils_Ref := Host.Utils.Get_Instance (
         PC_Utils        => T.PC_Utils,
         District_Ref    => T.District_Ref,
         Wrapper_Factory => T.Wrapper_Factory,
         Stub            => T.Stub);
   end Set_Up;

   procedure Tear_Down (T: in out Host_Utils_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Pedestrian_Stops_Over (T : in out Host_Utils_Test) is
      Pedestrian    : Pedestrian_Mock.Reference := Pedestrian_Mock.Create;
      Pedestrian_Id       : Agent.Agent_Id :=
         Agent.Create_Id_From_Natural (800);
      Added               : Boolean;
      Travellers          : Agent_Id_List.List;
      Utils               : PC_Utils_Mock.Reference
         := PC_Utils_Mock.Reference (T.PC_Utils);
      Host_Mock_Ref       : Host_Mock.Reference := Host_Mock.Create;
      Host_Id             : Infra_Id := 52;
      Host_Added          : Boolean := False;
      Travellers_Returned : Agent_Id_List.List;
   begin
      Host_Mock_Ref.Set_Return_Value_For_Get_Id (Host_Id);
      Pedestrian.Set_Id (Pedestrian_Id);
      T.District_Ref.Add_Traveller (
         Traveller_Pkg.Reference (Pedestrian), Added);

      Travellers.Append (Pedestrian_Id);
      Utils.Set_Value_For_Is_A_People_Carrier (Pedestrian_Id, False);

      T.District_Ref.Add_Host (Host_Mock_Ref.all, Host_Added);
      Ass.Assert (Host_Added, "Host was not added");

      Travellers_Returned :=
         T.Host_Utils_Ref.Stop_Over (Host_Id, Pedestrian_Id);
      Ass.Assert (Host_Mock_Ref.Get_Stop_Over_Called,
                  "Utils did not request the host to make someone enter");
      Ass.Assert (Travellers = Travellers_Returned,
                  "Something different from a pedestrian is stopping");
   end Test_Pedestrian_Stops_Over;

   procedure Test_Vehicle_Stops_Over (T : in out Host_Utils_Test) is
      Vehicle : Vehicle_Mock.Reference := Vehicle_Mock.Create;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Added               : Boolean;
      Travellers : Agent_Id_List.List;
      Passenger1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Passenger2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (802);
      Passenger3_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (803);
      Passenger4_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (804);
      Utils : PC_Utils_Mock.Reference :=  PC_Utils_Mock.Reference (T.PC_Utils);
      Host_Mock_Ref : Host_Mock.Reference := Host_Mock.Create;
      Host_Id       : Infra_Id := 52;
      Host_Added    : Boolean;
      Travellers_Returned : Agent_Id_List.List;
   begin
      Vehicle.Set_Return_Value_For_Get_Id (Vehicle_Id);
      T.District_Ref.Add_Traveller (
         Traveller_Pkg.Reference (Vehicle), Added);

      Travellers.Append (Passenger1_Id);
      Travellers.Append (Passenger2_Id);
      Travellers.Append (Passenger3_Id);
      Travellers.Append (Passenger4_Id);

      Utils.Set_Value_For_Is_A_People_Carrier (Vehicle_Id, True);
      Utils.Set_List_For_Get_Passengers (Travellers);
      Utils.Set_Value_For_Free (Passenger1_Id, True);
      Utils.Set_Value_For_Free (Passenger2_Id, True);
      Utils.Set_Value_For_Free (Passenger3_Id, True);
      Utils.Set_Value_For_Free (Passenger4_Id, True);

      Host_Mock_Ref.Set_Return_Value_For_Get_Id (Host_Id);
      T.District_Ref.Add_Host (Host_Mock_Ref.all, Host_Added);
      Ass.Assert (Host_Added, "Host was not added");

      Travellers_Returned := T.Host_Utils_Ref.Stop_Over (Host_Id, Vehicle_Id);

      Ass.Assert (Host_Mock_Ref.Get_Stop_Over_Called,
                  "Utils did not request the host to make someone enter");
      Ass.Assert (Utils.Get_Free_Called,
                  "Free operation has not been called");
      Ass.Assert (Travellers = Travellers_Returned,
                  "Something different from passengers is stopping");
   end Test_Vehicle_Stops_Over;

   procedure Test_Exit_Building_Defer (T : in out Host_Utils_Test) is
      Host_Mock_Ref : Host_Mock.Reference := Host_Mock.Create;
      Host_Id       : Infra_Id := 53;
      Host_Added    : Boolean;
      Traveller_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Result_Action : Next_Action;
      Result_Id     : Agent.Agent_Id;
   begin
      Host_Mock_Ref.Set_Return_Value_For_Get_Id (Host_Id);
      Host_Mock_Ref.Set_Return_Values_For_Exit_Building (Vehicle_Id, DEFER);

      T.District_Ref.Add_Host (Host_Mock_Ref.all, Host_Added);
      Ass.Assert (Host_Added, "Host was not added");

      Result_Action :=
         T.Host_Utils_Ref.Exit_Building (Host_Id, Traveller_Id, Result_Id);

      Ass.Assert (Result_Action = DEFER,
         "Next action is not deferred");

      Ass.Assert (Result_Id = Vehicle_Id,
         "Traveller did not exit with the right vehicle");
   end Test_Exit_Building_Defer;

   procedure Test_Exit_Building_Retry (T : in out Host_Utils_Test) is
      Host_Mock_Ref : Host_Mock.Reference := Host_Mock.Create;
      Host_Id       : Infra_Id := 53;
      Host_Added    : Boolean;
      Traveller_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Result_Action : Next_Action;
      Result_Id     : Agent.Agent_Id;
   begin
      Host_Mock_Ref.Set_Return_Value_For_Get_Id (Host_Id);
      Host_Mock_Ref.Set_Return_Values_For_Exit_Building (Vehicle_Id, RETRY);

      T.District_Ref.Add_Host (Host_Mock_Ref.all, Host_Added);
      Ass.Assert (Host_Added, "Host was not added");

      Result_Action :=
         T.Host_Utils_Ref.Exit_Building (Host_Id, Traveller_Id, Result_Id);

      Ass.Assert (Result_Action = RETRY,
         "Next action is not deferred");

      Ass.Assert (Result_Id = Vehicle_Id,
         "Traveller did not exit with the right vehicle");
   end Test_Exit_Building_Retry;

   procedure Test_Exit_Building_Do_Not_Defer (T : in out Host_Utils_Test) is
      Host_Mock_Ref : Host_Mock.Reference := Host_Mock.Create;
      Host_Id       : Infra_Id := 53;
      Host_Added    : Boolean;
      Traveller_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);
      Vehicle_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (801);
      Result_Action : Next_Action;
      Result_Id     : Agent.Agent_Id;
   begin
      Host_Mock_Ref.Set_Return_Value_For_Get_Id (Host_Id);
      Host_Mock_Ref.Set_Return_Values_For_Exit_Building (
         Vehicle_Id, DO_NOT_DEFER);

      T.District_Ref.Add_Host (Host_Mock_Ref.all, Host_Added);
      Ass.Assert (Host_Added, "Host was not added");

      Result_Action :=
         T.Host_Utils_Ref.Exit_Building (Host_Id, Traveller_Id, Result_Id);

      Ass.Assert (Result_Action = DO_NOT_DEFER,
         "Next action is not deferred");

      Ass.Assert (Result_Id = Vehicle_Id,
         "Traveller did not exit with the right vehicle");
   end Test_Exit_Building_Do_Not_Defer;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Pedestrian_Stops_Over_Wrapper
      (T : in out Host_Utils_Test'Class)
   is
   begin
      Test_Pedestrian_Stops_Over (T);
   end Test_Pedestrian_Stops_Over_Wrapper;

   procedure Test_Vehicle_Stops_Over_Wrapper
      (T : in out Host_Utils_Test'Class)
   is
   begin
      Test_Vehicle_Stops_Over (T);
   end Test_Vehicle_Stops_Over_Wrapper;

   procedure Test_Exit_Building_Defer_Wrapper
      (T : in out Host_Utils_Test'Class)
   is
   begin
      Test_Exit_Building_Defer (T);
   end Test_Exit_Building_Defer_Wrapper;

   procedure Test_Exit_Building_Retry_Wrapper
      (T : in out Host_Utils_Test'Class)
   is
   begin
      Test_Exit_Building_Retry (T);
   end Test_Exit_Building_Retry_Wrapper;

   procedure Test_Exit_Building_Do_Not_Defer_Wrapper
      (T : in out Host_Utils_Test'Class)
   is
   begin
      Test_Exit_Building_Do_Not_Defer (T);
   end Test_Exit_Building_Do_Not_Defer_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Host_Utils_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Host_Utils_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Pedestrian_Stops_Over_Wrapper'Access,
         Name    => "Tests a pedestrian stopping with facility utils");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Vehicle_Stops_Over_Wrapper'Access,
         Name    => "Tests a vehicle stopping with Host.Utils");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Defer_Wrapper'Access,
         Name    => "Tests exiting out of a building with deferral");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Retry_Wrapper'Access,
         Name    => "Tests exiting out of a building with retry");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Exit_Building_Do_Not_Defer_Wrapper'Access,
         Name    => "Tests exiting out of a building without deferral");

   end Register_Tests;

   function Name(T : Host_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Decoration.Host.Utils");
   end Name;

end Reactive.Infrastructure.Building.Host.Utils.Tests;
