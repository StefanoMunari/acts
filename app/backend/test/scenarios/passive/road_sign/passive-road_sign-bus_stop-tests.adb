with Ada.Text_IO;
with AUnit.Assertions;

with Active.Agent;
with Active.Bus_Service.Mock;
with Active.Bus_Service.Utils.Mock;

with Passive.Road_Sign.Bus_Stop;

with Reactive.Infrastructure.Street.Mock;
with Reactive.Infrastructure.Way.Footway.Mock;
with Reactive.Infrastructure.Lane.Footway_Lane.Mock;
with Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;

with Shared.Direction;

package body Passive.Road_Sign.Bus_Stop.Tests is
   package IO renames Ada.Text_IO;
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Bus_Service_Utils_Mock renames Active.Bus_Service.Utils.Mock;
   package Bus_Service renames Active.Bus_Service.Mock;
   package Bus_Stop renames Passive.Road_Sign.Bus_Stop;
   package Street renames Reactive.Infrastructure.Street.Mock;
   package Footway renames Reactive.Infrastructure.Way.Footway.Mock;
   package Footway_Lane renames Reactive.Infrastructure.Lane.Footway_Lane.Mock;
   package Footway_Stretch
   renames Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;
   package Direction renames Shared.Direction;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Bus_Service_Ref     : Bus_Service.Reference;
   Street_Ref          : Street.Reference;
   Footway_Ref         : Footway.Reference;
   Footway_Lane_Ref    : Footway_Lane.Reference;
   Footway_Stretch_Ref : Footway_Stretch.Reference;
   Bus_Stop_Ref        : Bus_Stop.Reference;
   Bus_Utils           : Bus_Service_Utils_Mock.Reference;
   Bus_Service_Id      : Agent.Agent_Id;
   Bus_Ids             : Agent_Id_List.List := Agent_Id_List.Empty_List;
   Waiting_Ids         : Agent_Id_To_Agent_Id_List_Map.Map :=
      Agent_Id_To_Agent_Id_List_Map.Empty_Map;
   Stops               : Agent_Id_To_Infra_Id_List_Map.Map :=
      Agent_Id_To_Infra_Id_List_Map.Empty_Map;
   Bus1_Id             : Agent.Agent_Id
      := Agent.Create_Id_From_Natural (44);

   procedure Set_Up_Case (T: in out Bus_Stop_Test) is
   begin
      null;
   end Set_Up_Case;

   procedure Set_Up (T: in out Bus_Stop_Test) is
   begin
      Street_Ref          := Street.Create;
      Footway_Ref         := Footway.Create;
      Footway_Lane_Ref    := Footway_Lane.Create;
      Footway_Stretch_Ref := Footway_Stretch.Create;

      Bus_Utils              := Bus_Service_Utils_Mock.Create;
      Bus_Service_Ref        := Bus_Service.Create;
      Bus_Service_Id         := Agent.Create_Id_From_Natural (42);
      Bus_Utils.Set_Traveller_For_On_Bus_Stop (
         Bus_Service_Id, Bus_Service_Ref);

      Bus_Ids.Append (Bus1_Id);
      Bus_Stop_Ref := Road_Sign.Bus_Stop.Create (
         Buses             => Bus_Ids,
         Waiting_Ids       => Waiting_Ids,
         Stops             => Stops,
         Bus_Service_Utils => Bus_Utils);
   end Set_Up;

   procedure Tear_Down (T: in out Bus_Stop_Test) is
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Bus_Stop_Test) is
   begin
      null;
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Bus_Service_On_Bus_Stop (T: in out TC.Test_Case'Class) is
   begin
   -- TODO: All the stuff like adding pedestrian ref or something similar
      IO.Put_Line ("Ada is on the bus stop");
      Bus_Stop_Ref.Apply (Bus_Service_Id);
      Ass.Assert (Bus_Service_Ref.Get_On_Bus_Stop_Called,
                  "On_Bus_Stop has not been called");
   end Bus_Service_On_Bus_Stop;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Bus_Stop_Test) is
      use TC.Registration;
   begin
      null;
      Register_Routine (Test => T,
                        Routine => Bus_Service_On_Bus_Stop'Access,
                        Name => "Bus_Service goes on Bus Stop");
   end Register_Tests;

   function Name(T: Bus_Stop_Test) return AU.Message_String is
   begin
      return AU.Format ("Bus_Stop");
   end Name;
end Passive.Road_Sign.Bus_Stop.Tests;
