with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Space_Master.Mock;
with Active.Travel.Mock;
with Active.Travel.Travel_Completed.Mock;

with Reactive.Infrastructure.Utils.Mock;

with Shared.Slice;

package body Active.Travel.Travel_Progress.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Space_Master_Mock renames Active.Space_Master.Mock;
   package Travel_Mock renames Active.Travel.Mock;
   package Travel_Completed_Mock renames Active.Travel.Travel_Completed.Mock;
   package Infrastructure_Utils_Mock
      renames Reactive.Infrastructure.Utils.Mock;
   package Slice renames Shared.Slice;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Travel : Travel_Mock.Reference;
   Travel_Completed : Travel_Completed_Mock.Reference;
   Infrastructure_Utils : Infrastructure_Utils_Mock.Reference;
   Travel_Progress : Active.Travel.Travel_Progress.Reference;
      Space_Master : access Space_Master_Mock.Object;

   procedure Set_Up (T: in out Travel_Progress_Test) is
   begin
      Travel := Travel_Mock.Create;
      Travel_Completed := Travel_Completed_Mock.Create;
      Infrastructure_Utils := Infrastructure_Utils_Mock.Create;
      Space_Master := Space_Master_Mock.Create;
      Travel_Progress := Active.Travel.Travel_Progress.Get_Instance (
         Travel_Completed     => Travel_Completed,
         Infrastructure_Utils => Infrastructure_Utils,
         Space_Master    => Space_Master);
   end Set_Up;

   procedure Test_Has_Next_Step (T: in out TC.Test_Case'Class)
   is
      Current_Step_Id : Infra_Id := 234;
      Last_Step_Id    : Infra_Id := 245;
   begin
      Travel.Set_Return_Value_For_Get_Current_Step_Id (Current_Step_Id);
      Travel.Set_Return_Value_For_Get_Last_Step_Id (Last_Step_Id);

      Ass.Assert (Travel_Progress.Has_Next_Step (Travel.all),
                  "The travel has no next step");
   end Test_Has_Next_Step;

   procedure Test_Is_Progressing (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (Travel_Progress.Is_Progressing (Travel.all),
                  "The travel is not progressing");
   end Test_Is_Progressing;

-- TODO: Check this test after slice modification
   procedure Test_Advance (T: in out TC.Test_Case'Class)
   is
      Traveller_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (74453);
      Previous_Step_Id     : Infra_Id := 343768;
      Current_Step_Id      : Infra_Id := 34353;
      Route_Destination    : Slice.Map := Slice.Empty_Map;
      Route_Destination_Id : Infra_Id := 43534;
   begin
      Travel.Change_Travel_State (Travel_Progress);
      Ass.Assert (Travel.Get_Travel_State = Travel_Progress,
                  "The travel state is not 'Travel_Progress'");

      Travel.Set_Return_Value_For_Get_Traveller_Id (Traveller_Id);
      Infrastructure_Utils.Set_Return_Value_For_Tread (FALSE);

      Travel.Set_Return_Value_For_Get_Previous_Step (Previous_Step_Id);
      Travel.Set_Return_Value_For_Get_Current_Step_Id (Current_Step_Id);

      Travel.Set_Return_Value_For_Get_Route_Destination (Route_Destination);

      Travel_Progress.Advance (Travel.all);

      Ass.Assert (Travel.Get_Travel_State = Travel_Completed,
                  "The travel state is not 'Travel_Completed'");
   end Test_Advance;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Travel_Progress_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Has_Next_Step'Access,
                        Name => "Test has next step");

      Register_Routine (
         Test => T,
         Routine => Test_Is_Progressing'Access,
         Name => "Test is progressing");

      Register_Routine (
         Test => T,
         Routine => Test_Advance'Access,
         Name => "Test advance");
   end Register_Tests;

   function Name(T: Travel_Progress_Test) return AU.Message_String is
   begin
      return AU.Format ("Travel_Progress");
   end Name;
end Active.Travel.Travel_Progress.Tests;
