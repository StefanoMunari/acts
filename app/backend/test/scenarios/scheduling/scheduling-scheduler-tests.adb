with AUnit.Assertions;

with Active.Agent;
with Active.Agent.Mock;
with Active.Traffic_Light.Utils.Mock;

with Reactive.District.Mock;

package body Scheduling.Scheduler.Tests is
   package Ass renames AUnit.Assertions;
   package TL_Utils_Mock_Pkg renames Active.Traffic_Light.Utils.Mock;
   package District_Pkg      renames Reactive.District;
   package District_Mock_Pkg renames District_Pkg.Mock;

   District_Ref          : District_Pkg.Reference;
   TL_Utils_Mock         : TL_Utils_Mock_Pkg.Reference;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Scheduler_Test) is
   begin
      District_Ref := District_Pkg.Reference (District_Mock_Pkg.Create);
   end Set_Up;

   procedure Set_Up_Case (T: in out Scheduler_Test) is
   begin -- Set_Up_Case
      TL_Utils_Mock := TL_Utils_Mock_Pkg.Create;
      Scheduler.Instance.Start (
         District => District_Ref,
         Epoch    => Real_Time.Milliseconds (0),
         Traffic_Light_Utils_Arg => TL_Utils_Mock);
   end Set_Up_Case;

   procedure Tear_Down (T: in out Scheduler_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Schedule (T : in out TC.Test_Case'Class)
   is
      Agent              : aliased Active.Agent.Reference
         := Active.Agent.Mock.Create;
      Agent_Mock         : Active.Agent.Mock.Reference
         := Active.Agent.Mock.Reference(Agent);
      Id                 : Active.Agent.Agent_Id
         := Active.Agent.Create_Id_From_Natural (42);
      Has_Been_Scheduled : Boolean := False;
      Shutdown_Ok        : Boolean := False;
   begin
      Agent_Mock.Set_Id (Id);
      TL_Utils_Mock.Set_Value_For_Is_A_Traffic_Light (Id, False);

      Scheduler.Instance.Schedule (Agent, 1.0, Has_Been_Scheduled);

      Ass.Assert (
         Has_Been_Scheduled,
         "Agent has not been scheduled");
      Ass.Assert (Agent_Mock.Get_Act_Called,
                  "Act has not been called on scheduled Agent");

      Scheduler.Instance.Shutdown;
   end Test_Schedule;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Scheduler_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Schedule'Access,
                        Name    => "Test execute schedule");
   end Register_Tests;

   function Name(T: Scheduler_Test) return AU.Message_String is
   begin
      return AU.Format ("Scheduler");
   end Name;
end Scheduling.Scheduler.Tests;
