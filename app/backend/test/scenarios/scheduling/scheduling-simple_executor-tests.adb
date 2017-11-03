with AUnit.Assertions;

with Active.Agent.Mock;
use Active.Agent;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Remote.Stub.Mock;

package body Scheduling.Simple_Executor.Tests is
   package Ass            renames AUnit.Assertions;
   package Work_Queue_Pkg renames Scheduling.Work_Queue;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Executor : Simple_Executor.Reference;

   procedure Set_Up (T: in out Simple_Executor_Test) is
   begin
      Executor := new Simple_Executor.Object;
   end Set_Up;

   procedure Tear_Down (T: in out Simple_Executor_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Execute_One (T : in out TC.Test_Case'Class)
   is
      Agent      : aliased Active.Agent.Reference := Active.Agent.Mock.Create;
      Agent_Mock : Active.Agent.Mock.Reference
         := Active.Agent.Mock.Reference(Agent);
   begin
      Agent_Mock.Set_Id (Active.Agent.Create_Id_From_Natural (37));

      Executor.Init (Real_Time.Milliseconds (0), 3);
      Executor.Execute (Agent);

      Ass.Assert (Agent_Mock.Get_Act_Called,
                  "Act has not been called");

      Executor.Shutdown;
   end Test_Execute_One;

   procedure Test_Execute_More (T : in out TC.Test_Case'Class)
   is
      Agent_1 : aliased Active.Agent.Reference := Active.Agent.Mock.Create;
      Agent_Mock_1 : Active.Agent.Mock.Reference
                  := Active.Agent.Mock.Reference(Agent_1);
      Agent_2 : aliased Active.Agent.Reference := Active.Agent.Mock.Create;
      Agent_Mock_2 : Active.Agent.Mock.Reference
                  := Active.Agent.Mock.Reference(Agent_2);
   begin
      Agent_Mock_1.Set_Id (Active.Agent.Create_Id_From_Natural (1));
      Agent_Mock_2.Set_Id (Active.Agent.Create_Id_From_Natural (2));
      Executor.Init (Real_Time.Milliseconds (0), 1);

      Executor.Execute (Agent_1);
      Executor.Execute (Agent_2);

      Ass.Assert (Agent_Mock_1.Get_Act_Called,
                  "Act has not been called on Agent #1");
      Ass.Assert (Agent_Mock_2.Get_Act_Called,
                  "Act has not been called on Agent #2");

      Executor.Shutdown;
   end Test_Execute_More;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Simple_Executor_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Execute_One'Access,
                        Name    => "Test execute one item");
      Register_Routine (Test    => T,
                        Routine => Test_Execute_More'Access,
                        Name    => "Test execute more than one item");
   end Register_Tests;

   function Name(T: Simple_Executor_Test) return AU.Message_String is
   begin
      return AU.Format ("Simple Executor");
   end Name;
end Scheduling.Simple_Executor.Tests;
