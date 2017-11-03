with AUnit.Assertions;

with Active.Agent;
with Active.Agent.Mock;
use Active.Agent;

package body Scheduling.Worker_Thread.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Worker            : Worker_Thread.Reference;
   Number_Of_Workers : Integer := 1;
   Queue             : Work_Queue.Reference;
   Controller        : Command_Broker.Controller_Reference;

   procedure Set_Up (T: in out Worker_Thread_Test) is
   begin
      Controller := new Command_Broker.Controller_For_Workers;
      Queue      := new Work_Queue.The_Queue;
      Worker     := new Worker_Thread.T;
      Controller.Init (Number_Of_Workers);
      Worker.Init (Queue, Controller);
   end Set_Up;

   procedure Tear_Down (T: in out Worker_Thread_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Can_Stop (T : in out TC.Test_Case'Class)
   is
      Poison_Pill  : Work_Queue_Pkg.Work_Queue_Item;
   begin
      Poison_Pill.Shutdown_Notification := True;

      Queue.Add_Item (Poison_Pill);

      Controller.Wait_For_Workers;
   end Test_Can_Stop;

   procedure Test_Can_Execute_And_Stop (T : in out TC.Test_Case'Class)
   is
      Poison_Pill : Work_Queue_Pkg.Work_Queue_Item;
      Work_Item   : Work_Queue_Pkg.Work_Queue_Item;
      Agent       : aliased Active.Agent.Reference := Active.Agent.Mock.Create;
      Agent_Mock  : Active.Agent.Mock.Reference
         := Active.Agent.Mock.Reference(Agent);
   begin
      Work_Item.Action := Agent;

      Queue.Add_Item (Work_Item);
      Poison_Pill.Shutdown_Notification := True;
      Queue.Add_Item (Poison_Pill);

      Ass.Assert (Agent_Mock.Get_Act_Called, "Act has not been called");

      Controller.Wait_For_Workers;
   end Test_Can_Execute_And_Stop;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Worker_Thread_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Can_Stop'Access,
                        Name    => "Test can stop");

      Register_Routine (Test    => T,
                        Routine => Test_Can_Execute_And_Stop'Access,
                        Name    => "Test can execute and then stop");
   end Register_Tests;

   function Name(T: Worker_Thread_Test) return AU.Message_String is
   begin
      return AU.Format ("Worker Thread");
   end Name;
end Scheduling.Worker_Thread.Tests;
