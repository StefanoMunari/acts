with Scheduling.Work_Queue.Tests;
with Scheduling.Worker_Thread.Tests;
with Scheduling.Simple_Executor.Tests;
with Scheduling.Scheduler.Tests;

package body Scheduling_Suite is
   Result               : aliased TS.Test_Suite;
   Work_Queue_Test      :
      aliased Scheduling.Work_Queue.Tests.Work_Queue_Test;
   Worker_Thread_Test   :
      aliased Scheduling.Worker_Thread.Tests.Worker_Thread_Test;
   Simple_Executor_Test :
      aliased Scheduling.Simple_Executor.Tests.Simple_Executor_Test;
   Scheduler_Test       :
      aliased Scheduling.Scheduler.Tests.Scheduler_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Work_Queue_Test'Access);
      TS.Add_Test (Result'Access, Worker_Thread_Test'Access);
      TS.Add_Test (Result'Access, Simple_Executor_Test'Access);
      TS.Add_Test (Result'Access, Scheduler_Test'Access);

      return Result'Access;
   end Suite;
end Scheduling_Suite;
