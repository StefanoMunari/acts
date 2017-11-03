-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Containers.Pair;

package body Interface_Layer.Service.Activators.Requests.Executors is
   use Interface_Layer.Containers.Queues; -- view queues
   package Pair renames Interface_Layer.Containers.Pair;

   task body Executor is
      Item           : Pair.Object;
      Executor_Index : Stacks.Stack_Range;
   begin
      loop

         accept Init (Task_Index : Stacks.Stack_Range)
         do
         -- Store parameters and mark task busy
            Executor_Index := Task_Index;
         end;

         select
            accept Shutdown;
         or
            accept Exec;
         end select;
         Activator_Request_Queue.Dequeue (Item);
      -- DEBUG
      -- DEBUG
         Dispatcher.Dispatch (Item.First.Element ("Request"), Item.Second);
      -- Eventually change Requests state to STOPPED.
      -- Thus, Requests will complete its loop waiting for each executing
      -- worker to complete its job and then it will terminate gracefully
         if Requests.Is_Shutdown (Item.First.Element ("Request"))
         then
            Requests.Request_State := PT.STOPPED;
         end if;
      -- insert in the task_pool (task completed)
         Req_Stack_Instance.Push (Executor_Index);

      end loop;
   end Executor;

end Interface_Layer.Service.Activators.Requests.Executors;
