-- core
with Ada.Unchecked_Deallocation;
-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Containers.Pair;
-- callbacks
with Shared.Callback_Pair;
with Shared.Callback_Map;
with Shared.Callback_Map_Utils;
with Interface_Layer.Tables.Pending_Request;

package body Interface_Layer.Service.Activators.Acks.Executors is
   use Interface_Layer.Containers.Queues; -- view queues
   package Pair_Pkg               renames Interface_Layer.Containers.Pair;
   package Callback_Pair_Pkg      renames Shared.Callback_Pair;
   package Pending_Request_Pkg
      renames Interface_Layer.Tables.Pending_Request;

   procedure Free is new
      Ada.Unchecked_Deallocation (
         Callback_Pair_Pkg.Object'Class, Callback_Pair_Pkg.Reference);

   task body Executor is
      use Shared.Callback_Map_Utils;
      Item           : Pair_Pkg.Object;
      Executor_Index : Stacks.Stack_Range;
      Callback_Pair  : Callback_Pair_Pkg.Object;
      Callback_Found : Boolean := False;
      Ack            : Boolean;
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
         Activator_Ack_Queue.Dequeue (Item);
      -- Eventually change Acks state to STOPPED.
      -- Thus, Acks will complete its loop waiting for each executing
      -- worker to complete its job and then it will terminate gracefully
         if Acks.Is_Shutdown (Item.First.Element ("Request"))
         then
            Acks.Ack_State := PT.STOPPED;
         else
         -- DEBUG
         -- DEBUG
         -- Get the Callback_Pair object from the Ack_Table
            Pending_Request_Pkg.Table.Find_And_Delete (
               Item.First.Element ("Request_Id"),
               Callback_Pair, Callback_Found);

            if Callback_Found then

            -- Answer the pending request with True (Ack) || False (Nack)
               Ack := Item.Second.Get_Data;
               if Ack then
                  Callback_Pair.Success.Execute;
               else
                  Callback_Pair.Failure.Execute;
               end if;

            end if;
         end if;
      -- insert in the task_pool (task completed)
         Ack_Stack_Instance.Push (Executor_Index);
      end loop;
   end Executor;

end Interface_Layer.Service.Activators.Acks.Executors;
