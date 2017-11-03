with Active.Agent;

package body Scheduling.Worker_Thread is

   task body T
   is
      Controller        : Command_Broker.Controller_Reference;
      Queue             : Work_Queue_Pkg.Reference;
      Stop              : Boolean := False;
      Work_Item         : Work_Queue_Pkg.Work_Queue_Item;
   begin
   -- receive initialization from executor
   -- init worker with rendezvous object and queue
      accept Init (Q : in out Work_Queue.Reference;
                   C : in     Command_Broker.Controller_Reference)
      do
         Controller := C;
         Queue      := Q;
      end Init;

      Queue.Take_Item (Work_Item);

      while not Work_Item.Shutdown_Notification loop
         Work_Item.Action.Act;
         Queue.Take_Item (Work_Item);
      end loop;

      Controller.Worker_Done;

   end;

end Scheduling.Worker_Thread;
