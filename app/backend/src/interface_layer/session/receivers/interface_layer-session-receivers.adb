with Interface_Layer.Session.Receivers.Executors;
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.XFormat;

with Shared.String_Splitter;

-- DEBUG
-- DEBUG

package body Interface_Layer.Session.Receivers is

   package XFormat     renames Interface_Layer.Presentation.XFormat;
   use Interface_Layer.Containers.Queues;

   procedure Init (
      Hostname       : String;
      Port           : G_Socket.Port_Type;
      Task_Pool_Size : Stacks.Stack_Range) is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

-- private
   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Receiver is
      Client     : G_Socket.Sock_Addr_Type;
      Task_Index : Stacks.Stack_Range;
   begin
      select
      accept Listen;
         declare
         -- make Executors completely visible
            use Interface_Layer.Session.Receivers.Executors;
            use PT; -- make '=' visible for Process_Types
            Workers : array (1.. Stack_Instance.Get_Size) of Socket_Executor;
        begin
         -- wait for a connection and delegate it to the next free task of
         -- the task_pool
            Receive:
         -- when Ready execute, otherwise exit and terminate
            while Receiver_State = PT.ACTIVE loop
               declare
                  Connection : G_Socket.Socket_Type;
                  Channel    : G_Socket.Stream_Access;
               begin
               -- accept a connection from a client
                  G_Socket.Accept_Socket (
                     Server  => Server,
                     Socket  => Connection,
                     Address => Client);
               -- DEBUG
               -- DEBUG
               -- get a stream/channel from the client's connection
                  Channel := G_Socket.Stream (Connection);
               -- get a task from the task pool
               -- if there are no tasks available then wait on the Pop entry
               -- Stack_Instance is a Protected_Object
                  Stack_Instance.Pop (Task_Index);
               -- assign the socket to the task in RENDEZVOUS
                  Workers (Task_Index).Setup (
                     Connection,Client, Channel, Task_Index);
               -- run the ASYNC task which handles the socket
                  Workers (Task_Index).Exec;
               end;
            end loop Receive;
            for Worker of Workers loop
               Worker.Shutdown;
            end loop;
          -- terminate
          -- Note: this point will be reached when all the workers have
          -- complete their jobs
            Decoder_Message_Queue.Enqueue (XFormat.Reference (Poison_Pill));
            Receivers.Shutdown;
         end;
      or
         terminate;
      end select;
   end Receiver;

--   begin
-- -- Free resources
--   -- Close the socket associated with the listening server
--   G_Socket.Close_Socket (Server);
--   -- Call when GNAT.Sockets are closed
--   G_Socket.Finalize;

end Interface_Layer.Session.Receivers;
