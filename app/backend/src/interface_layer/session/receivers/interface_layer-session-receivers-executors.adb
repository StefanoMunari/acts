-- core
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
-- local
with Active;

with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.XFormat;
with Interface_Layer.Presentation.JSON_Format;
-- DEBUG
-- DEBUG

package body Interface_Layer.Session.Receivers.Executors is

   package XFormat     renames Interface_Layer.Presentation.XFormat;
   package JSON_Format renames Interface_Layer.Presentation.JSON_Format;

   task body Socket_Executor is
      Executor_Connection : G_Socket.Socket_Type;
      Executor_Client     : G_Socket.Sock_Addr_Type;
      Executor_Channel    : G_Socket.Stream_Access;
      Executor_Index      : Stacks.Stack_Range;
      Valid_JSON          : Boolean;
   begin
      loop

         select
            accept Shutdown;
         or
            accept Setup (Connection   : G_Socket.Socket_Type;
                          Client       : G_Socket.Sock_Addr_Type;
                          Channel      : G_Socket.Stream_Access;
                          Task_Index   : Stacks.Stack_Range)
            do
            -- Store parameters and mark task busy
               Executor_Connection := Connection;
               Executor_Client     := Client;
               Executor_Channel    := Channel;
               Executor_Index      := Task_Index;
            end;
         end select;

            accept Exec;
         declare
            use Interface_Layer.Containers.Queues; -- make queues visible
            Message     : JSON_Format.Reference := new JSON_Format.Object;
            Aux_Message : JSON_Format.Object    := JSON_Format.Create_Object;
            Input_SU    : SU.Unbounded_String   := SU.To_Unbounded_String ("");
         begin
            Valid_JSON := False;
            while not Valid_JSON loop
               declare
               begin
                  null;
               declare
                  Input_String : String := String'Input (Executor_Channel);
                  Aux_Object   : JSON_Format.Object;
               begin -- Read_From_Stream
                  SU.Append (Input_SU, SU.To_Unbounded_String (Input_String));
                  Aux_Object := JSON_Format.Read (SU.To_String (Input_SU));
                  Valid_JSON := True;
               end;
               exception
                  when others =>
                     null;
               end;
            end loop;
            Aux_Message := JSON_Format.Read (SU.To_String (Input_SU));

            Message.all := JSON_Format.Object'Class (Aux_Message);
         -- DEBUG
         -- DEBUG
         -- Eventually change Receiver state to STOPPED.
         -- Thus, Receiver will complete its loop waiting for each executing
         -- worker to complete its job and then it will terminate gracefully
         -- after having inserted the last message in the Decoder Queue
            declare
               Header : JSON_Format.Object :=
                  JSON_Format.Object (Aux_Message.Get_Header);
            begin
               if not Is_Shutdown (JSON_Format.Write (Header.Get ("Request")))
               then
                  Decoder_Message_Queue.Enqueue (XFormat.Reference (Message));
               else
                  Poison_Pill.all := JSON_Format.Object'Class (Aux_Message);
                  Receiver_State := PT.STOPPED;
               end if;
            end;
         -- Eventually change Receiver state to STOPPED.
         -- Thus, Receiver can complete its loop and
         -- terminate gracefully (all executors terminated)
         -- DEBUG
         end;

      -- close socket (communication completed)
         G_Socket.Close_Socket (Executor_Connection);
      -- insert in the task_pool (task completed)
         Stack_Instance.Push (Executor_Index);
      end loop;
   end Socket_Executor;

end Interface_Layer.Session.Receivers.Executors;
