-- local
with Interface_Layer.Presentation.JSON_Format;
with Interface_Layer.Presentation.XFormat;
with Interface_Layer.Containers.Queues;
-- core
with Ada.Unchecked_Deallocation;
-- DEBUG
-- Termination
with GNAT.OS_Lib;

package body Interface_Layer.Session.Senders is
   package JSON_Format renames  Interface_Layer.Presentation.JSON_Format;
   use Interface_Layer.Containers.Queues; -- view queues instances

   -- TODO: implement scoped_pointers
   procedure Free is new  Ada.Unchecked_Deallocation (Sender, Reference);

   procedure Init (Hostname : in String; Port : in G_Socket.Port_Type) is separate;
   procedure Start is separate;
   procedure Stop is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Sender is
      use PT; -- make '=' visible for Process_Types
      -- TODO: implement scoped_pointers
      procedure Free is new  Ada.Unchecked_Deallocation
         (JSON_Format.Object'Class, JSON_Format.Reference);
   begin
      select
         accept Send;
            declare
               Socket   : G_Socket.Socket_Type;
               Message  : JSON_Format.Reference;
               Channel  : G_Socket.Stream_Access;
               Hostname : String := SU.To_String (Sender_Hostname);
            begin
               while Sender_State = PT.ACTIVE loop
               -- when ready then execute
                  Sender_Message_Queue.Dequeue (
                     Interface_Layer.Presentation.XFormat.Reference (Message));
               -- DEBUG
                  G_Socket.Create_Socket (Socket);
                  G_Socket.Set_Socket_Option
                     (Socket, G_Socket.Socket_Level,
                        (G_Socket.Reuse_Address, True));
               -- If the sender's socket is not bound,
               -- Connect_Socket will bind to an unused address.
               -- The sender uses Connect_Socket to create a logical
               -- connection between its socket (Socket) and the server's
               -- socket (Server_Socket)
                  Server_Socket.Addr :=
                     G_Socket.Addresses (
                        G_Socket.Get_Host_By_Name (Hostname), 1);
                  G_Socket.Connect_Socket (Socket, Server_Socket);
               -- DEBUG
                  Channel := G_Socket.Stream (Socket);
               -- DEBUG
               -- DEBUG
               --   Send message
                  String'Output (Channel, JSON_Format.Write
                     (JSON_Format.Object (Message.all)));
               -- DEBUG
               -- DEBUG
               -- Free resources
                  G_Socket.Close_Socket (Socket);
               -- Eventually change Receiver state to STOPPED.
               -- Thus, Receiver will complete its loop waiting for each
               --+ executing worker to complete its job and then it will
               --+ terminate gracefully
                  declare
                     Action_Header : JSON_Format.Object :=
                        JSON_Format.Object (Message.Get_Header);
                  begin
                     if Is_Shutdown (Action_Header.Get ("Request"))
                     then
                        Sender_State := PT.STOPPED;
                     end if;
                  end;
                  Free (Message);
               end loop;
            -- otherwise terminate
               Senders.Shutdown;
            end;
         or
            terminate;
         end select;
   end Sender;

   -- -- Free resources
   -- Free (Sender_Ref);

end Interface_Layer.Session.Senders;