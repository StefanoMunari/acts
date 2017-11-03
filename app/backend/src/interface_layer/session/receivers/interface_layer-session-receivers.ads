------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::inteface_layer::session::receivers
-- @purpose use a pool of executors to satisfy remote TCP requests
--          and forward each request to the upper sub-layer
-- @interface
-- @dependencies -
-- @details -
------------------------------------------------------------------------------
with Interface_Layer.Containers.Stacks;
with Interface_Layer.Presentation.JSON_Format;
-- with Interface_Layer.Presentation.JSON_Format;
with Shared.Process_Types;
-- library
with GNAT.Sockets;

package Interface_Layer.Session.Receivers is

   package PT renames Shared.Process_Types;
   package Stacks renames Interface_Layer.Containers.Stacks;
   package G_Socket renames GNAT.Sockets;
   package JSON_Format renames Interface_Layer.Presentation.JSON_Format;

   procedure Init (Hostname : String; Port : G_Socket.Port_Type;
      Task_Pool_Size : Stacks.Stack_Range);
   procedure Start;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Receiver is
      entry Listen;
   end Receiver;
   type Reference is access Receiver;

   -- static data fields
   Address        : G_Socket.Sock_Addr_Type;
   Server         : G_Socket.Socket_Type;
   Stack_Instance : Stacks.Stack;
   Receiver_State : PT.Process_T := PT.TERMINATED;
   Receiver_Ref   : Reference := NULL;
   Poison_Pill    : JSON_Format.Reference := new JSON_Format.Object;

end Interface_Layer.Session.Receivers;