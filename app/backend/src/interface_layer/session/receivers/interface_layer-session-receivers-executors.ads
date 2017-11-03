------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::inteface_layer::session::
-- @purpose
-- @interface
-- @dependencies -
-- @details -
------------------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Interface_Layer.Containers.Stacks;
-- library
with GNAT.Sockets;

package Interface_Layer.Session.Receivers.Executors is

   package SU          renames Ada.Strings.Unbounded;
   package Stacks renames Interface_Layer.Containers.Stacks;
   package G_Socket renames GNAT.Sockets;

   task type Socket_Executor is
     -- rendezvous entry (sets parameters for Exec)
     entry Setup (Connection : G_Socket.Socket_Type;
                  Client     : G_Socket.Sock_Addr_Type;
                  Channel    : G_Socket.Stream_Access;
                  Task_Index : Stacks.Stack_Range);
     -- async entry (NO RENDEZVOUS)
     entry Exec;
     entry Shutdown;
   end Socket_Executor;

end Interface_Layer.Session.Receivers.Executors;