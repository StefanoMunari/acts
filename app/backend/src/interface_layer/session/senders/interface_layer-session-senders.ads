------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::inteface_layer::session::senders
-- @purpose send messages coming from the application-layer towards a specified
--          host and port
-- @interface
-- @dependencies -
-- @details -
------------------------------------------------------------------------------
with Ada.Strings.Unbounded;
-- library
with GNAT.Sockets;

with Shared.Process_Types;

package Interface_Layer.Session.Senders is

   package SU       renames Ada.Strings.Unbounded;
   package PT       renames Shared.Process_Types;
   package G_Socket renames GNAT.Sockets;

   procedure Init (Hostname : in String; Port : in G_Socket.Port_Type);
   procedure Start;
   procedure Stop;
   procedure Shutdown;

   Shutdown_Completed : exception;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Sender is
      entry Send;
   end Sender;
   type Reference is access Sender;

   -- static data fields
   Server_Socket   : G_Socket.Sock_Addr_Type;
   Sender_State    : PT.Process_T := PT.TERMINATED;
   Sender_Ref      : Reference := NULL;
   Sender_Hostname : SU.Unbounded_String := SU.To_Unbounded_String ("");

end Interface_Layer.Session.Senders;
