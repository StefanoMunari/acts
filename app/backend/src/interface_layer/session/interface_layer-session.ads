------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::inteface_layer::session
-- @purpose manage the session layer. This layer handles TCP connections
--          and send messages to towards a specified host
-- @interface
-- @dependencies -
-- @details -
------------------------------------------------------------------------------
-- local
with  Interface_Layer.Containers.Stacks;
-- library
with GNAT.Sockets;

package Interface_Layer.Session is

   package Stacks renames Interface_Layer.Containers.Stacks;
   package G_Socket renames GNAT.Sockets;

   procedure Init
         (Application_Address : in String;
         Application_Port : in G_Socket.Port_Type;
         Middleware_Address : in String;
         Middleware_Port : in G_Socket.Port_Type;
         Receivers_Pool_Size : Stacks.Stack_Range);
   procedure Start;
   procedure Shutdown;

end Interface_Layer.Session;
