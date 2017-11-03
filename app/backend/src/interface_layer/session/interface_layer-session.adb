-- local
with Interface_Layer.Session.Receivers;
with Interface_Layer.Session.Senders;
package body Interface_Layer.Session is

   procedure Init
         (Application_Address : in String; Application_Port : in G_Socket.Port_Type;
         Middleware_Address : in String; Middleware_Port : in G_Socket.Port_Type;
         Receivers_Pool_Size : Stacks.Stack_Range) is
   begin
      Senders.Init (Middleware_Address, Middleware_Port);
      Receivers.Init (Application_Address, Application_Port, Receivers_Pool_Size);
   end Init;

   procedure Start is
   begin
      Senders.Start;
      Receivers.Start;
   end Start;

   procedure Shutdown is
   begin
      -- Receivers has already been terminated
      -- Senders.Stop allows the sender to empty its queue before
      -- calling Shutdown on itself (i.e., shutdown gracefully)
      Senders.Stop;
   end Shutdown;

end Interface_Layer.Session;
