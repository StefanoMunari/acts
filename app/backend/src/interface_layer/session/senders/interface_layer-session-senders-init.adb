separate (Interface_Layer.Session.Senders)
procedure Init (
   Hostname : in String;
   Port     : in G_Socket.Port_Type)
is
begin
   Sender_Hostname := SU.To_Unbounded_String (Hostname);
   Server_Socket.Port := Port;
   Sender_State := PT.READY;
   -- create the sender task
   Sender_Ref := new Sender;
end Init;
