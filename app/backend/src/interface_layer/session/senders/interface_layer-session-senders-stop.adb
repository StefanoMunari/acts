separate (Interface_Layer.Session.Senders)
procedure Stop is
begin
   -- stop the sender task
   Sender_State := PT.STOPPED;
end Stop;
