separate (Interface_Layer.Session.Senders)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   Sender_State := PT.TERMINATED;
end Shutdown;
