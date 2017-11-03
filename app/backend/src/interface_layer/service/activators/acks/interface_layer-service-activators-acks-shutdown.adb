separate (Interface_Layer.Service.Activators.Acks)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   Ack_State := PT.TERMINATED;
end Shutdown;
