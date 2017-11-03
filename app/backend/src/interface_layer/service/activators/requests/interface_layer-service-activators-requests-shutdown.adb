separate (Interface_Layer.Service.Activators.Requests)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   Request_State := PT.TERMINATED;
end Shutdown;
