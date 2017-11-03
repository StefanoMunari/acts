separate (Interface_Layer.Service.Activators.Requests)
procedure Init (
   Pool_Size : Stacks.Stack_Range)
is
begin
   Req_Stack_Instance.Init (Pool_Size);
   -- the task is now ready to execute
   Request_State := PT.READY;
   -- create the task
   Request_Ref := new Request;
end Init;
