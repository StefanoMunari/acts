separate (Interface_Layer.Service.Activators.Acks)
procedure Init (
   Pool_Size : Stacks.Stack_Range)
is
begin
   Ack_Stack_Instance.Init (Pool_Size);
   -- the task is now ready to execute
   Ack_State := PT.READY;
   -- create the task
   Ack_Ref   := new Ack;
end Init;
