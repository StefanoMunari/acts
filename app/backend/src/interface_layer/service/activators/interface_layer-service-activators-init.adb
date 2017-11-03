separate (Interface_Layer.Service.Activators)
procedure Init (
  Req_Pool_Size : Stacks.Stack_Range;
  Ack_Pool_Size : Stacks.Stack_Range)
is
begin
   Requests_Pkg.Init (Req_Pool_Size);
   Acks_Pkg.Init (Ack_Pool_Size);
   Interface_Dispatcher.Init;
end Init;
