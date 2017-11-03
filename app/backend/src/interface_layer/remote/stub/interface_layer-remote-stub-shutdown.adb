separate (Interface_Layer.Remote.Stub)
procedure Shutdown (
   This    : Stub.Object)
is

   Queue_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Queue_Handler;

   Call_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Call_Handler (
         Queue_Handler, Types.ASYNC);

   Req_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Request_Handler (
         Call_Handler, Types.SHUTDOWN);

begin
-- Activate the Chain of Handlers
   Req_Handler.Handle;
-- Free resources
   Interface_Layer.Service.Pipelines.Handler.Finalize (Req_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Call_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Queue_Handler);
end Shutdown;
