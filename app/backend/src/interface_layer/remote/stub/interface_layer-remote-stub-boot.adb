separate (Interface_Layer.Remote.Stub)
procedure Boot (
   This    : Stub.Object)
is

   Boot_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Boot_Handler;

   Call_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Call_Handler (
         Boot_Handler, Types.SYNC);

   Req_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Request_Handler (
         Call_Handler, Types.BOOT);

begin
-- Activate the Chain of Handlers
   Req_Handler.Handle;

-- Free resources
   Interface_Layer.Service.Pipelines.Handler.Finalize (Req_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Call_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Boot_Handler);
end Boot;
