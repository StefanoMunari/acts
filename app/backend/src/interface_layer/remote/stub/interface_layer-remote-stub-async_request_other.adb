separate (Interface_Layer.Remote.Stub)
procedure Async_Request_Other (
   This            :    Stub.Object;
   Data_Wrapper    : in App_Wrapper_Pkg.Reference;
   Request         : in Types.Request_Type;
   Recipient       : in Recipient_Type;
   Correlation_Id  : in Agent.Agent_Id)
is

   Queue_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Queue_Handler;

   Data_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Data_Other_Handler (
         Queue_Handler, Data_Wrapper, Types.MESSAGE);

   Id_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Correlation_Id_Handler (
         Data_Handler, Correlation_Id);

   Recipient_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Recipient_Handler (
         Id_Handler, Recipient);

   Call_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Call_Handler (
         Recipient_Handler, Types.ASYNC);

   Req_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Request_Handler (
         Call_Handler, Request);

begin
-- Activate the Chain of Handlers
   Req_Handler.Handle;
-- Free resources
   Interface_Layer.Service.Pipelines.Handler.Finalize (Req_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Call_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Id_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Data_Handler);
   Interface_Layer.Service.Pipelines.Handler.Finalize (Queue_Handler);
end Async_Request_Other;
