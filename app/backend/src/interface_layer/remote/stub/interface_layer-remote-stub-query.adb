separate (Interface_Layer.Remote.Stub)
procedure Query (
   This           :        Stub.Object;
   Request        : in out App_Wrapper_Pkg.Reference;
   Callbacks      : in     Callback_Pair_Pkg.Object;
   Recipient      : in     Recipient_Type;
   Correlation_Id : in     Agent.Agent_Id)
is

   Queue_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Queue_Handler;

   Data_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Data_Other_Handler (
         Queue_Handler, Request, Types.MESSAGE);

   Rendezvous_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Rendezvous_Handler (
         Data_Handler, Callbacks, Correlation_Id);

   Id_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Correlation_Id_Handler (
         Rendezvous_Handler, Correlation_Id);

   Recipient_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Recipient_Handler (
         Id_Handler, Recipient);

   Call_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Call_Handler (
         Recipient_Handler, Types.SYNC);

   Req_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Request_Handler (
         Call_Handler, Types.QUERY);

begin
   -- Activate the Chain of Handlers
      Req_Handler.Handle;
   -- Free resources
      Request.Finalize;
      Interface_Layer.Service.Pipelines.Handler.Finalize (Req_Handler);
      Interface_Layer.Service.Pipelines.Handler.Finalize (Call_Handler);
      Interface_Layer.Service.Pipelines.Handler.Finalize (Id_Handler);
      Interface_Layer.Service.Pipelines.Handler.Finalize (Rendezvous_Handler);
      Interface_Layer.Service.Pipelines.Handler.Finalize (Data_Handler);
      Interface_Layer.Service.Pipelines.Handler.Finalize (Queue_Handler);
end Query;
