with Interface_Layer.Service.Pipelines.Handler.Boot.Mock;
with Interface_Layer.Service.Pipelines.Handler.Call.Mock;
with Interface_Layer.Service.Pipelines.Handler.Correlation_Id.Mock;
with Interface_Layer.Service.Pipelines.Handler.Data.Other.Mock;
with Interface_Layer.Service.Pipelines.Handler.Data.Traveller.Mock;
with Interface_Layer.Service.Pipelines.Handler.Queue.Mock;
with Interface_Layer.Service.Pipelines.Handler.Recipient.Mock;
with Interface_Layer.Service.Pipelines.Handler.Request.Mock;
with Interface_Layer.Service.Pipelines.Handler.Rendezvous.Mock;
with Interface_Layer.Wrappers.Request;

package body Interface_Layer.Service.Pipelines.Handler.Mock_Factory is
   package Request_Wrapper renames Interface_Layer.Wrappers.Request;

   function Create_Queue_Handler (This : Mock_Factory.Object)
   return Handler.Reference
   is
      Instance : Handler.Reference
         := Handler.Reference (Handler.Queue.Mock.Create);
   begin
      return Instance;
   end Create_Queue_Handler;

   function Create_Boot_Handler (This : Mock_Factory.Object)
   return Handler.Reference
   is
      Instance : Handler.Reference
         := Handler.Reference (Handler.Boot.Mock.Create);
   begin
      return Instance;
   end Create_Boot_Handler;

   function Create_Call_Handler (
      This      :    Mock_Factory.Object;
      Next      :    Handler.Reference := null;
      Call_Type : in Types.Call_Type)
   return Handler.Reference
   is
      Wrapper  : Request_Wrapper.Object := Request_Wrapper.Create (Call_Type);
      Instance : Handler.Reference;
   begin
      Instance := Handler.Reference (Handler.Call.Mock.Create (Next, Wrapper));
      return Handler.Reference (Instance);
   end Create_Call_Handler;

   function Create_Correlation_Id_Handler (
      This           :    Mock_Factory.Object;
      Next           :    Handler.Reference := null;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference
   is
      Wrapper : Request_Wrapper.Object :=
         Request_Wrapper.Create (Correlation_Id);
      Instance  : Handler.Reference;
   begin
      Instance :=
         Handler.Reference (Handler.Correlation_Id.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Correlation_Id_Handler;

   function Create_Rendezvous_Handler (
      This           :    Mock_Factory.Object;
      Next           :    Handler.Reference := null;
      Callbacks      : in Callback_Pair_Pkg.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference
   is
      Instance : Handler.Reference;
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (Callbacks, Correlation_Id);
   begin
      Instance :=
         Handler.Reference (Handler.Rendezvous.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Rendezvous_Handler;

   function Create_Data_Other_Handler (
      This      : Mock_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference;
      D_Type    : Types.Data_Type)
   return Handler.Reference
   is
      Wrapper   : Request_Wrapper.Object := Request_Wrapper.Create (A_Wrapper);
      Instance  : Handler.Reference;
   begin
      Instance :=
         Handler.Reference (
            Handler.Data.Traveller.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Data_Other_Handler;

   function Create_Data_Traveller_Handler (
      This      : Mock_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference)
   return Handler.Reference
   is
      Wrapper   : Request_Wrapper.Object := Request_Wrapper.Create (A_Wrapper);
      Instance  : Handler.Reference;
   begin
      Instance :=
         Handler.Reference (
            Handler.Data.Traveller.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Data_Traveller_Handler;

   function Create_Request_Handler (
      This         : Mock_Factory.Object;
      Next         : Handler.Reference := null;
      Request_Type : in Types.Request_Type)
   return Handler.Reference
   is
      Wrapper : Request_Wrapper.Object
         := Request_Wrapper.Create (Request_Type);
      Instance : Handler.Reference;
   begin
      Instance :=
         Handler.Reference (Handler.Request.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Request_Handler;

   function Create_Recipient_Handler (
      This      :    Mock_Factory.Object;
      Next      :    Handler.Reference := null;
      Recipient : in Recipient_Type)
   return Handler.Reference
   is
      Wrapper  : Request_Wrapper.Object := Request_Wrapper.Create (Recipient);
      Instance : Handler.Reference;
   begin
      Instance :=
         Handler.Reference (Handler.Recipient.Mock.Create (Next, Wrapper));
      return Instance;
   end Create_Recipient_Handler;

end Interface_Layer.Service.Pipelines.Handler.Mock_Factory;
