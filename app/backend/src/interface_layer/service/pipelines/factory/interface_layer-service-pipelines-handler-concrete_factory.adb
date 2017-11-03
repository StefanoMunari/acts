with Interface_Layer.Service.Pipelines.Handler.Boot;
with Interface_Layer.Service.Pipelines.Handler.Call;
with Interface_Layer.Service.Pipelines.Handler.Correlation_Id;
with Interface_Layer.Service.Pipelines.Handler.Data.Other;
with Interface_Layer.Service.Pipelines.Handler.Data.Traveller;
with Interface_Layer.Service.Pipelines.Handler.Queue;
with Interface_Layer.Service.Pipelines.Handler.Recipient;
with Interface_Layer.Service.Pipelines.Handler.Rendezvous;
with Interface_Layer.Service.Pipelines.Handler.Request;
with Interface_Layer.Wrappers.Request;

package body Interface_Layer.Service.Pipelines.Handler.Concrete_Factory is
   package Request_Wrapper renames Interface_Layer.Wrappers.Request;
   package Recipient_Pkg
      renames Interface_Layer.Service.Pipelines.Handler.Recipient;

   function Create_Queue_Handler (This : Concrete_Factory.Object)
   return Handler.Reference is
      Instance : Handler.Reference :=
         Handler.Reference (Handler.Queue.Create);
   begin
      return Instance;
   end Create_Queue_Handler;

   function Create_Boot_Handler (This : Concrete_Factory.Object)
   return Handler.Reference is
      Instance : Handler.Reference :=
         Handler.Reference (Handler.Boot.Create);
   begin
      return Instance;
   end Create_Boot_Handler;

   function Create_Call_Handler (
      This : Concrete_Factory.Object;
      Next : Handler.Reference := null;
      Call_Type : in Types.Call_Type)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object := Request_Wrapper.Create (Call_Type);
      Instance : Handler.Reference := Handler.Reference (
         Handler.Call.Create (Next, Wrapper));
   begin
      return Handler.Reference (Instance);
   end Create_Call_Handler;

   function Create_Correlation_Id_Handler (
      This           :    Concrete_Factory.Object;
      Next           :    Handler.Reference := null;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (SU.Unbounded_String (Correlation_Id));
      Instance : Handler.Reference := Handler.Reference (
         Handler.Correlation_Id.Create (Next, Wrapper));
   begin
      return Handler.Reference (Instance);
   end Create_Correlation_Id_Handler;

   function Create_Rendezvous_Handler (
      This           :    Concrete_Factory.Object;
      Next           :    Handler.Reference := null;
      Callbacks      : in Callback_Pair_Pkg.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (
            Callbacks, SU.Unbounded_String (Correlation_Id));
      Instance : Handler.Reference :=
         Handler.Reference (Handler.Rendezvous.Create (Next, Wrapper));
   begin
      return Instance;
   end Create_Rendezvous_Handler;

   function Create_Data_Other_Handler (
      This       : Concrete_Factory.Object;
      Next       : Handler.Reference := null;
      A_Wrapper  : App_Wrapper_Pkg.Reference;
      D_Type     : Types.Data_Type)
   return Handler.Reference
   is
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (A_Wrapper, D_Type);
      Instance : Handler.Reference :=
         Handler.Reference (Handler.Data.Other.Create (Next, Wrapper));
   begin
      return Instance;
   end Create_Data_Other_Handler;

   function Create_Data_Traveller_Handler (
      This       : Concrete_Factory.Object;
      Next       : Handler.Reference := null;
      A_Wrapper  : App_Wrapper_Pkg.Reference)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object := Request_Wrapper.Create (A_Wrapper);
      Instance : Handler.Reference :=
         Handler.Reference (Handler.Data.Traveller.Create (Next, Wrapper));
   begin
      return Instance;
   end Create_Data_Traveller_Handler;

   function Create_Request_Handler (
      This         :    Concrete_Factory.Object;
      Next         :    Handler.Reference := null;
      Request_Type : in Types.Request_Type)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (Request_Type);
      Instance : Request.Reference := Handler.Request.Create (Next, Wrapper);
   begin
      return Handler.Reference (Instance);
   end Create_Request_Handler;

   function Create_Recipient_Handler (
      This      :    Concrete_Factory.Object;
      Next      :    Handler.Reference := null;
      Recipient : in Recipient_Type)
   return Handler.Reference is
      Wrapper  : Request_Wrapper.Object :=
         Request_Wrapper.Create (Recipient);
      Instance : Recipient_Pkg.Reference
         := Recipient_Pkg.Create (Next, Wrapper);
   begin
      return Handler.Reference (Instance);
   end Create_Recipient_Handler;

end Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;
