with Active.Agent;

with Interface_Layer.Service.Pipelines.Handler;
with Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application;

with Reactive;

with Shared.Callback_Pair;
with Shared.Rendezvous.Boolean_Rendezvous;
with Shared.Shared_References_App_Wrapper;

package Interface_Layer.Service.Pipelines.Handler.Mock_Factory is

   package Agent              renames Active.Agent;
-- IL
   package Types              renames Interface_Layer.Utils.Types;
   package App_Wrapper_Pkg    renames Interface_Layer.Wrappers.Application;
-- shared
   package Callback_Pair_Pkg  renames Shared.Callback_Pair;
   package Boolean_Rendezvous renames Shared.Rendezvous.Boolean_Rendezvous;
   package SR_App_Wrapper_Pkg renames Shared.Shared_References_App_Wrapper;

   use Reactive.Infra_Id_Type;
   use Types.Recipient_Type_Pkg;

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Abstract_Factory.Object
   with null record;
   type Reference is access all Mock_Factory.Object'Class;

   overriding
   function Create_Queue_Handler (This : Mock_Factory.Object)
   return Handler.Reference;

   overriding
   function Create_Boot_Handler (This : Mock_Factory.Object)
   return Interface_Layer.Service.Pipelines.Handler.Reference;

   overriding
   function Create_Call_Handler (
      This      :    Mock_Factory.Object;
      Next      :    Handler.Reference := null;
      Call_Type : in Types.Call_Type)
   return Handler.Reference;

   overriding
   function Create_Correlation_Id_Handler (
      This           :    Mock_Factory.Object;
      Next           :    Handler.Reference := null;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference;

   overriding
   function Create_Rendezvous_Handler (
      This           :    Mock_Factory.Object;
      Next           :    Handler.Reference := null;
      Callbacks      : in Callback_Pair_Pkg.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference;

   function Create_Data_Other_Handler (
      This      : Mock_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference;
      D_Type    : Types.Data_Type)
   return Handler.Reference;

   overriding
   function Create_Data_Traveller_Handler (
      This      : Mock_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference)
   return Handler.Reference;

   overriding
   function Create_Request_Handler (
      This         :    Mock_Factory.Object;
      Next         :    Handler.Reference := null;
      Request_Type : in Types.Request_Type)
   return Handler.Reference;

   overriding
   function Create_Recipient_Handler (
      This      :    Mock_Factory.Object;
      Next      :    Handler.Reference := null;
      Recipient : in Recipient_Type)
   return Handler.Reference;

end Interface_Layer.Service.Pipelines.Handler.Mock_Factory;
