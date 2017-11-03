-- core
with Ada.Strings.Unbounded;

with Active.Agent;

with Interface_Layer.Service.Pipelines.Handler;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Utils.Types;

with Reactive;

with Shared.Callback_Pair;

package Interface_Layer.Service.Pipelines.Handler.Abstract_Factory is

   package Agent             renames Active.Agent;
   package Types             renames Interface_Layer.Utils.Types;
   package App_Wrapper_Pkg   renames Interface_Layer.Wrappers.Application;
   package Callback_Pair_Pkg renames Shared.Callback_Pair;
   package SU                renames Ada.Strings.Unbounded;

   use Reactive.Infra_Id_Type;
   use Types.Recipient_Type_Pkg;

   type Object is interface;
   type Reference is access all Abstract_Factory.Object'Class;

   function Create_Queue_Handler (This : Abstract_Factory.Object)
   return Handler.Reference is abstract;

   function Create_Boot_Handler (This : Abstract_Factory.Object)
   return Interface_Layer.Service.Pipelines.Handler.Reference is abstract;

   function Create_Call_Handler (
      This      :    Abstract_Factory.Object;
      Next      :    Handler.Reference := null;
      Call_Type : in Types.Call_Type)
   return Handler.Reference is abstract;

   function Create_Correlation_Id_Handler (
      This           :    Abstract_Factory.Object;
      Next           :    Handler.Reference := null;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference is abstract;

   function Create_Rendezvous_Handler (
      This           :    Abstract_Factory.Object;
      Next           :    Handler.Reference := null;
      Callbacks      : in Callback_Pair_Pkg.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Handler.Reference is abstract;

   function Create_Data_Other_Handler (
      This      : Abstract_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference;
      D_Type    : Types.Data_Type)
   return Handler.Reference is abstract;

   function Create_Data_Traveller_Handler (
      This      : Abstract_Factory.Object;
      Next      : Handler.Reference := null;
      A_Wrapper : App_Wrapper_Pkg.Reference)
   return Handler.Reference is abstract;

   function Create_Request_Handler (
      This         :    Abstract_Factory.Object;
      Next         :    Handler.Reference := null;
      Request_Type : in Types.Request_Type)
   return Handler.Reference is abstract;

   function Create_Recipient_Handler (
      This      :    Abstract_Factory.Object;
      Next      :    Handler.Reference := null;
      Recipient : in Recipient_Type)
   return Handler.Reference is abstract;

end Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
