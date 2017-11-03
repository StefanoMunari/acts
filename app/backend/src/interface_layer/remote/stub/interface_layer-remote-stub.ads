with Ada.Strings.Unbounded;

with Active.Agent;

with Interface_Layer.Utils.Types;
with Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
with Interface_Layer.Wrappers.Application;

with Reactive;

with Shared.Callback_Pair;

package Interface_Layer.Remote.Stub is

-- active
   package Agent renames Active.Agent;
-- interface layer
   package Abstract_Handler_Factory
      renames Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
   package Types renames Interface_Layer.Utils.Types;
   package App_Wrapper_Pkg renames Interface_Layer.Wrappers.Application;
-- shared
   package Callback_Pair_Pkg renames Shared.Callback_Pair;
-- core
   package SU renames Ada.Strings.Unbounded;

   use Reactive.Infra_Id_Type;
   use Types.Recipient_Type_Pkg;

   type Object is tagged private;
   type Reference is access all Stub.Object'Class;

   function Create (
      Handler_Factory : access Abstract_Handler_Factory.Object'Class := null)
   return Stub.Reference;
   procedure Finalize (This : in out Stub.Object);
   procedure Finalize (This_Ref : in out Stub.Reference);

   procedure Enter (
      This           :        Stub.Object;
      App_Wrapper    : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id :        Agent.Agent_Id);

   procedure Async_Request (
      This            :    Stub.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id);

   procedure Async_Request_Other (
      This            :    Stub.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id);

   procedure Query (
      This           :        Stub.Object;
      Request        : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id : in     Agent.Agent_Id);

   procedure Snapshot (This    : Stub.Object;
                       Request : App_Wrapper_Pkg.Reference);

   procedure Boot (This : Stub.Object);

   procedure Shutdown (This : Stub.Object);

private
   type Object is tagged
   record
      Handler_Factory : access Abstract_Handler_Factory.Object'Class := null;
   end record;
end Interface_Layer.Remote.Stub;
