with Ada.Strings.Unbounded;

with Active.Agent;

with Interface_Layer.Wrappers.Application;
with Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;

with Shared.Rendezvous.Boolean_Rendezvous;

package Interface_Layer.Remote.Stub.Mock is

   package Agent renames Active.Agent;
   package App_Wrapper renames Interface_Layer.Wrappers.Application;
   package Boolean_Rendezvous renames Shared.Rendezvous.Boolean_Rendezvous;
   package Abstract_Handler_Factory
   renames Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
   package SU renames Ada.Strings.Unbounded;

   type Object is new Stub.Object with private;
   type Reference is access all Mock.Object'Class;

   function Create return Stub.Mock.Reference;

   function Create (Handler_Factory: Abstract_Handler_Factory.Reference)
      return Mock.Object;
   procedure Finalize (This : in out Mock.Object);
   procedure Finalize (This_Ref : in out Mock.Reference);

   overriding
   procedure Enter (
      This           :        Mock.Object;
      App_Wrapper    : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id :        Agent.Agent_Id);

   overriding
   procedure Async_Request (
      This            :    Mock.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id) is null;

   overriding
   procedure Async_Request_Other (
      This            :    Stub.Mock.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id) is null;

   overriding
   procedure Query (
      This           :        Stub.Mock.Object;
      Request        : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id : in     Agent.Agent_Id);

   overriding
   procedure Snapshot (This : Mock.Object;
      Request : App_Wrapper.Reference);

   overriding
   procedure Boot (This : Mock.Object);

   not overriding
   procedure Set_Return_Value_For_Enter (
      This         : in out Stub.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Query (
      This         : in out Stub.Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Enter : Boolean;
      Enter_Existence : Boolean := FALSE;
      Query : Boolean;
      Query_Existence : Boolean := FALSE;
   end record;

   type Object is new Stub.Object with record
      Return_Values : Return_Values_Collection;
   end record;
end Interface_Layer.Remote.Stub.Mock;
