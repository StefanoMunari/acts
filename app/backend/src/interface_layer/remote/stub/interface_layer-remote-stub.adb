-- core
with Ada.Unchecked_Deallocation;

with Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;

package body Interface_Layer.Remote.Stub is

   package Handler_Concrete_Factory_Pkg
      renames Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;

   function Create (
      Handler_Factory : access Abstract_Handler_Factory.Object'Class := null)
   return Stub.Reference
   is
      This : Stub.Reference := new Stub.Object;
   begin
      if Handler_Factory = null then
         This.Handler_Factory := new Handler_Concrete_Factory_Pkg.Object;
      else
         This.Handler_Factory := Handler_Factory;
      end if;

      return This;
   end Create;

   procedure Finalize (This : in out Stub.Object) is
      procedure Free is new
      Ada.Unchecked_Deallocation (Abstract_Handler_Factory.Object'Class,
                                  Abstract_Handler_Factory.Reference);
   begin
      Free (This.Handler_Factory);
   end Finalize;

   procedure Finalize (This_Ref : in out Stub.Reference) is
      procedure Free is new Ada.Unchecked_Deallocation
      (Stub.Object'Class, Stub.Reference);
   begin
      This_Ref.all.Finalize;
      Free (This_Ref);
   end Finalize;

   procedure Enter (
      This           :        Stub.Object;
      App_Wrapper    : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id :        Agent.Agent_Id) is separate;

   procedure Async_Request (
      This            :    Stub.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id) is separate;

   procedure Async_Request_Other (
      This            :    Stub.Object;
      Data_Wrapper    : in App_Wrapper_Pkg.Reference;
      Request         : in Types.Request_Type;
      Recipient       : in Recipient_Type;
      Correlation_Id  : in Agent.Agent_Id) is separate;

   procedure Query (
      This           :        Stub.Object;
      Request        : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id : in     Agent.Agent_Id) is separate;


   procedure Snapshot (
      This    : Stub.Object;
      Request : App_Wrapper_Pkg.Reference) is separate;

   procedure Boot (This : Stub.Object) is separate;

   procedure Shutdown (This : Stub.Object) is separate;

end Interface_Layer.Remote.Stub;
