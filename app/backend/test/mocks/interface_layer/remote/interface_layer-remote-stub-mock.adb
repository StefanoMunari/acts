-- core
with Ada.Unchecked_Deallocation;

with Mock.Exceptions;

use Mock.Exceptions;

package body Interface_Layer.Remote.Stub.Mock is

   function Create return Stub.Mock.Reference
   is (new Stub.Mock.Object);

   function Create (Handler_Factory: Abstract_Handler_Factory.Reference)
   return Mock.Object is
      This : Mock.Object;
   begin
      This.Handler_Factory := Handler_Factory;
      return This;
   end Create;

   procedure Finalize (This : in out Mock.Object) is
      procedure Free is new
      Ada.Unchecked_Deallocation (Abstract_Handler_Factory.Object'Class,
                                  Abstract_Handler_Factory.Reference);
   begin
      Free (This.Handler_Factory);
   end Finalize;

   procedure Finalize (This_Ref : in out Mock.Reference) is
      procedure Free is new Ada.Unchecked_Deallocation
      (Mock.Object'Class, Mock.Reference);
   begin
      This_Ref.all.Finalize;
      Free (This_Ref);
   end Finalize;

   procedure Enter (
      This           :        Mock.Object;
      App_Wrapper    : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id :        Agent.Agent_Id)
   is
   begin
      if not This.Return_Values.Enter_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Answer for Rendezvous",
            Procedure_Name => "Enter",
            Package_Name   => "Stub.Mock");
      end if;

   -- The following line is deprecated
      --Rendezvous_Ref.Provide_Answer (This.Return_Values.Enter);
   end Enter;

   procedure Query (
      This           :        Stub.Mock.Object;
      Request        : in out App_Wrapper_Pkg.Reference;
      Callbacks      : in     Callback_Pair_Pkg.Object;
      Recipient      : in     Recipient_Type;
      Correlation_Id : in     Agent.Agent_Id) is
   begin
      if not This.Return_Values.Query_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Answer for Rendezvous",
            Procedure_Name => "Query",
            Package_Name   => "Stub.Mock");
      end if;

   -- The following line is deprecated
      --Rendezvous_Ref.Provide_Answer (This.Return_Values.Query);
   end Query;

-- TODO: Add implementation
   procedure Heartbeat (This : Mock.Object) is null;
   procedure Snapshot (This : Mock.Object;
      Request : App_Wrapper.Reference) is null;
   procedure Boot (This : Mock.Object) is null;

   procedure Set_Return_Value_For_Enter (
      This         : in out Stub.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Enter := Return_Value;
      This.Return_Values.Enter_Existence := TRUE;
   end Set_Return_Value_For_Enter;

   procedure Set_Return_Value_For_Query (
      This         : in out Stub.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Query := Return_Value;
      This.Return_Values.Query_Existence := TRUE;
   end Set_Return_Value_For_Query;

end Interface_Layer.Remote.Stub.Mock;
