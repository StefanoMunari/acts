with Mock.Exceptions;

use Mock.Exceptions;

package body Interface_Layer.Remote.Query_Builder.Mock is

   function Create return Query_Builder.Mock.Reference
   is (new Query_Builder.Mock.Object);

   function With_Correlation_Id (
      This           :    Query_Builder.Mock.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Query_Builder.Mock.Object'Class is (This);

   function With_Name (
      This      :    Query_Builder.Mock.Object;
      Proc_Name : in String)
   return Query_Builder.Mock.Object'Class is (This);

   function With_Arg (
      This :    Query_Builder.Mock.Object;
      Arg  : in Infra_Id)
   return Query_Builder.Mock.Object'Class is (This);

   function With_Arg (
      This :    Query_Builder.Mock.Object;
      Arg  : in Agent.Agent_Id)
   return Query_Builder.Mock.Object'Class is (This);

   function Get_Result (This : Query_Builder.Mock.Object)
   return SU.Unbounded_String is
   begin
      if not This.Return_Values.Get_Result_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "Get_Result",
            Function_Param => "",
            Package_Name   => "Interface_Layer.Remote.Query_Builder.Mock");
      end if;

      return This.Return_Values.Get_Result;
   end Get_Result;

   not overriding
   procedure Set_Return_Value_For_Get_Result (
      This         : in out Query_Builder.Mock.Object;
      Return_Value : in     SU.Unbounded_String)
   is
   begin
      This.Return_Values.Get_Result_Existence := True;
      This.Return_Values.Get_Result := Return_Value;
   end Set_Return_Value_For_Get_Result;

end Interface_Layer.Remote.Query_Builder.Mock;
