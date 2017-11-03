package Interface_Layer.Remote.Query_Builder.Mock is

   type Object is new Query_Builder.Object with private;
   type Reference is access all Query_Builder.Mock.Object'Class;

   function Create return Query_Builder.Mock.Reference;

   function With_Correlation_Id (
      This           :    Query_Builder.Mock.Object;
      Correlation_Id : in Agent.Agent_Id)
   return Query_Builder.Mock.Object'Class;

   function With_Name (
      This      :    Query_Builder.Mock.Object;
      Proc_Name : in String) return Query_Builder.Mock.Object'Class;

   function With_Arg (
      This :    Query_Builder.Mock.Object;
      Arg  : in Infra_Id) return Query_Builder.Mock.Object'Class;

   function With_Arg (
      This :    Query_Builder.Mock.Object;
      Arg  : in Agent.Agent_Id) return Query_Builder.Mock.Object'Class;

   function Get_Result (This : Query_Builder.Mock.Object)
   return SU.Unbounded_String;

   not overriding
   procedure Set_Return_Value_For_Get_Result (
      This         : in out Query_Builder.Mock.Object;
      Return_Value : in     SU.Unbounded_String);

private
   type Return_Values_Collection is record
      Get_Result : SU.Unbounded_String;
      Get_Result_Existence : Boolean := FALSE;
   end record;

   type Object is new Query_Builder.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Interface_Layer.Remote.Query_Builder.Mock;
