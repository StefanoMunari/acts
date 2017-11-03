-- core
with Ada.Strings.Unbounded;
-- gnatcoll libs
with GNATCOLL.JSON;

--local
with Active.Agent;

with Reactive;

package Interface_Layer.Remote.Query_Builder is

   package G_JSON renames GNATCOLL.JSON;
   package Agent  renames Active.Agent;
   package SU     renames Ada.Strings.Unbounded;

   use Reactive.Infra_Id_Type;

   type Object is tagged private;
   type Reference is access all Query_Builder.Object'Class;

   function Create return Query_Builder.Reference;

   function With_Correlation_Id (
      This           :    Query_Builder.Object;
      Correlation_Id : in Agent.Agent_Id) return Query_Builder.Object'Class;

   function With_Name (
      This      :    Query_Builder.Object;
      Proc_Name : in String) return Query_Builder.Object'Class;

   function With_Arg (
      This :    Query_Builder.Object;
      Arg  : in Infra_Id) return Query_Builder.Object'Class;

   function With_Arg (
      This :    Query_Builder.Object;
      Arg  : in Agent.Agent_Id) return Query_Builder.Object'Class;

   function Get_Result (This : Query_Builder.Object) return SU.Unbounded_String;

private

   type Object is tagged record
      Query_Object : G_JSON.JSON_Value := G_JSON.Create_Object;
   end record;

end Interface_Layer.Remote.Query_Builder;
