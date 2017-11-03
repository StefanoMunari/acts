package body Interface_Layer.Remote.Query_Builder is

   function Create return Query_Builder.Reference
   is (new Query_Builder.Object);

   function With_Correlation_Id (
      This           :    Query_Builder.Object;
      Correlation_Id : in Agent.Agent_Id) return Query_Builder.Object'Class
   is
   begin
      This.Query_Object.Set_Field (
         Remote.Query_Corr_Field, SU.To_String (Correlation_Id));
      return This;
   end With_Correlation_Id;

   function With_Name (
      This      :    Query_Builder.Object;
      Proc_Name : in String) return Query_Builder.Object'Class
   is
   begin
      This.Query_Object.Set_Field (Remote.Query_Proc_Field, Proc_Name);
      return This;
   end With_Name;

   function With_Arg (
      This : in Query_Builder.Object;
      Arg  : in Infra_Id) return Query_Builder.Object'Class
   is
      Arg_Int  : Integer := Integer (Arg);
      Arg_JSON : G_JSON.JSON_Value := G_JSON.Create (Arg_Int);
      Args     : G_JSON.JSON_Array;
   begin
   -- get current arguments list
      if This.Query_Object.Has_Field (Remote.Query_Args_Field) then
         Args := This.Query_Object.Get (Remote.Query_Args_Field);
   -- if there isn't any, create new json array from scratch
      else
         Args := G_JSON.Empty_Array;
      end if;

   -- append new arg to args list
      G_JSON.Append (Args, Arg_JSON);
   -- update args in query
      This.Query_Object.Set_Field (Remote.Query_Args_Field, Args);

      return This;
   end With_Arg;

   function With_Arg (
      This : in Query_Builder.Object;
      Arg  : in Agent.Agent_Id)
   return Query_Builder.Object'Class
   is
      Arg_SU   : SU.Unbounded_String := SU.Unbounded_String (Arg);
      Arg_JSON : G_JSON.JSON_Value := G_JSON.Create (SU.To_String (Arg_SU));
      Args     : G_JSON.JSON_Array;
   begin
   -- get current arguments list
      if This.Query_Object.Has_Field (Remote.Query_Args_Field) then
         Args := This.Query_Object.Get (Remote.Query_Args_Field);
   -- if there isn't any, create new json array from scratch
      else
         Args := G_JSON.Empty_Array;
      end if;

   -- append new arg to args list
      G_JSON.Append (Args, Arg_JSON);
   -- update args in query
      This.Query_Object.Set_Field (Remote.Query_Args_Field, Args);

      return This;
   end With_Arg;

   function Get_Result (This : Query_Builder.Object) return SU.Unbounded_String
   is (SU.To_Unbounded_String (This.Query_Object.Write));

end Interface_Layer.Remote.Query_Builder;
