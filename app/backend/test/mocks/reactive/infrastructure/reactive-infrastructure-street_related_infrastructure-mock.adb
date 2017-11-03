with Mock.Exceptions; use Mock.Exceptions;

with Shared.Infra_Id_To_String_Map;

package body Reactive.Infrastructure.Street_Related_Infrastructure.Mock is

   function Create return Street_Related_Infrastructure.Mock.Reference
   is (new Street_Related_Infrastructure.Mock.Object);

   function Get_Id (This : in Street_Related_Infrastructure.Mock.Object)
      return Infra_Id
   is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Package_Name  => "Reactive.Infrastructure.Street_Related_Infrastructure.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function "=" (This, Outher : in Street_Related_Infrastructure.Mock.Object)
                 return Boolean is
   begin
      if not This.Return_Values.Equality_Operator_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "'='",
            Package_Name  => "Reactive.Infrastructure.Street_Related_Infrastructure.Mock");
      end if;

      return This.Return_Values.Equality_Operator;
   end "=";

   function Find_Street (This : in Street_Related_Infrastructure.Mock.Object)
      return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name  => "Reactive.Infrastructure.Street_Related_Infrastructure.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Is_Contained_By (
      This         : in Street_Related_Infrastructure.Mock.Object;
      Container_Id : in Infra_Id)
   return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name  => "Reactive.Infrastructure."
                  & "Street_Related_Infrastructure.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   function Dump (This : in Street_Related_Infrastructure.Mock.Object)
   return G_JSON.JSON_Value is
   begin
      if not This.Return_Values. Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Dump",
            Package_Name  => "Reactive.Infrastructure."
                  & "Street_Related_Infrastructure.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   procedure Set_Id (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Id := Return_Value;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
      This.Return_Values.Equality_Operator_Existence := TRUE;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

   procedure Set_Return_Value_For_Dump
     (This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := TRUE;
   end Set_Return_Value_For_Dump;

end Reactive.Infrastructure.Street_Related_Infrastructure.Mock;
