with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock is

   function Create return Intersection_Config_Reader.Mock.Reference
   is (new Intersection_Config_Reader.Mock.Object);

   procedure Set_Builder (
      This    :    out Intersection_Config_Reader.Mock.Object;
      Builder : access Intersection_Builder_Pkg.Object'Class := null) is
   begin
      This.Mock_Values.Set_Builder_Called := True;
   end Set_Builder;

   function Read (
      This              : in out Intersection_Config_Reader.Mock.Object;
      Intersection_Json : in     G_JSON.JSON_Value) return Infra_Id
   is
      Return_Value : Infra_Id;
   begin
      This.Mock_Values.Read_Called := True;

      if This.Return_Values.Read.Is_Empty then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Read",
            Function_Param => "Return value",
            Package_Name   =>
               "Reactive.Infrastructure.Build.Street_Config_Reader.Mock");
      end if;

      Return_Value := This.Return_Values.Read.First_Element;
      This.Return_Values.Read.Delete_First;
      return Return_Value;
   end Read;

   function Get_Set_Builder_Called (
      This : in out Intersection_Config_Reader.Mock.Object)
   return Boolean is (This.Mock_Values.Set_Builder_Called);

   procedure Set_Return_Value_For_Read (
      This         : in out Intersection_Config_Reader.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Read.Append (Return_Value);
   end Set_Return_Value_For_Read;

   function Get_Read_Called (
      This : in out Intersection_Config_Reader.Mock.Object)
   return Boolean is (This.Mock_Values.Read_Called);

end Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock;
