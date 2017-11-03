with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Build.Street_Builder.Mock is

   function Create return Street_Builder.Mock.Reference
   is (new Street_Builder.Mock.Object);

   function With_Bikeway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id is
   begin
      if not This.Return_Values.With_Bikeway_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "With_Bikeway",
            Function_Param => "Return value",
            Package_Name   =>
               "Reactive.Infrastructure.Build.Street_Builder.Mock");
      end if;

      This.Mock_Values.With_Bikeway_Called := True;
      return This.Return_Values.With_Bikeway;
   end With_Bikeway;

   function With_Footway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id is
   begin
      if not This.Return_Values.With_Footway_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "With_Footway",
            Function_Param => "Return value",
            Package_Name   =>
               "Reactive.Infrastructure.Build.Street_Builder.Mock");
      end if;

      This.Mock_Values.With_Footway_Called := True;
      return This.Return_Values.With_Footway;
   end With_Footway;

   function With_Roadway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id is
   begin
      if not This.Return_Values.With_Roadway_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "With_Roadway",
            Function_Param => "Return value",
            Package_Name   =>
               "Reactive.Infrastructure.Build.Street_Builder.Mock");
      end if;

      This.Mock_Values.With_Roadway_Called := True;
      return This.Return_Values.With_Roadway;
   end With_Roadway;

   function Get_Street (This : in out Street_Builder.Mock.Object;
                        JSON : in     G_JSON.JSON_Value)
      return Infra_Id is
   begin
      if not This.Return_Values.Get_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "Get_Street",
            Function_Param => "Return value",
            Package_Name   =>
               "Reactive.Infrastructure.Build.Street_Builder.Mock");
      end if;

      return This.Return_Values.Get_Street;
   end Get_Street;

   procedure Set_Return_Value_For_With_Bikeway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.With_Bikeway := Return_Value;
      This.Return_Values.With_Bikeway_Existence := True;
   end Set_Return_Value_For_With_Bikeway;

   procedure Set_Return_Value_For_With_Footway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.With_Footway := Return_Value;
      This.Return_Values.With_Footway_Existence := True;
   end Set_Return_Value_For_With_Footway;

   procedure Set_Return_Value_For_With_Roadway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.With_Roadway := Return_Value;
      This.Return_Values.With_Roadway_Existence := True;
   end Set_Return_Value_For_With_Roadway;

   procedure Set_Return_Value_For_Get_Street (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Street := Return_Value;
      This.Return_Values.Get_Street_Existence := True;
   end Set_Return_Value_For_Get_Street;

   function Get_With_Bikeway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean is (This.Mock_Values.With_Bikeway_Called);

   function Get_With_Footway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean is (This.Mock_Values.With_Footway_Called);

   function Get_With_Roadway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean is (This.Mock_Values.With_Roadway_Called);

end Reactive.Infrastructure.Build.Street_Builder.Mock;
