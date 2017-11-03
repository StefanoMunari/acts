with Mock.Exceptions; use Mock.Exceptions;

package body Reactive.Infrastructure.Factory.Street_Factory.Mock is

   function Create return Infrastructure.Factory.Street_Factory.Mock.Reference
   is (new Street_Factory.Mock.Object);

   function Decorate_Stretch (
      This        : in out Street_Factory.Mock.Object;
      Stretch_Ref : in     Reactive.Infrastructure.Stretch.Reference;
      Decoration  : in     G_JSON.JSON_Value)
   return Stretch.Reference is
   begin
      if not This.Return_Values.Decorate_Stretch_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Return_Value",
            Procedure_Name => "Decorate_Stretch",
            Package_Name   =>
               "Reactive.Infrastructure.Factory.Street_Factory.Mock");
      end if;

      return This.Return_Values.Decorate_Stretch;
   end Decorate_Stretch;

   function Decorate_Lane (
      This       : in out Street_Factory.Mock.Object;
      Lane       : in     Reactive.Infrastructure.Lane.Reference;
      Decoration : in     G_JSON.JSON_Value)
   return Reactive.Infrastructure.Lane.Reference is
   begin
      if not This.Return_Values.Decorate_Lane_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Return_Value",
            Procedure_Name => "Decorate_Lane",
            Package_Name   =>
               "Reactive.Infrastructure.Factory.Street_Factory.Mock");
      end if;

      return This.Return_Values.Decorate_Lane;
   end Decorate_Lane;

   function Create_Stretch (This : in out Street_Factory.Mock.Object)
      return Stretch.Reference
    is
      Result : Stretch.Reference;
   begin
      if This.Return_Values.Create_Stretch.Is_Empty then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Return_Value",
            Procedure_Name => "Create_Stretch",
            Package_Name   =>
               "Reactive.Infrastructure.Factory.Street_Factory.Mock");
      end if;

      Result := This.Return_Values.Create_Stretch.First_Element;
      This.Return_Values.Create_Stretch.Delete_First;
      return Result;
   end Create_Stretch;

   procedure Set_Stretch_Id (This : in out Street_Factory.Mock.Object;
                             Id   : in     Infra_Id) is
   begin
      This.Mock_Values.Set_Stretch_Id_Called := TRUE;
   end Set_Stretch_Id;

   procedure Set_Stretch_Size (This : in out Street_Factory.Mock.Object;
                               Size : in     Natural) is
   begin
      This.Mock_Values.Set_Stretch_Size_Called := TRUE;
   end Set_Stretch_Size;

   function Create_Lane (This : in out Street_Factory.Mock.Object)
      return Lane.Reference
   is
      Result : Lane.Reference;
   begin
      if This.Return_Values.Create_Lane.Is_Empty then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Return_Value",
            Procedure_Name => "Create_Lane",
            Package_Name   =>
               "Reactive.Infrastructure.Factory.Street_Factory.Mock");
      end if;

      Result := This.Return_Values.Create_Lane.First_Element;
      This.Return_Values.Create_Lane.Delete_First;
      return Result;
   end Create_Lane;

   procedure Set_Lane_Id (This : in out Street_Factory.Mock.Object;
                          Id   : in     Infra_Id) is
   begin
      This.Mock_Values.Set_Lane_Id_Called := TRUE;
   end Set_Lane_Id;

   procedure Set_Lane_Direction (
      This      : in out Street_Factory.Mock.Object;
      Direction : in     Shared.Direction.Straight) is
   begin
      This.Mock_Values.Set_Lane_Direction_Called := TRUE;
   end Set_Lane_Direction;

   procedure Set_Lane_Stretches (
      This      : in out Street_Factory.Mock.Object;
      Stretches : in     Infra_Id_List.List) is
   begin
      This.Mock_Values.Set_Lane_Stretches_Called := TRUE;
   end Set_Lane_Stretches;

   procedure Set_Return_Value_Decorate_Stretch
     (This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Stretch.Reference) is
   begin
      This.Return_Values.Decorate_Stretch_Existence := TRUE;
      This.Return_Values.Decorate_Stretch := Return_Value;
   end Set_Return_Value_Decorate_Stretch;

   procedure Set_Return_Value_Decorate_Lane
     (This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Lane.Reference) is
   begin
      This.Return_Values.Decorate_Lane_Existence := TRUE;
      This.Return_Values.Decorate_Lane := Return_Value;
   end Set_Return_Value_Decorate_Lane;

   procedure Set_Return_Value_Create_Stretch
     (This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Stretch.Reference) is
   begin
      This.Return_Values.Create_Stretch.Append (Return_Value);
   end Set_Return_Value_Create_Stretch;

   procedure Set_Return_Value_Create_Lane
     (This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Lane.Reference) is
   begin
      This.Return_Values.Create_Lane.Append (Return_Value);
   end Set_Return_Value_Create_Lane;

end Reactive.Infrastructure.Factory.Street_Factory.Mock;
