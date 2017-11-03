with Mock.Exceptions;use Mock.Exceptions;

with Shared.Infra_Id_To_String_Map;

package body Reactive.Infrastructure.Street.Mock is
   package Infra_Id_To_String_Map renames Shared.Infra_Id_To_String_Map;

   function Create return Street.Mock.Reference
   is (new Street.Mock.Object);

   function Is_Treadable_In_Direction (
      This      : in Street.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
      return Boolean is
   begin
      if not This.Return_Values.Is_Treadable_In_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Treadable_In_Direction",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Return_Values.Is_Treadable_In_Direction;
   end Is_Treadable_In_Direction;

   function Is_Not_Treadable_In_Direction (
      This      : in Street.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
      return Boolean is
   begin
      if not This.Return_Values.Is_Not_Treadable_In_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Not_Treadable_In_Direction",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Return_Values.Is_Not_Treadable_In_Direction;
   end Is_Not_Treadable_In_Direction;

   function Get_Id (This : in Street.Mock.Object) return Infra_Id
   is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Get_Orientation (This : in Street.Mock.Object)
                             return Direction.Orientation is
   begin
      if not This.Mock_Values.Orientation_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Orientation",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Mock_Values.Orientation;
   end Get_Orientation;

   function "=" (This, Outher : in Street.Mock.Object)
                 return Boolean is
   begin
      if not This.Return_Values.Equality_Operator_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "'='",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Return_Values.Equality_Operator;
   end "=";

   function Find_Street (This : in Street.Mock.Object)
                         return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Find_Lanes_By_Direction (This             : in Street.Mock.Object;
                                     Travel_Direction : in Direction.Straight)
      return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Lanes_By_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Lanes_By_Direction",
            Package_Name  => "Reactive.Infrastructure.Street.Mock");
      end if;

      return This.Return_Values.Find_Lanes_By_Direction;
   end Find_Lanes_By_Direction;

   function Is_Contained_By (This         : in Street.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean is
      Objects : Infra_Id_To_String_Map.Map;
   begin
      if not This.Return_Values.Is_Contained_By.Contains (Container_Id) then
         Objects.Insert (Key     => Container_Id,
                         New_Item => "Container");

         Objects.Insert (Key     => This.Mock_Values.Id,
                         New_Item => "Content");

         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name  => "Reactive.Infrastructure.Street.Mock",
            Objects => Objects);
      end if;

      return This.Return_Values.Is_Contained_By.Element (Container_Id);
   end Is_Contained_By;

   procedure Set_Return_Value_For_Is_Treadable_In_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Treadable_In_Direction := Return_Value;
      This.Return_Values.Is_Treadable_In_Direction_Existence := TRUE;
   end Set_Return_Value_For_Is_Treadable_In_Direction;

   procedure Set_Return_Value_For_Is_Not_Treadable_In_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Not_Treadable_In_Direction := Return_Value;
      This.Return_Values.Is_Not_Treadable_In_Direction_Existence := TRUE;
   end Set_Return_Value_For_Is_Not_Treadable_In_Direction;

   procedure Set_Id (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Id := Return_Value;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Orientation (
      This         : in out Street.Mock.Object;
      Return_Value : in     Direction.Orientation) is
   begin
      This.Mock_Values.Orientation := Return_Value;
      This.Mock_Values.Orientation_Existence := TRUE;
   end Set_Orientation;

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Street.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
      This.Return_Values.Equality_Operator_Existence := TRUE;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Find_Lanes_By_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Lanes_By_Direction := Return_Value;
      This.Return_Values.Find_Lanes_By_Direction_Existence := TRUE;
   end Set_Return_Value_For_Find_Lanes_By_Direction;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Street.Mock.Object;
      Container_Id : in     Infra_Id;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By.Insert (Key      => Container_Id,
                                                 New_Item => Return_Value);
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

end Reactive.Infrastructure.Street.Mock;
