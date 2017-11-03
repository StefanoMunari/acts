with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Street.Utils.Mock is

   function Create return Street.Utils.Mock.Reference
   is (new Infrastructure.Street.Utils.Mock.Object);

   function Get_Id (This : in Street.Utils.Mock.Object;
                    Infrastructure_Id : in Infra_Id) return Infra_Id
   is
   begin
      if not This.Return_Values.Get_Id.Contains (Infrastructure_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Function_Param => "Infrastructure_Id = "
              & Infra_Id'Image (Infrastructure_Id),
             Package_Name  => "Street.Utils.Mock");
      end if;

      return This.Return_Values.Get_Id.Element (Key => Infrastructure_Id);
   end Get_Id;

   function Is_Not_Treadable_In_Direction (
      This      : in Street.Utils.Mock.Object;
      Street_Id : in Infra_Id;
      Direction : in Shared.Direction.Cardinal)
      return Boolean
   is
   begin
      if not This.Return_Values.Is_Not_Treadable_In_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
            (Function_Name => "Is_Not_Treadable_In_Direction",
             Package_Name  => "Street.Utils.Mock");
      end if;

      return This.Return_Values.Is_Not_Treadable_In_Direction;
   end Is_Not_Treadable_In_Direction;

   function Get_Orientation (This : in Street.Utils.Mock.Object;
                             Street_Id: in Infra_Id)
                             return Direction.Orientation is
   begin
      if not This.Return_Values.Get_Orientation_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
            (Function_Name => "Get_Orientation",
             Package_Name  => "Street.Utils.Mock");
      end if;

      return This.Return_Values.Get_Orientation;
   end Get_Orientation;

   function Find_Lanes_By_Direction (This : in Street.Utils.Mock.Object;
                                     Street_Id        : in Infra_Id;
                                     Travel_Direction : in Direction.Straight)
                                     return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Lanes_By_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
            (Function_Name => "Find_Lanes_By_Direction",
             Package_Name  => "Street.Utils.Mock");
      end if;

      return This.Return_Values.Find_Lanes_By_Direction;
   end Find_Lanes_By_Direction;

   function Is_Contained_By (This : in Street.Utils.Mock.Object;
                             Street_Id, Container_Id : in Infra_Id)
                             return Boolean
   is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
            (Function_Name => "Is_Contained_By",
             Package_Name  => "Street.Utils.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   procedure Set_Return_Value_For_Get_Id (
      This : in out Street.Utils.Mock.Object;
      Infrastructure_Id, Street_Id : in Infra_Id) is
   begin
      This.Return_Values.Get_Id.Insert (Key      => Infrastructure_Id,
                                        New_Item => Street_Id);
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Is_Not_Treadable_In_Direction (
      This : in out Street.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Not_Treadable_In_Direction := Return_Value;
      This.Return_Values.Is_Not_Treadable_In_Direction_Existence := TRUE;
   end Set_Return_Value_For_Is_Not_Treadable_In_Direction;

   procedure Set_Return_Value_For_Get_Orientation (
      This : in out Street.Utils.Mock.Object;
      Return_Value: in Direction.Orientation) is
   begin
      This.Return_Values.Get_Orientation := Return_Value;
      This.Return_Values.Get_Orientation_Existence := TRUE;
   end Set_Return_Value_For_Get_Orientation;

   procedure Set_Return_Value_For_Find_Lanes_By_Direction (
      This : in out Street.Utils.Mock.Object;
      Return_Value : in Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Lanes_By_Direction := Return_Value;
      This.Return_Values.Find_Lanes_By_Direction_Existence := TRUE;
   end Set_Return_Value_For_Find_Lanes_By_Direction;

   procedure Set_Return_Value_For_Is_Contained_By (
      This : in out Street.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

end Reactive.Infrastructure.Street.Utils.Mock;
