with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Way.Bikeway.Mock is

   function Create return Bikeway.Mock.Reference
   is (new Bikeway.Mock.Object);

   procedure Add_Lane (This    : in out Bikeway.Mock.Object;
                       Lane_Id : in     Infra_Id;
                       Added   :    out Boolean) is
   begin
      if not This.Return_Values.Add_Lane_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Added",
            Procedure_Name => "Add_Lane",
            Package_Name   => "Reactive.Infrastructure.Way.Bikeway.Mock");
      end if;

      Added := This.Return_Values.Add_Lane;
   end Add_Lane;

   procedure Find_Lane_By_Direction (This             : in Bikeway.Mock.Object;
                                     Travel_Direction : in Direction.Straight;
                                     Lane_Id          :    out Infra_Id;
                                     Found            : out Boolean) is
   begin
      if not This.Return_Values.Find_Lane_By_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Procedure_Name => "Find_Lane_By_Direction",
            Package_Name   => "Reactive.Infrastructure.Way.Bikeway.Mock");
      end if;

      Lane_Id := This.Return_Values.Find_Lane_By_Direction;
      Found := This.Return_Values.Find_Lane_By_Direction_Found;
   end Find_Lane_By_Direction;

   function Find_Street (This : in Bikeway.Mock.Object)
                         return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name  => "Reactive.Infrastructure.Way.Bikeway.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Get_Id (This : in Bikeway.Mock.Object) return Infra_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Package_Name  => "Reactive.Infrastructure.Way.Bikeway.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Is_Contained_By (This         : in Bikeway.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name  => "Reactive.Infrastructure.Way.Bikeway.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Bikeway.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Id (
      This         : in out Bikeway.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Id := Return_Value;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Bikeway.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

   procedure Set_Return_Value_For_Add_Lane (
      This  : in out Bikeway.Mock.Object;
      Added : in     Boolean) is
   begin
      This.Return_Values.Add_Lane := Added;
      This.Return_Values.Add_Lane_Existence := TRUE;
   end Set_Return_Value_For_Add_Lane;

   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Bikeway.Mock.Object;
      Lane_Id : in     Infra_Id;
      Found   : in     Boolean) is
   begin
      This.Return_Values.Find_Lane_By_Direction := Lane_Id;
      This.Return_Values.Find_Lane_By_Direction_Found := Found;
      This.Return_Values.Find_Lane_By_Direction_Existence := TRUE;
   end Set_Return_Value_For_Find_Lane_By_Direction;

end Reactive.Infrastructure.Way.Bikeway.Mock;
