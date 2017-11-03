with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Lane.Mock is

   function Create return Lane.Mock.Reference
   is (new Lane.Mock.Object);

   procedure Enter (
      This         : in out Lane.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      null;
   end Enter;

   procedure Append_Stretch (
      This       : in out Lane.Mock.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean) is
   begin
      if not This.Return_Values.Append_Stretch_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Added",
            Procedure_Name  => "Append_Stretch",
            Package_Name   => "Reactive.Infrastructure.Lane.Mock");
      end if;

      Added := This.Return_Values.Append_Stretch;
   end Append_Stretch;

   function Count_Stretches (This : in Lane.Mock.Object)
      return Natural is
   begin
      if not This.Return_Values.Count_Stretches_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "Count_Stretches",
            Package_Name   => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Return_Values.Count_Stretches;
   end Count_Stretches;

   procedure Find_Stretch_Position (
      This             : in     Lane.Mock.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found    : out Boolean) is
   begin
      if not This.Return_Values.Find_Stretch_Position_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name  => "Find_Stretch_Position",
            Package_Name   => "Reactive.Infrastructure.Lane.Mock");
      end if;

      Stretch_Position := This.Return_Values.Find_Stretch_Position;
      Found := This.Return_Values.Find_Stretch_Position_Found;
   end Find_Stretch_Position;

   function "=" (This, Outher : in Lane.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Equality_Operator_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "'='",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Return_Values.Equality_Operator;
   end "=";

   function Find_Street (This : in Lane.Mock.Object)
                         return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Find_Street",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Get_Id (This : in Lane.Mock.Object) return Infra_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Id",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Get_Direction (This : Lane.Mock.Object)
                           return Shared.Direction.Straight is
   begin
      if not This.Mock_Values.Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Direction",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Mock_Values.Direction;
   end Get_Direction;

   function Find_Intersections (
      This : in Lane.Mock.Object) return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Intersections_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Find_Intersections",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Return_Values.Find_Intersections;
   end Find_Intersections;

   procedure Add_Intersection (
      This            : in out Lane.Mock.Object;
      Intersection_Id : in Infra_Id;
      Added           : out Boolean) is
   begin
      if not This.Return_Values.Add_Intersection_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Added",
            Procedure_Name  => "Add_Intersection",
            Package_Name   => "Reactive.Infrastructure.Lane.Mock");
      end if;

      Added := This.Return_Values.Add_Intersection;
   end Add_Intersection;

   function Is_Contained_By (This         : in Lane.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Is_Contained_By",
            Package_Name  => "Reactive.Infrastructure.Lane.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Lane.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
      This.Return_Values.Equality_Operator_Existence := TRUE;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Id (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Id := Return_Value;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
      This.Return_Values.Find_Intersections_Existence := TRUE;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Append_Stretch (
      This  : in out Lane.Mock.Object;
      Added : in Boolean) is
   begin
      This.Return_Values.Append_Stretch := Added;
      This.Return_Values.Append_Stretch_Existence := TRUE;
   end Set_Return_Value_For_Append_Stretch;

   procedure Set_Return_Value_For_Count_Stretches (
      This         : in out Lane.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Count_Stretches := Return_Value;
      This.Return_Values.Count_Stretches_Existence := TRUE;
   end Set_Return_Value_For_Count_Stretches;

   procedure Set_Return_Value_For_Find_Stretch_Position (
      This     : in out Lane.Mock.Object;
      Position : in Natural;
      Found : in Boolean) is
   begin
      This.Return_Values.Find_Stretch_Position := Position;
      This.Return_Values.Find_Stretch_Position_Found := Found;
      This.Return_Values.Find_Stretch_Position_Existence := TRUE;
   end Set_Return_Value_For_Find_Stretch_Position;

   procedure Set_Direction (
      This         : in out Lane.Mock.Object;
      Return_Value : in Direction.Straight) is
   begin
      This.Mock_Values.Direction := Return_Value;
      This.Mock_Values.Direction_Existence := TRUE;
   end Set_Direction;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Lane.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

   procedure Set_Return_Value_For_Add_Intersection (
      This  : in out Lane.Mock.Object;
      Added : in Boolean) is
   begin
      This.Return_Values.Add_Intersection := Added;
      This.Return_Values.Add_Intersection_Existence := TRUE;
   end Set_Return_Value_For_Add_Intersection;

end Reactive.Infrastructure.Lane.Mock;
