with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Intersection.Mock is

   function Create return Intersection.Mock.Reference
   is (new Intersection.Mock.Object);

   procedure Tread (
      This         : in out Intersection.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is
   begin
      if not This.Return_Values.Tread_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Advanced",
            Procedure_Name => "Tread",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      Advanced := This.Return_Values.Tread;
   end Tread;

   function Is_Connected_At_Direction (This : Intersection.Mock.Object;
                                       Direction : Shared.Direction.Cardinal)
                                       return Boolean is
   begin
      return This.Mock_Values.Streets_Existence (Direction);
   end Is_Connected_At_Direction;

   function Get_Street (This      : in     Intersection.Mock.Object;
                        Direction : in out Shared.Direction.Cardinal)
   return Infra_Id is
   begin
      if not This.Mock_Values.Streets_Existence (Direction) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Street",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Mock_Values.Streets (Direction);
   end Get_Street;

   procedure Find_Street_Direction (This : Intersection.Mock.Object;
                                    Street_Id : Infra_Id;
                                    Street_Direction : out Direction.Cardinal;
                                    Found : out Boolean) is
   begin
      if not This.Return_Values.Find_Street_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Find_Street_Direction",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      Street_Direction := This.Return_Values.Find_Street_Direction;
      Found := This.Return_Values.Find_Street_Direction_Found;
   end Find_Street_Direction;

   function Find_Streets_Connected_With_Intersection (
      This : in Intersection.Mock.Object) return Infra_Id_Set.Set is
   begin
      if not This.Return_Values
        .Find_Streets_Connected_With_Intersection_Existence
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Find_Streets_Connected_With_Intersection",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Return_Values.Find_Streets_Connected_With_Intersection;
   end Find_Streets_Connected_With_Intersection;

   function Get_Id (This : in Intersection.Mock.Object) return Infra_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Id",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Find_Intersections (This : in Intersection.Mock.Object)
                                return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Intersections_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Find_Intersections",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Return_Values.Find_Intersections;
   end Find_Intersections;

   function Count_Streets (This : in Intersection.Mock.Object)
                           return Natural is
   begin
      if not This.Mock_Values.Streets_Count_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Count_Streets",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Mock_Values.Streets_Count;
   end Count_Streets;

   function Exists_Street_For_Direction (
      This      : in Intersection.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
      return Boolean is
   begin
      if not This.Return_Values.Exists_Street_For_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Exists_Street_For_Direction",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Return_Values.Exists_Street_For_Direction;
   end Exists_Street_For_Direction;

   function "="(This, Other : Intersection.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Equality_Operator_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "'='",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Return_Values.Equality_Operator;
   end "=";

   function Is_Contained_By (This         : in Intersection.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Is_Contained_By",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   function Is_Fully_Connected (This : in out Intersection.Mock.Object)
                         return Boolean is
   begin
      if not This.Mock_Values.Fully_Connected_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Is_Not_Fully_Connected",
            Package_Name   => "Reactive.Infrastructure.Intersection.Mock");
      end if;

      return This.Mock_Values.Fully_Connected;
   end Is_Fully_Connected;

   function Is_Not_Fully_Connected (This : in out Intersection.Mock.Object)
                         return Boolean is
   begin
      return not This.Is_Fully_Connected;
   end Is_Not_Fully_Connected;

   procedure Connect_Street (This      : in out Intersection.Mock.Object;
                             Street_Id :        Infra_Id;
                             Stretches : in     Infra_Id_List.List;
                             Direction :        Shared.Direction.Cardinal) is
   begin
      This.Mock_Values.Streets (Direction) := Street_Id;
      This.Mock_Values.Streets_Existence (Direction) := TRUE;
   end Connect_Street;

   procedure Increment_Streets (This : in out Intersection.Mock.Object) is
   begin
      This.Mock_Values.Streets_Count := This.Mock_Values.Streets_Count + 1;
   end Increment_Streets;

   procedure Set_Return_Value_For_Tread (
      This     : in out Intersection.Mock.Object;
      Advanced : in Boolean) is
   begin
      This.Return_Values.Tread := Advanced;
      This.Return_Values.Tread_Existence := TRUE;
   end Set_Return_Value_For_Tread;

   procedure Set_Street (
      This         : in out Intersection.Mock.Object;
      Direction    : in Shared.Direction.Cardinal;
      Return_Value : in Infra_Id) is
   begin
      This.Mock_Values.Streets (Direction) := Return_Value;
      This.Mock_Values.Streets_Existence (Direction) := TRUE;
   end Set_Street;

   procedure Set_Return_Value_For_Find_Street_Direction (
      This      : in out Intersection.Mock.Object;
      Direction : in Shared.Direction.Cardinal;
      Found : in Boolean) is
   begin
      This.Return_Values.Find_Street_Direction := Direction;
      This.Return_Values.Find_Street_Direction_Found := Found;
      This.Return_Values.Find_Street_Direction_Existence := TRUE;
   end Set_Return_Value_For_Find_Street_Direction;

   procedure Set_Return_Value_For_Find_Streets_Connected_With_Intersection (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Streets_Connected_With_Intersection
        := Return_Value;
      This.Return_Values.Find_Streets_Connected_With_Intersection_Existence := TRUE;
   end Set_Return_Value_For_Find_Streets_Connected_With_Intersection;

   procedure Set_Id (
      This         : in out Intersection.Mock.Object;
      Id : in Infra_Id) is
   begin
      This.Mock_Values.Id := Id;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Intersection_Type (
      This              : in out Intersection.Mock.Object;
      Intersection_Type : in Intersection.Intersection_Type) is
   begin
      This.Mock_Values.Intersection_Type := Intersection_Type;
      This.Mock_Values.Intersection_Type_Existence := TRUE;
   end Set_Intersection_Type;

   procedure Set_Fully_Connected (
      This : in out Intersection.Mock.Object;
      Fully_Connected : in Boolean) is
   begin
      This.Mock_Values.Fully_Connected := Fully_Connected;
      This.Mock_Values.Fully_Connected_Existence := TRUE;
   end Set_Fully_Connected;

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Intersection.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
      This.Return_Values.Find_Intersections_Existence := TRUE;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Count_Streets (
      This         : in out Intersection.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Mock_Values.Streets_Count := Return_Value;
      This.Mock_Values.Streets_Count_Existence := TRUE;
   end Set_Return_Value_For_Count_Streets;

   procedure Set_Return_Value_For_Exists_Street_For_Direction (
      This         : in out Intersection.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Exists_Street_For_Direction := Return_Value;
      This.Return_Values.Exists_Street_For_Direction_Existence := TRUE;
   end Set_Return_Value_For_Exists_Street_For_Direction;

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
      This.Return_Values.Equality_Operator_Existence := TRUE;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

   procedure Initialize (This : in out Intersection.Mock.Object) is
   begin
      for Direction in Shared.Direction.Cardinal
      loop
         This.Mock_Values.Streets_Existence (Direction) := FALSE;
      end loop;
   end Initialize;

end Reactive.Infrastructure.Intersection.Mock;
