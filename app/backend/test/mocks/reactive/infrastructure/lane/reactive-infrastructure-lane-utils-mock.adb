with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Lane.Utils.Mock is

   function Create return Lane.Utils.Mock.Reference
   is (new Lane.Utils.Mock.Object);

   procedure Enter (
      This         : in     Lane.Utils.Mock.Object;
      Lane_Id      : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      null;
   end Enter;

   function Find_Street (This    : in Lane.Utils.Mock.Object;
                         Lane_Id : in Infra_Id)
                         return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name  => "Lane.Utils.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Find_Intersections (This    : in Lane.Utils.Mock.Object;
                                Lane_Id : in Infra_Id)
                                return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Intersections_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Intersections",
            Package_Name  => "Lane.Utils.Mock");
      end if;

      return This.Return_Values.Find_Intersections;
   end Find_Intersections;

   function Count_Stretches (This    : in Lane.Utils.Mock.Object;
                             Lane_Id : in Infra_Id) return Natural is
   begin
      if not This.Return_Values.Count_Stretches_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Count_Stretches",
            Package_Name  => "Lane.Utils.Mock");
      end if;

      return This.Return_Values.Count_Stretches;
   end Count_Stretches;

   procedure Find_Stretch_Position (
      This                : in     Lane.Utils.Mock.Object;
      Lane_Id, Stretch_Id : in     Infra_Id;
      Stretch_Position    :    out Natural;
      Found               :    out Boolean) is
   begin
      if not This.Return_Values.Find_Stretch_Position.Contains (Stretch_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Stretch_Position",
            Procedure_Name => "Find_Stretch_Position",
            Package_Name => "Lane.Utils.Mock");
      end if;

      if not This.Return_Values.Find_Stretch_Position_Found.Contains (Stretch_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Found",
            Procedure_Name => "Find_Stretch_Position",
            Package_Name => "Lane.Utils.Mock");
      end if;

      Stretch_Position :=
        This.Return_Values.Find_Stretch_Position.Element (Key => Stretch_Id);
      Found :=
         This.Return_Values.Find_Stretch_Position_Found.Element
           (Key => Stretch_Id);
   end Find_Stretch_Position;

   function Get_Direction (This    : in Lane.Utils.Mock.Object;
                           Lane_Id : in Infra_Id)
      return Direction.Straight is
   begin
      if not This.Return_Values.Get_Direction.Contains (Key => Lane_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Direction",
            Package_Name  => "Lane.Utils.Mock");
      end if;

      return This.Return_Values.Get_Direction.Element (Key => Lane_Id);
   end Get_Direction;

   procedure Add_Intersection (
      This            : in Lane.Utils.Mock.Object;
      Lane_Id,
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean) is
   begin
      if not This.Return_Values.Add_Intersection_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Added",
            Procedure_Name => "Add_Intersection",
            Package_Name => "Lane.Utils.Mock");
      end if;

      Added := This.Return_Values.Add_Intersection;
   end Add_Intersection;

   function Is_Contained_By (This : in Lane.Utils.Mock.Object;
                             Lane_Id, Container_Id : in Infra_Id)
                             return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name  => "Lane.Utils.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
      This.Return_Values.Find_Intersections_Existence := TRUE;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Count_Stretches (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Count_Stretches           := Return_Value;
      This.Return_Values.Count_Stretches_Existence := TRUE;
   end Set_Return_Value_For_Count_Stretches;

   procedure Set_Return_Value_For_Find_Stretch_Position (
      This         : in out Lane.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Natural;
      Found        : in     Boolean) is
   begin
      This.Return_Values.Find_Stretch_Position.Insert
         (Key      => Stretch_Id, New_Item => Return_Value);
      This.Return_Values.Find_Stretch_Position_Found.Insert
         (Key      => Stretch_Id, New_Item => Found);
   end Set_Return_Value_For_Find_Stretch_Position;

   procedure Set_Return_Value_For_Get_Direction
     (This      : in out Lane.Utils.Mock.Object;
      Lane_Id   : in     Infra_Id;
      Direction : in     Shared.Direction.Straight) is
   begin
      This.Return_Values.Get_Direction.Insert (Key      => Lane_Id,
                                               New_Item => Direction);
   end Set_Return_Value_For_Get_Direction;

   procedure Set_Return_Value_For_Add_Intersection (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Add_Intersection := Return_Value;
      This.Return_Values.Add_Intersection_Existence := TRUE;
   end Set_Return_Value_For_Add_Intersection;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

end Reactive.Infrastructure.Lane.Utils.Mock;
