with Ada.Strings.Unbounded;
with Mock.Exceptions;
with Shared.Infra_Id_To_String_Map;

use Mock.Exceptions;

package body Reactive.Infrastructure.Intersection.Utils.Mock is

   package SU renames Ada.Strings.Unbounded;
   package Infra_Id_To_String_Map renames Shared.Infra_Id_To_String_Map;

   function Create return Intersection.Utils.Mock.Reference
   is (new Intersection.Utils.Mock.Object);

   procedure Find_Street_Direction
     (This : in Intersection.Utils.Mock.Object;
      Intersection_Id,
      Street_Id        : in     Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean)
   is
      Key : String
        := Infra_Id'Image(Intersection_Id) & "-" & Infra_Id'Image(Street_Id);
      Objects : Infra_Id_To_String_Map.Map;
   begin
      Objects.Insert (Key      => Street_Id,
                      New_Item => "Street");
      Objects.Insert (Key      => Intersection_Id,
                      New_Item => "Intersection");

      if not This.Return_Values.Find_Street_Direction.Contains (Key) then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Direction",
            Procedure_Name => "Find_Street_Direction",
            Package_Name   => "Reactive.Infrastructure.Intersection.Utils",
            Objects        => Objects);
      end if;

      if not This.Return_Values.Find_Street_Direction_Found.Contains (Key) then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Found",
            Procedure_Name => "Find_Street_Direction",
            Package_Name   => "Reactive.Infrastructure.Intersection.Utils",
            Objects        => Objects);
      end if;

      Street_Direction
        := This.Return_Values.Find_Street_Direction.Element (Key);
      Found := This.Return_Values.Find_Street_Direction_Found.Element (Key);
   end Find_Street_Direction;

   function Find_Streets_Connected_With_Intersection
     (This : in Intersection.Utils.Mock.Object;
      Intersection_Id : in Infra_Id) return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Streets_Connected_With_Intersection
        .Contains (Intersection_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Streets_Connected_With_Intersection",
            Function_Param => "Intersection_Id = "
            & Infra_Id'Image (Intersection_Id),
            Package_Name  => "Reactive.Infrastructure.Intersection.Utils");
      end if;

      return This.Return_Values.Find_Streets_Connected_With_Intersection
        .Element (Intersection_Id);
   end Find_Streets_Connected_With_Intersection;

   procedure Set_Return_Value_For_Find_Street_Direction
     (This            : in out Intersection.Utils.Mock.Object;
      Intersection_Id, Street_Id : in Infra_Id;
      Direction       : in Shared.Direction.Cardinal;
      Found           : in Boolean)
   is
      Key : String
        := Infra_Id'Image (Intersection_Id) & "-" & Infra_Id'Image (Street_Id);
   begin
      This.Return_Values.Find_Street_Direction
        .Insert (Key      => Key,
                 New_Item => Direction);

      This.Return_Values.Find_Street_Direction_Found
        .Insert (Key      => Key,
                 New_Item => Found);
   end Set_Return_Value_For_Find_Street_Direction;

   procedure Add_Return_Value_For_Find_Streets_Connected_With_Intersection
     (This            : in out Intersection.Utils.Mock.Object;
      Intersection_Id, Return_Value : in Infra_Id)
   is
      Streets : Infra_Id_Set.Set;
   begin
      if This.Return_Values.Find_Streets_Connected_With_Intersection
        .Contains (Key => Intersection_Id) then

         Streets := This.Return_Values.Find_Streets_Connected_With_Intersection
           .Element (Key => Intersection_Id);

         Streets.Insert (New_Item => Return_Value);

         This.Return_Values.Find_Streets_Connected_With_Intersection
           .Replace (Key => Intersection_Id,
                     New_Item => Streets);
      else
         Streets.Insert (New_Item => Return_Value);

         This.Return_Values.Find_Streets_Connected_With_Intersection
           .Insert (Key => Intersection_Id,
                    New_Item => Streets);
      end if;
   end Add_Return_Value_For_Find_Streets_Connected_With_Intersection;

end Reactive.Infrastructure.Intersection.Utils.Mock;
