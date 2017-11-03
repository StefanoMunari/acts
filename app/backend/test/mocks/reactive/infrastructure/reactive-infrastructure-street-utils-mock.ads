with Active.Agent;

with Shared.Infra_Id_Map;
with Shared.Infra_Id_Multimap;
limited with Reactive.District.Mock;

package Reactive.Infrastructure.Street.Utils.Mock is

   package Agent             renames Active.Agent;
   package Infra_Id_Map      renames Shared.Infra_Id_Map;
   package Infra_Id_Multimap renames Shared.Infra_Id_Multimap;

   type Object (<>) is new Street.Utils.Object with private;
   type Reference is access all Street.Utils.Mock.Object'Class;

   function Create return Street.Utils.Mock.Reference;

   function Get_Id (This : in Street.Utils.Mock.Object;
                    Infrastructure_Id : in Infra_Id)
                    return Infra_Id;

   function Is_Not_Treadable_In_Direction (
      This      : in Street.Utils.Mock.Object;
      Street_Id : in Infra_Id;
      Direction :    Shared.Direction.Cardinal)
   return Boolean;

   overriding
   function Get_Orientation (This : in Street.Utils.Mock.Object;
                             Street_Id: in Infra_Id)
                             return Direction.Orientation;

   overriding
   function Find_Lanes_By_Direction (This : in Street.Utils.Mock.Object;
                                     Street_Id        : in Infra_Id;
                                     Travel_Direction : in Direction.Straight)
                                     return Infra_Id_Set.Set;

   overriding
   function Is_Contained_By (This : in Street.Utils.Mock.Object;
                             Street_Id, Container_Id : in Infra_Id)
                             return Boolean;

   procedure Set_Return_Value_For_Get_Id (
      This              : in out Street.Utils.Mock.Object;
      Infrastructure_Id : in Infra_Id;
      Street_Id         : in Infra_Id);

   procedure Set_Return_Value_For_Is_Not_Treadable_In_Direction (
      This         : in out Street.Utils.Mock.Object;
      Return_Value : in Boolean);

   procedure Set_Return_Value_For_Get_Orientation (
      This         : in out Street.Utils.Mock.Object;
      Return_Value : in Direction.Orientation);

   procedure Set_Return_Value_For_Find_Lanes_By_Direction (
      This         : in out Street.Utils.Mock.Object;
      Return_Value : in Infra_Id_Set.Set);

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Street.Utils.Mock.Object;
      Return_Value : in Boolean);

private
   type Return_Values_Collection is record
      Get_Id : Infra_Id_Map.Map;
      Is_Not_Treadable_In_Direction : Boolean;
      Is_Not_Treadable_In_Direction_Existence : Boolean := FALSE;
      Get_Orientation : Direction.Orientation;
      Get_Orientation_Existence : Boolean := FALSE;
      Find_Lanes_By_Direction : Infra_Id_Set.Set;
      Find_Lanes_By_Direction_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
   end record;

   type Object is new Street.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Street.Utils.Mock;
