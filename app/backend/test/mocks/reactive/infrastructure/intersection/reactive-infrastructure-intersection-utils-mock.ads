with Shared.Cardinal_Hashed_Map;
with Shared.Boolean_Hashed_Map;
with Shared.Infra_Id_Multimap;

package Reactive.Infrastructure.Intersection.Utils.Mock is

   package Infra_Id_Multimap renames Shared.Infra_Id_Multimap;
   package Cardinal_Hashed_Map renames Shared.Cardinal_Hashed_Map;
   package Boolean_Hashed_Map renames Shared.Boolean_Hashed_Map;

   type Object (<>) is new Intersection.Utils.Object with private;
   type Reference is access all Intersection.Utils.Mock.Object'Class;

   function Create return Intersection.Utils.Mock.Reference;

   procedure Find_Street_Direction
     (This : in Intersection.Utils.Mock.Object;
      Intersection_Id,
      Street_Id        : in Infra_Id;
      Street_Direction : out Direction.Cardinal;
      Found            : out Boolean);

   function Find_Streets_Connected_With_Intersection
     (This : in Intersection.Utils.Mock.Object;
      Intersection_Id : in Infra_Id)
      return Infra_Id_Set.Set;

   procedure Set_Return_Value_For_Find_Street_Direction
     (This            : in out Intersection.Utils.Mock.Object;
      Intersection_Id, Street_Id : in Infra_Id;
      Direction       : in Shared.Direction.Cardinal;
      Found           : in Boolean);

   procedure Add_Return_Value_For_Find_Streets_Connected_With_Intersection
     (This            : in out Intersection.Utils.Mock.Object;
      Intersection_Id, Return_Value : in Infra_Id);

private
   type Return_Values_Collection is record
      Find_Street_Direction : Cardinal_Hashed_Map.Map;
      Find_Street_Direction_Found : Boolean_Hashed_Map.Map;
      Find_Streets_Connected_With_Intersection : Infra_Id_Multimap.Map;
   end record;

   type Object is new Intersection.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Intersection.Utils.Mock;
