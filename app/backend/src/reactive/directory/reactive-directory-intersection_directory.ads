with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Reactive.Infrastructure.Intersection;

package Reactive.Directory.Intersection_Directory is
-- libs
   package G_JSON           renames GNATCOLL.JSON;
-- reactive
   package Intersection_Pkg renames Reactive.Infrastructure.Intersection;
   use Reactive.Infra_Id_Type;

   package Intersection_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Intersection_Pkg.Reference,
        "<"          => "<",
        "="          => Intersection_Pkg."=");

   protected type Directory is

      procedure Add (
        Infrastructure :
          aliased in out Intersection_Pkg.Object'Class;
        Added          : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean;

      function Find_By_Id (Intersection_Id : in Infra_Id)
      return Intersection_Pkg.Reference;

      function Safe_Find_By_Id (Intersection_Id : in     Infra_Id;
                                Found           :    out Boolean)
      return Intersection_Pkg.Reference;

      procedure Clear;

      function Dump
      return G_JSON.JSON_Value;

   private
      Intersection_Directory : Intersection_By_Id.Map;
   end Directory;

end Reactive.Directory.Intersection_Directory;
