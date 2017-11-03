with Reactive.Treadable;

with Shared.Infra_Id_Set;

package Reactive.Intersectable is

   package Infra_Id_Set renames Shared.Infra_Id_Set;

   type Object is interface;
   type Reference is access all Intersectable.Object'Class;

   not overriding
   function Find_Intersections (This : in Intersectable.Object)
   return Infra_Id_Set.Set is abstract;

end Reactive.Intersectable;
