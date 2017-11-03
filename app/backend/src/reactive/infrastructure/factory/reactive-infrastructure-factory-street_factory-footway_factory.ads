with Reactive.Infrastructure.Factory.Street_Factory;
with Reactive.Infrastructure.Lane;
with Reactive.Infrastructure.Stretch;

with Shared.Direction;
with Shared.Infra_Id_List;

package Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory is
   package Street_Factory
      renames Reactive.Infrastructure.Factory.Street_Factory;
   package Infra_Id_List renames Shared.Infra_Id_List;
   use Shared.Direction;

   type Object is new Street_Factory.Object with null record;
   type Reference is access all Footway_Factory.Object'Class;

   overriding
   function Create_Stretch (This : in out Footway_Factory.Object)
      return Stretch.Reference;

   overriding
   function Create_Lane (This : in out Footway_Factory.Object)
      return Lane.Reference;

end Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory;
