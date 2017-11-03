with Ada.Containers.Ordered_Maps;

with Reactive.Infrastructure.Stretch;

package Reactive.Directory.Stretch_Directory is
   use Reactive.Infra_Id_Type;

   package Stretch_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Stretch.Reference,
        "<"          => "<",
        "="          => Infrastructure.Stretch."=");

   protected type Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Stretch.Reference;
         Added          : out Boolean);

      function Contains_Infrastructure (Stretch_Id : in Infra_Id)
      return Boolean;

      function Find_By_Id (Stretch_Id : in Infra_Id)
      return Reactive.Infrastructure.Stretch.Reference;

      function Safe_Find_By_Id (Stretch_Id : in     Infra_Id;
                                Found      :    out Boolean)
      return Reactive.Infrastructure.Stretch.Reference;

      procedure Clear;

   private
      Stretch_Directory : Stretch_By_Id.Map;
   end Directory;

end Reactive.Directory.Stretch_Directory;
