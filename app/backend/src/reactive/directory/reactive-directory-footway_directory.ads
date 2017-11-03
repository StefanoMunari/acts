with Ada.Containers.Ordered_Maps;

with Reactive.Infrastructure.Way.Footway;

package Reactive.Directory.Footway_Directory is
   use Reactive.Infra_Id_Type;

   package Footway_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Way.Footway.Reference,
        "<"          => "<",
        "="          => Infrastructure.Way.Footway."=");

   protected type Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Footway.Reference;
         Added          : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean;

      function Find_By_Id (Footway_Id : in Infra_Id)
                          return Reactive.Infrastructure.Way.Footway.Reference;

      function Safe_Find_By_Id (Footway_Id : in Infra_Id; Found : out Boolean)
                          return Reactive.Infrastructure.Way.Footway.Reference;

      procedure Clear;

   private
      Footway_Directory : Footway_By_Id.Map;
   end Directory;

end Reactive.Directory.Footway_Directory;
