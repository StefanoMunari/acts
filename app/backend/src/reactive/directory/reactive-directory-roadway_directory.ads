with Ada.Containers.Ordered_Maps;

with Reactive.Infrastructure.Way.Roadway;

package Reactive.Directory.Roadway_Directory is
   use Reactive.Infra_Id_Type;

   package Roadway_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Way.Roadway.Reference,
        "<"          => "<",
        "="          => Infrastructure.Way.Roadway."=");

   protected type Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
         Added          : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean;

      function Find_By_Id (Roadway_Id : in Infra_Id)
                          return Reactive.Infrastructure.Way.Roadway.Reference;

      function Safe_Find_By_Id (Roadway_Id : in Infra_Id; Found : out Boolean)
                          return Reactive.Infrastructure.Way.Roadway.Reference;

      procedure Clear;

   private
      Roadway_Directory : Roadway_By_Id.Map;
   end Directory;

end Reactive.Directory.Roadway_Directory;
