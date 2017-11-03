with Ada.Containers.Ordered_Maps;

with Reactive.Infrastructure.Way.Bikeway;

package Reactive.Directory.Bikeway_Directory is
   use Reactive.Infra_Id_Type;

   package Bikeway_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Way.Bikeway.Reference,
        "<"          => "<",
        "="          => Infrastructure.Way.Bikeway."=");

   protected type Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
         Added          : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean;

      function Find_By_Id (Bikeway_Id : in Infra_Id)
      return Reactive.Infrastructure.Way.Bikeway.Reference;

      function Safe_Find_By_Id (Bikeway_Id : in Infra_Id; Found : out Boolean)
      return Reactive.Infrastructure.Way.Bikeway.Reference;

      procedure Clear;

   private
      Bikeway_Directory : Bikeway_By_Id.Map;
   end Directory;

end Reactive.Directory.Bikeway_Directory;
