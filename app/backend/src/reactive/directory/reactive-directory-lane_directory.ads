with Ada.Containers.Ordered_Maps;

with Reactive.Infrastructure.Lane;

package Reactive.Directory.Lane_Directory is
   use Reactive.Infra_Id_Type;

   package Lane_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Lane.Reference,
        "<"          => "<",
        "="          => Infrastructure.Lane."=");

   protected type Directory is

      procedure Add
        (Infrastructure : aliased in out Reactive.Infrastructure.Lane.Reference;
         Added          : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean;

      function Find_By_Id (Lane_Id : in Infra_Id)
                          return Reactive.Infrastructure.Lane.Reference;

      function Safe_Find_By_Id (Lane_Id : in Infra_Id;
                                Found : out Boolean)
              return Reactive.Infrastructure.Lane.Reference;

      procedure Clear;

   private
      Lane_Directory : Lane_By_Id.Map;
   end Directory;

end Reactive.Directory.Lane_Directory;
