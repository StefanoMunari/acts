with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Reactive.Infrastructure.Street;

with Shared.Shared_References_Street;

package Reactive.Directory.Street_Directory is

-- libs
   package G_JSON        renames GNATCOLL.JSON;
-- reactive
   package Street_Pkg    renames Reactive.Infrastructure.Street;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   use Reactive.Infra_Id_Type;

   package Street_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Reactive.Infrastructure.Street.Reference,
        "<"          => "<",
        "="          => Infrastructure.Street."=");

   protected type Directory is

      procedure Add (
         SR_Street :     SR_Street_Pkg.Shared_Reference;
         Added     : out Boolean);

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean;

      function Find_By_Id (Street_Id : in Infra_Id)
      return Reactive.Infrastructure.Street.Reference;

      function Safe_Find_By_Id (Street_Id : in     Infra_Id;
                                Found     :    out Boolean)
      return Reactive.Infrastructure.Street.Reference;

      procedure Clear;

      function Dump
      return G_JSON.JSON_Value;

   private
      Street_Directory : Street_By_Id.Map;
   end Directory;

end Reactive.Directory.Street_Directory;
