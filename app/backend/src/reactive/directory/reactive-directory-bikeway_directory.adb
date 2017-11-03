with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Bikeway_Directory is

  ----------------------------------------------------------------------------
   -- BikeWAY DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Bikeway_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Bikeway_Directory.Include (Key      => Infrastructure_Id,
                                    New_Item => Infrastructure);
         Added := Bikeway_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean is
      begin
         return Bikeway_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Bikeway_Id : in Infra_Id)
      return Reactive.Infrastructure.Way.Bikeway.Reference is
      begin
         if not Bikeway_Directory.Contains (Key => Bikeway_Id) then
            Raise_Bikeway_Missing_Exception (Bikeway_Id);
         end if;
         return Bikeway_Directory.Element (Key => Bikeway_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Bikeway_Id : in Infra_Id; Found : out Boolean)
      return Reactive.Infrastructure.Way.Bikeway.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Bikeway_Id);
         exception
          when Bikeway_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Bikeway_Directory.Clear;
      end Clear;

   end Directory;

end Reactive.Directory.Bikeway_Directory;
