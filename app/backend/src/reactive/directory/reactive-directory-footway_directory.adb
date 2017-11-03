with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Footway_Directory is

  ----------------------------------------------------------------------------
   -- FOOTWAY DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Footway.Reference;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Footway_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Footway_Directory.Include (Key      => Infrastructure_Id,
                                    New_Item => Infrastructure);
         Added := Footway_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean is
      begin
         return Footway_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Footway_Id : in Infra_Id)
      return Reactive.Infrastructure.Way.Footway.Reference is
      begin
         if not Footway_Directory.Contains (Key => Footway_Id) then
            Raise_Footway_Missing_Exception (Footway_Id);
         end if;
         return Footway_Directory.Element (Key => Footway_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Footway_Id : in Infra_Id; Found : out Boolean)
      return Reactive.Infrastructure.Way.Footway.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Footway_Id);
         exception
          when Footway_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Footway_Directory.Clear;
      end Clear;

   end Directory;

end Reactive.Directory.Footway_Directory;
