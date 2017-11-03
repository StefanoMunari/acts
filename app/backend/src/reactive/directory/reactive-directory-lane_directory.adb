with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Lane_Directory is

  ----------------------------------------------------------------------------
   -- Lane DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure : aliased in out Reactive.Infrastructure.Lane.Reference;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Lane_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Lane_Directory.Include (Key      => Infrastructure_Id,
                                 New_Item => Infrastructure);
         Added := Lane_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean is
      begin
         return Lane_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Lane_Id : in Infra_Id)
                         return Reactive.Infrastructure.Lane.Reference is
      begin
         if not Lane_Directory.Contains (Key => Lane_Id) then
            Raise_Lane_Missing_Exception (Lane_Id);
         end if;
         return Lane_Directory.Element (Key => Lane_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Lane_Id : in Infra_Id;
                                Found : out Boolean)
              return Reactive.Infrastructure.Lane.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Lane_Id);
         exception
          when Lane_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Lane_Directory.Clear;
      end;

   end Directory;

end Reactive.Directory.Lane_Directory;
