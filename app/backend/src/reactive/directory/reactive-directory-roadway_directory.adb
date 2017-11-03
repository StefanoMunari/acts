with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Roadway_Directory is

  ----------------------------------------------------------------------------
   -- RoadWAY DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Roadway_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Roadway_Directory.Include (Key      => Infrastructure_Id,
                                    New_Item => Infrastructure);
         Added := Roadway_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean is
      begin
         return Roadway_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Roadway_Id : in Infra_Id)
                         return Reactive.Infrastructure.Way.Roadway.Reference is
      begin
         if not Roadway_Directory.Contains (Key => Roadway_Id) then
            Raise_Roadway_Missing_Exception (Roadway_Id);
         end if;
         return Roadway_Directory.Element (Key => Roadway_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Roadway_Id : in Infra_Id; Found : out Boolean)
                         return Reactive.Infrastructure.Way.Roadway.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Roadway_Id);
         exception
          when Roadway_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Roadway_Directory.Clear;
      end;

   end Directory;

end Reactive.Directory.Roadway_Directory;
