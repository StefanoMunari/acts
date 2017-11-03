with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Stretch_Directory is

  ----------------------------------------------------------------------------
   -- Stretch DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Stretch.Reference;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Stretch_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Stretch_Directory.Include (Key      => Infrastructure_Id,
                                    New_Item => Infrastructure);
         Added := Stretch_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Stretch_Id : in Infra_Id)
      return Boolean
      is
         Answer : Boolean
            := Stretch_Directory.Contains (Key => Stretch_Id);
      begin
         return Answer;
      end Contains_Infrastructure;

      function Find_By_Id (Stretch_Id : in Infra_Id)
                         return Reactive.Infrastructure.Stretch.Reference is
      begin
         if not Stretch_Directory.Contains (Key => Stretch_Id) then
            Raise_Stretch_Missing_Exception (Stretch_Id);
         end if;
         return Stretch_Directory.Element (Key => Stretch_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Stretch_Id : in Infra_Id;
                                Found : out Boolean)
              return Reactive.Infrastructure.Stretch.Reference is
      begin
         Found := TRUE;
         return Find_By_Id (Stretch_Id);
         exception
          when Stretch_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Stretch_Directory.Clear;
      end Clear;

   end Directory;

end Reactive.Directory.Stretch_Directory;
