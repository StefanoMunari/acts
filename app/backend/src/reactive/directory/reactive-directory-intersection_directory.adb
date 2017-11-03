with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Intersection_Directory is

  ----------------------------------------------------------------------------
   -- Intersection DIRECTORY
   protected body Directory is

      procedure Add (
         Infrastructure :
            aliased in out Reactive.Infrastructure.Intersection.Object'Class;
         Added          : out Boolean)
      is
         Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
      begin
         if Intersection_Directory.Contains (Key => Infrastructure_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Infrastructure_Id);
         end if;
         Added := FALSE;
         Intersection_Directory.Include (
            Key      => Infrastructure_Id,
            New_Item => Infrastructure'Unchecked_Access);
         Added := Intersection_Directory.Contains (Key => Infrastructure_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
      return Boolean is
      begin
         return Intersection_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Intersection_Id : in Infra_Id)
      return Reactive.Infrastructure.Intersection.Reference is
      begin
         if not Intersection_Directory.Contains (Key => Intersection_Id) then
            Raise_Intersection_Missing_Exception (Intersection_Id);
         end if;
         return Intersection_Directory.Element (Key => Intersection_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Intersection_Id : in     Infra_Id;
                                Found           :    out Boolean)
      return Reactive.Infrastructure.Intersection.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Intersection_Id);
         exception
          when Intersection_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Intersection_Directory.Clear;
      end Clear;

      function Dump return G_JSON.JSON_Value
      is
         JSON             : G_JSON.JSON_Array;
         Intersection_Ref : Intersection_Pkg.Reference;
      begin -- Dump
         for Intersection_Entry in Intersection_Directory.Iterate loop
            Intersection_Ref := Intersection_By_Id.Element (Intersection_Entry);
            G_JSON.Append (JSON, Intersection_Ref.Dump);
         end loop;
         return G_JSON.Create (JSON);
      end Dump;

   end Directory;

end Reactive.Directory.Intersection_Directory;
