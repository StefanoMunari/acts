with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Directory.Street_Directory is

  ----------------------------------------------------------------------------
   -- Street DIRECTORY
   protected body Directory is

      procedure Add (
         SR_Street :     SR_Street_Pkg.Shared_Reference;
         Added          : out Boolean)
      is
         Street_Id  : Infra_Id := SR_Street.Get.Get_Id;
         Street_Acc : access Street_Pkg.Object'Class;
         Street_Ref : Street_Pkg.Reference;
      begin
         if Street_Directory.Contains (Key => Street_Id) then
            Raise_Infrastructure_Already_Existent_Exception (Street_Id);
         end if;
         Added := FALSE;
         Street_Acc := Street_Pkg.Reference (SR_Street.Get_Reference);
         Street_Ref := Street_Pkg.Reference (Street_Acc);
         Street_Directory.Include (Key      => Street_Id,
                                   New_Item => Street_Ref);
         Added := Street_Directory.Contains (Key => Street_Id);
      end Add;

      function Contains_Infrastructure (Infrastructure_Id : in Infra_Id)
                                        return Boolean is
      begin
         return Street_Directory.Contains (Key => Infrastructure_Id);
      end Contains_Infrastructure;

      function Find_By_Id (Street_Id : in Infra_Id)
                         return Reactive.Infrastructure.Street.Reference is
      begin
         if not Street_Directory.Contains (Key => Street_Id) then
            Raise_Street_Missing_Exception (Street_Id);
         end if;
         return Street_Directory.Element (Key => Street_Id);
      end Find_By_Id;

      function Safe_Find_By_Id (Street_Id : in Infra_Id;
                                Found : out Boolean)
              return Reactive.Infrastructure.Street.Reference is
      begin
         Found := TRUE;
         return Find_By_Id(Street_Id);
         exception
          when Street_Missing =>
          Found := FALSE;
          return NULL;
      end Safe_Find_By_Id;

      procedure Clear is
      begin
        Street_Directory.Clear;
      end Clear;

      function Dump return G_JSON.JSON_Value
      is
         Streets    : G_JSON.JSON_Array := G_JSON.Empty_Array;
         Street_Ref : Street_Pkg.Reference;
      begin -- Dump
         for Street_Entry in Street_Directory.Iterate loop
            Street_Ref := Street_By_Id.Element (Street_Entry);
            G_JSON.Append (Streets, Street_Ref.Dump);
         end loop;
         return G_JSON.Create (Streets);
      end Dump;

   end Directory;

end Reactive.Directory.Street_Directory;
