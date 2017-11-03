with Reactive.Host_Registry.Exceptions;
use Reactive.Host_Registry.Exceptions;

package body Reactive.Host_Registry is

   function Get_Instance return Host_Registry.Reference is
   begin
      if Instance = null then
         Instance := new Host_Registry.Object;
      end if;
      return Instance;
   end Get_Instance;

   function Contains_Host (
      This    : in Host_Registry.Object;
      Host_Id : in Infra_Id)
   return Boolean
   is (This.Population.Contains_Host (Host_Id));

   function Find_Host_By_Id (
      This    : in Host_Registry.Object;
      Host_Id : in Infra_Id)
   return Host_Pkg.Reference
   is
      Host_Ref : Host_Pkg.Reference;
   begin
      Host_Ref := This.Population.Find_Host_By_Id (Host_Id);
      return Host_Ref;
   end;

   procedure Add_Host (
      This  :         in out Host_Registry.Object;
      Host  : aliased in out Host_Pkg.Object'Class;
      Added :            out Boolean) is
   begin
      This.Population.Add_Host (Host => Host,
                                Added     => Added);
   end Add_Host;

   procedure Remove_Host (
      This    : in out Host_Registry.Object;
      Host_Id : in     Infra_Id;
      Removed :    out Boolean) is
   begin
      This.Population.Remove_Host (
         Host_Id => Host_Id,
         Removed => Removed);
   end Remove_Host;

   function Dump (This : in Host_Registry.Object)
   return G_JSON.JSON_Value is
   begin
      return This.Population.Dump;
   end Dump;

   -----------------
   --- POPULATION
   -----------------

   protected body Population is

      function Contains_Host (Host_Id : in Infra_Id)
      return Boolean is
      begin
         return Population.Contains (Key => Host_Id);
      end Contains_Host;

      function Find_Host_By_Id (Host_Id : in Infra_Id)
      return Host_Pkg.Reference is
         use Host_By_Id;
         Host_Ref : Host_Pkg.Reference;
      begin
         if not Population.Contains (Key => Host_Id) then
            Raise_Host_Missing_Exception (Host_Id);
         end if;
         Host_Ref := Population.Element (Key => Host_Id);
         return Host_Ref;
      end Find_Host_By_Id;

      procedure Add_Host (
         Host  : aliased in out Host_Pkg.Object'Class;
         Added :            out Boolean)
      is
         Host_Id : Infra_Id := Host.Get_Id;
      begin
         if Population.Contains (Key => Host_Id) then
            Raise_Host_Already_Existent_Exception (Host_Id);
         end if;

         Added := FALSE;

         Population.Include (Key      => Host_Id,
                             New_Item => Host'Unchecked_Access);
         Added := Population.Contains (Key => Host_Id);
      end Add_Host;

      procedure Remove_Host (
         Host_Id : in     Infra_Id;
         Removed :    out Boolean) is
      begin
         Removed := FALSE;

         if not Population.Contains (Key => Host_Id) then
            Raise_Host_Missing_Exception (
               Host_Id => Host_Id);
         end if;

         Population.Delete (Key => Host_Id);
         Removed := Population.Contains (Key => Host_Id);
      end Remove_Host;

      procedure Clear is
      begin
         Population.Clear;
      end Clear;

      function Dump
      return G_JSON.JSON_Value
      is
         Hosts    : G_JSON.JSON_Array := G_JSON.Empty_Array;
         Host_Ref : Host_Pkg.Reference;
      begin
         for Host_Entry in Population.Iterate loop
            Host_Ref := Host_By_Id.Element (Host_Entry);
            G_JSON.Append (Hosts, Host_Ref.Dump);
         end loop;
         return G_JSON.Create (Hosts);
      end Dump;

   end Population;

end Reactive.Host_Registry;
