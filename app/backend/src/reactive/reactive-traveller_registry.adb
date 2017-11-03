with Reactive.Traveller_Registry.Exceptions;

use Reactive.Traveller_Registry.Exceptions;

package body Reactive.Traveller_Registry is

   function Get_Instance return Traveller_Registry.Reference is
   begin
      if Instance = null then
         Instance := new Traveller_Registry.Object;
      end if;
      return Instance;
   end Get_Instance;

   function Contains_Traveller (This         : in Traveller_Registry.Object;
                                Traveller_Id : in Agent.Agent_Id)
   return Boolean
   is (This.Population.Contains_Traveller (Traveller_Id));

   function Find_Traveller_By_Id (This         : in Traveller_Registry.Object;
                                  Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference
   is (This.Population.Find_Traveller_By_Id (Traveller_Id));

   procedure Add_Traveller (
      This      : in out Traveller_Registry.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean) is
   begin
      This.Population.Add_Traveller (Traveller => Traveller,
                                     Added     => Added);
   end Add_Traveller;

   procedure Remove_Traveller (This : in out Traveller_Registry.Object;
                               Traveller_Id : in Agent.Agent_Id;
                               Removed      : out Boolean) is
   begin
      This.Population.Remove_Traveller (Traveller_Id => Traveller_Id,
                                        Removed      => Removed);
   end Remove_Traveller;

   function Dump (This : in Traveller_Registry.Object)
   return G_JSON.JSON_Value is
   begin
      return This.Population.Dump;
   end Dump;

   protected body Population is

      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean is
      begin
         return Population.Contains (Key => Traveller_Id);
      end Contains_Traveller;

      function Find_Traveller_By_Id (Traveller_Id : in Agent.Agent_Id)
      return Traveller.Reference is
      begin
         if not Population.Contains (Key => Traveller_Id) then
            Raise_Traveller_Missing_Exception (Traveller_Id);
         end if;

         return Population.Element (Key => Traveller_Id);
      end Find_Traveller_By_Id;

      procedure Add_Traveller (
         Traveller : in out Active.Traveller.Reference;
         Added     :    out Boolean)
      is
         Traveller_Id : Agent.Agent_Id := Traveller.Get_Id;
      begin
         if Population.Contains (Key => Traveller_Id) then
            Raise_Traveller_Already_Existent_Exception (Traveller_Id);
         end if;

         Added := FALSE;

         Population.Include (Key      => Traveller_Id,
                             New_Item => Traveller);
         Added := Population.Contains (Key => Traveller_Id);
      end Add_Traveller;

      procedure Remove_Traveller (
         Traveller_Id : in     Agent.Agent_Id;
         Removed      :    out Boolean) is
      begin
         Removed := FALSE;

         if not Population.Contains (Key => Traveller_Id) then
            Raise_Traveller_Missing_Exception (Traveller_Id => Traveller_Id);
         end if;

         Population.Delete (Key => Traveller_Id);
         Removed := Population.Contains (Key => Traveller_Id);
      end Remove_Traveller;

      procedure Clear is
      begin
         Population.Clear;
      end Clear;

     function Dump
     return G_JSON.JSON_Value
     is
         Travellers    : G_JSON.JSON_Array := G_JSON.Empty_Array;
         Traveller_Ref : Active.Traveller.Reference;
     begin
        for Traveller_Entry in Population.Iterate loop
            Traveller_Ref := Traveller_By_Id.Element (Traveller_Entry);
            G_JSON.Append (Travellers, Traveller_Ref.Dump);
         end loop;
         return G_JSON.Create (Travellers);
     end Dump;

   end Population;

end Reactive.Traveller_Registry;
