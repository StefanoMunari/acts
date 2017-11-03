with Reactive.Traffic_Light_Registry.Exceptions;

use Reactive.Traffic_Light_Registry.Exceptions;

package body Reactive.Traffic_Light_Registry is

   function Get_Instance return Traffic_Light_Registry.Reference is
   begin
      if Instance = null then
         Instance := new Traffic_Light_Registry.Object;
      end if;
      return Instance;
   end Get_Instance;

   function Contains_Traffic_Light (
      This             : in Traffic_Light_Registry.Object;
      Traffic_Light_Id : in Agent.Agent_Id)
   return Boolean
   is (This.Population.Contains_Traffic_Light (Traffic_Light_Id));

   function Find_Traffic_Light_By_Id (
      This             : in Traffic_Light_Registry.Object;
      Traffic_Light_Id : in Agent.Agent_Id)
   return Traffic_Light.Reference
   is (This.Population.Find_Traffic_Light_By_Id (Traffic_Light_Id));

   procedure Add_Traffic_Light (
      This          :         in out Traffic_Light_Registry.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
      Added         :            out Boolean) is
   begin
      This.Population.Add_Traffic_Light (Traffic_Light => Traffic_Light,
                                         Added     => Added);
   end Add_Traffic_Light;

   procedure Remove_Traffic_Light (
      This             : in out Traffic_Light_Registry.Object;
      Traffic_Light_Id : in     Agent.Agent_Id;
      Removed          :    out Boolean) is
   begin
      This.Population.Remove_Traffic_Light (
         Traffic_Light_Id => Traffic_Light_Id,
         Removed          => Removed);
   end Remove_Traffic_Light;

   function Dump (This : in Traffic_Light_Registry.Object)
   return G_JSON.JSON_Value is
   begin
      return This.Population.Dump;
   end Dump;

   protected body Population is

      function Contains_Traffic_Light (Traffic_Light_Id : in Agent.Agent_Id)
      return Boolean is
      begin
         return Population.Contains (Key => Traffic_Light_Id);
      end Contains_Traffic_Light;

      function Find_Traffic_Light_By_Id (Traffic_Light_Id : in Agent.Agent_Id)
      return Traffic_Light.Reference is
      begin
         if not Population.Contains (Key => Traffic_Light_Id) then
            Raise_Traffic_Light_Missing_Exception (Traffic_Light_Id);
         end if;

         return Population.Element (Key => Traffic_Light_Id);
      end Find_Traffic_Light_By_Id;

      procedure Add_Traffic_Light (
         Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
         Added         :            out Boolean)
      is
         Traffic_Light_Id : Agent.Agent_Id := Traffic_Light.Get_Id;
      begin
         if Population.Contains (Key => Traffic_Light_Id) then
            Raise_Traffic_Light_Already_Existent_Exception (Traffic_Light_Id);
         end if;

         Added := FALSE;

         Population.Include (Key      => Traffic_Light_Id,
                             New_Item => Traffic_Light'Unchecked_Access);
         Added := Population.Contains (Key => Traffic_Light_Id);
      end Add_Traffic_Light;

      procedure Remove_Traffic_Light (
         Traffic_Light_Id : in     Agent.Agent_Id;
         Removed          :    out Boolean) is
      begin
         Removed := FALSE;

         if not Population.Contains (Key => Traffic_Light_Id) then
            Raise_Traffic_Light_Missing_Exception (
               Traffic_Light_Id => Traffic_Light_Id);
         end if;

         Population.Delete (Key => Traffic_Light_Id);
         Removed := Population.Contains (Key => Traffic_Light_Id);
      end Remove_Traffic_Light;

      procedure Clear is
      begin
         Population.Clear;
      end Clear;

     function Dump
     return G_JSON.JSON_Value
     is
         Traffic_Lights    : G_JSON.JSON_Array := G_JSON.Empty_Array;
         Traffic_Light_Ref : Active.Traffic_Light.Reference;
     begin
        for Traffic_Light_Entry in Population.Iterate loop
            Traffic_Light_Ref := Traffic_Light_By_Id.Element (Traffic_Light_Entry);
            G_JSON.Append (Traffic_Lights, Traffic_Light_Ref.Dump);
         end loop;
         return G_JSON.Create (Traffic_Lights);
     end Dump;

   end Population;

end Reactive.Traffic_Light_Registry;
