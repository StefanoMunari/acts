with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Active.Agent;
with Active.Traveller;

package Reactive.Traveller_Registry is

-- libs
   package G_JSON       renames GNATCOLL.JSON;
-- active
   package Agent renames Active.Agent;
   package Traveller renames Active.Traveller;

   package Traveller_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Agent.Agent_Id,
        Element_Type => Traveller.Reference,
        "<"          => Agent."<",
        "="          => Traveller."=");

   type Object (<>) is tagged limited private;
   type Reference is access all Traveller_Registry.Object'Class;

   function Get_Instance return Traveller_Registry.Reference;

   function Contains_Traveller (This         : in Traveller_Registry.Object;
                                Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   function Find_Traveller_By_Id (This         : in Traveller_Registry.Object;
                                  Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference;

   procedure Add_Traveller (
      This      : in out Traveller_Registry.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean);

   procedure Remove_Traveller (This         : in out Traveller_Registry.Object;
                               Traveller_Id : in     Agent.Agent_Id;
                               Removed      :    out Boolean);

   function Dump (This : in Traveller_Registry.Object)
   return G_JSON.JSON_Value;

   protected type Population is

      not overriding
      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean;

      not overriding
      function Find_Traveller_By_Id (Traveller_Id : in Agent.Agent_Id)
      return Traveller.Reference;

      not overriding
      procedure Add_Traveller
        (Traveller : in out Active.Traveller.Reference;
         Added     : out Boolean);

      not overriding
      procedure Remove_Traveller (Traveller_Id : in Agent.Agent_Id;
                                  Removed      : out Boolean);

      not overriding
      procedure Clear;

     function Dump
     return G_JSON.JSON_Value;

   private
      Population : Traveller_By_Id.Map;
   end Population;

private
   type Object is tagged limited record
      Population : access Traveller_Registry.Population
         := new Traveller_Registry.Population;
   end record;

   Instance : Traveller_Registry.Reference;

end Reactive.Traveller_Registry;
