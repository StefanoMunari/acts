with Ada.Containers.Ordered_Maps;

with Active.Agent;

package Reactive.Traveller_Registry.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is new Traveller_Registry.Object with private;
   type Reference is access all Traveller_Registry.Mock.Object'Class;

   package Traveller_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Agent.Agent_Id,
        Element_Type => Traveller.Reference,
        "<"          => Agent."<",
        "="          => Traveller."=");

   function Create return Traveller_Registry.Mock.Reference;

   overriding
   function Find_Traveller_By_Id (This : in Traveller_Registry.Mock.Object;
                                  Traveller_Id : in Agent.Agent_Id)
                                  return Traveller.Reference;

   overriding
   function Contains_Traveller (
      This : in Traveller_Registry.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
      return Boolean;

   overriding
   procedure Add_Traveller (
      This      : in out Traveller_Registry.Mock.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean);

   overriding
   procedure Remove_Traveller (
      This : in out Traveller_Registry.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Removed : out Boolean);

private
   type Mock_Values_Collection is record
      Population : Traveller_By_Id.Map;
   end record;

   type Object is new Traveller_Registry.Object with record
      Mock_Values : Mock_Values_Collection;
   end record;

end Reactive.Traveller_Registry.Mock;
