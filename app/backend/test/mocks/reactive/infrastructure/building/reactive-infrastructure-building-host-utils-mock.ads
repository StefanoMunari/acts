with Active.Agent;

with Shared.Agent_Id_List;

package Reactive.Infrastructure.Building.Host.Utils.Mock is

   package Agent renames Active.Agent;
   package Agent_Id_List renames Shared.Agent_Id_List;

   type Object (<>) is new Host.Utils.Object with private;
   type Reference is access all Host.Utils.Mock.Object'Class;

   function Create return Host.Utils.Mock.Reference;

   overriding
   function Stop_Over (This         : in out Host.Utils.Mock.Object;
                       Host_Id      : in     Infra_Id;
                       Traveller_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

-- Beware of the "is null" if implementing this one
   overriding
   procedure Accessible_By (
      This       : in out Host.Utils.Mock.Object;
      Host_Id    : in     Infra_Id;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type) is null;

   overriding
   function Dump (
      This    : Host.Utils.Mock.Object;
      Host_Id : Infra_Id)
   return G_JSON.JSON_Value;

   not overriding
   procedure Set_Travellers_List_For_Stop_Over (
      This   : in out Host.Utils.Mock.Object;
      Agents : in     Agent_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Host.Utils.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

private
   type Return_Values_Collection is record
      Stop_Over : Agent_Id_List.List;
      Stop_Over_Existence : Boolean := False;
      Dump      : G_JSON.JSON_Value;
      Dump_Existence      : Boolean := False;
   end record;

   type Object is new Host.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Building.Host.Utils.Mock;
