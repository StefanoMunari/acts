with Active.Agent;

with Shared.Infra_Id_List;

package Reactive.Infrastructure.Building.Host.Mock is

   package Agent renames Active.Agent;
   package Infra_Id_List renames Shared.Infra_Id_List;
   use Active.Space_Master.Next_Action_Type;

   type Object (<>) is new Building.Host.Object with private;
   type Reference is access all Building.Host.Mock.Object'Class;

   function Create return Building.Host.Mock.Reference;

   overriding
   function Get_Id (This : in Host.Mock.Object) return Infra_Id;

   overriding
   function Get_Parking (This       : in out Host.Mock.Object)
   return Parking_Manager.Reference;

   overriding
   procedure Stop_Over (This       : in out Host.Mock.Object;
                        Travellers : in     Agent_Id_List.List);

   overriding
   function Exit_Building (This         : in out Host.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Next_Action;

   overriding
   procedure Put_Stopping_Traveller (This         : in out Host.Mock.Object;
                                     Traveller_Id : in     Agent.Agent_Id);

   procedure Accessible_By (
      This       : in out Host.Mock.Object;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type);

   overriding
   function Dump (This    : Host.Mock.Object)
   return G_JSON.JSON_Value;

   not overriding
   procedure Set_Return_Value_For_Get_Id (
      This         : in out Host.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Parking (
      This         : in out Host.Mock.Object;
      Return_Value : in     Parking_Manager.Reference);

   not overriding
   procedure Set_Return_Values_For_Exit_Building (
      This    : in out Host.Mock.Object;
      Vehicle : in     Agent.Agent_Id;
      Action  : in     Next_Action);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Host.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

   not overriding
   function Get_Stop_Over_Called (
      This : in out Building.Host.Mock.Object) return Boolean;

   not overriding
   function Get_Exit_Building_Called (
      This : in out Building.Host.Mock.Object) return Boolean;

   not overriding
   function Get_Put_Stopping_Traveller_Called (
      This : in out Building.Host.Mock.Object) return Boolean;

   not overriding
   function Get_Accessible_By_Called (
      This       : in out Building.Host.Mock.Object;
      Stretch_Id : in     Infra_Id) return Boolean;

private
   type Return_Values_Collection is record
      Id                      : Infra_Id;
      Id_Existence            : Boolean := FALSE;
      Get_Parking             : Parking_Manager.Reference;
      Get_Parking_Existence   : Boolean := FALSE;
      Exit_Building_Action    : Next_Action;
      Exit_Building_Vehicle   : Agent.Agent_Id;
      Exit_Building_Existence : Boolean := FALSE;
      Dump                    : G_JSON.JSON_Value;
      Dump_Existence          : Boolean := False;
   end record;

   type Mock_Values_Collection is record
      Stop_Over_Called              : Boolean := False;
      Exit_Building_Called          : Boolean := False;
      Put_Stopping_Traveller_Called : Boolean := False;
      Accessible_By_Called          : Infra_Id_List.List :=
         Infra_Id_List.Empty_List;
   end record;

   type Object is new Building.Host.Object with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

end Reactive.Infrastructure.Building.Host.Mock;
