with Active.Agent;
with Active.Space_Master;

with Reactive.Identifiable;
with Reactive.Infrastructure.Building.Parking_Manager;

with Shared.Agent_Id_List;

package Reactive.Infrastructure.Building.Host is

   package Agent renames Active.Agent;
   package Parking_Manager
      renames Reactive.Infrastructure.Building.Parking_Manager;
   package Agent_Id_List renames Shared.Agent_Id_List;

   use Active.Space_Master.Next_Action_Type;
   use Reactive.Stretch_Type_Package;

   type Object is interface and Reactive.Identifiable.Object;
   type Reference is access all Object'Class;

   not overriding
   function Get_Parking (This       : in out Host.Object)
   return Parking_Manager.Reference
   is abstract;

   not overriding
   procedure Stop_Over (This       : in out Host.Object;
                        Travellers : in     Agent_Id_List.List)
   is abstract;

   not overriding
   function Exit_Building (This         : in out Host.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           Exiting_Id   :    out Agent.Agent_Id)
   return Next_Action
   is abstract;

   not overriding
   procedure Put_Stopping_Traveller (This         : in out Host.Object;
                                     Traveller_Id : in     Agent.Agent_Id)
   is abstract;

   not overriding
   procedure Accessible_By (
      This       : in out Host.Object;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type)
   is abstract;

   not overriding
   function Dump (This : Host.Object) return G_JSON.JSON_Value is abstract;

-- JSON FIELDS CONSTANTS
   function Id_Field     return String is ("id");
   function Guests_Field return String is ("guests");
   function Garage_Field return String is ("garage");

end Reactive.Infrastructure.Building.Host;
