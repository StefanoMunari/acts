with Active.Agent;

limited with Reactive.District;

with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Stretch.Utils is

   package Agent renames Active.Agent;
   package Infra_Id_Set renames Shared.Infra_Id_Set;
   use Reactive.Infra_Id_Type;

   type Object (<>) is tagged limited private;
   type Reference is access all Stretch.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Stretch.Utils.Reference;

   not overriding
   procedure Tread (This         : in     Stretch.Utils.Object;
                    Stretch_Id   : in     Infra_Id;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   not overriding
   function Get_Id (This       : in Stretch.Utils.Object;
                    Stretch_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Find_Lane (This       : in Stretch.Utils.Object;
                       Stretch_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Calculate_Position (This       : in Stretch.Utils.Object;
                                Stretch_Id : in Infra_Id)
   return Natural;

   not overriding
   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Utils.Object;
      Stretch_Id   : in Infra_Id;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   not overriding
   procedure Leave (
      This         : in     Stretch.Utils.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   not overriding
   function Is_Before (
      This  : in Stretch.Utils.Object;
      Left  : in Infra_Id;
      Right : in Infra_Id)
   return Boolean;

   not overriding
   function Find_Intersections (This : in Stretch.Utils.Object;
                                Stretch_Id : in Infra_Id)
   return Infra_Id_Set.Set;

   not overriding
   function Get_Host (This       : in Stretch.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Has_Host (This       : in Stretch.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Get_Size_Of (This       : in Stretch.Utils.Object;
                         Stretch_Id : in Infra_Id) return Natural;

   not overriding
   function Attempt_Overtake (
      This                : in Stretch.Utils.Object;
      Stretch_Id          : in Infra_Id;
      Traveller_Id        : in Agent.Agent_Id)
   return Boolean;

   not overriding
   procedure Unbook (
      This                : in Stretch.Utils.Object;
      Stretch_Id          : in Infra_Id;
      Traveller_Id        : in Agent.Agent_Id);

   not overriding
   function Dump (This : Stretch.Utils.Object; Stretch_Id : in Infra_Id)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Stretch.Utils.Reference := null;

end Reactive.Infrastructure.Stretch.Utils;
