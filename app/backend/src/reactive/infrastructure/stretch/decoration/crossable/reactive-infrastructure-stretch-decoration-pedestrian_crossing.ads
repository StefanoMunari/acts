with Active.Agent;
with Active.Traveller.Pedestrian.Utils;

with Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;

package Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing is

   package Agent renames Active.Agent;
   package P_Utils renames Active.Traveller.Pedestrian.Utils;
   package Zebra_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;

   type Object is
      new Zebra_Crossing.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   function Create (
      Stretch_Ref      : Reactive.Infrastructure.Stretch.Reference;
      Pedestrian_Utils : access P_Utils.Object'Class := null)
   return Pedestrian_Crossing.Reference;

   overriding
   function Has_Priority_Over (
      This         : in out Pedestrian_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean;

   overriding
   function Dump (This : Pedestrian_Crossing.Object) return G_JSON.JSON_Value;

private
   type Object is
      new Zebra_Crossing.Object
   with record
      Pedestrian_Utils : access P_Utils.Object'Class;
   end record;

end Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
