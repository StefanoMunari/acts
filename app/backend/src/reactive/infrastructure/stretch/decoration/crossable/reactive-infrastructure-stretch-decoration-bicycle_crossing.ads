with Active.Agent;
with Active.Traveller.Vehicle.Bicycle.Utils;

with Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;

package Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing is

   package Agent renames Active.Agent;
   package B_Utils renames Active.Traveller.Vehicle.Bicycle.Utils;
   package Zebra_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;

   type Object is
      new Zebra_Crossing.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   function Create (Stretch_Ref   : Reactive.Infrastructure.Stretch.Reference;
                    Bicycle_Utils : access B_Utils.Object'Class := null)
   return Bicycle_Crossing.Reference;

   overriding
   function Has_Priority_Over (
      This         : in out Bicycle_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean;

   overriding
   function Dump (This : Bicycle_Crossing.Object) return G_JSON.JSON_Value;

private
   type Object is
      new Zebra_Crossing.Object
   with record
      Bicycle_Utils : access B_Utils.Object'Class;
   end record;

end Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
