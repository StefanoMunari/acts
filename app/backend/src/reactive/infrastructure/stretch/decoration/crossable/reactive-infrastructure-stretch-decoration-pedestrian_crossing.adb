with Active.Agent;

package body Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing is

   not overriding
   function Create (
      Stretch_Ref      : Reactive.Infrastructure.Stretch.Reference;
      Pedestrian_Utils : access P_Utils.Object'Class := null)
   return Pedestrian_Crossing.Reference is
      Decorated_Stretch : Pedestrian_Crossing.Reference
         := new Pedestrian_Crossing.Object;
   begin
      Decorated_Stretch.Init (Stretch_Ref);
      Zebra_Crossing.Init (Zebra_Crossing.Reference (Decorated_Stretch));

      Decorated_Stretch.Pedestrian_Utils := Pedestrian_Utils;
      if Decorated_Stretch.Pedestrian_Utils = null then
         Decorated_Stretch.Pedestrian_Utils := P_Utils.Get_Instance;
      end if;

      return Decorated_Stretch;
   end Create;

   function Has_Priority_Over (
      This         : in out Pedestrian_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean is
   begin
      return This.Pedestrian_Utils.Is_A_Pedestrian (Traveller_Id);
   end Has_Priority_Over;

   function Dump (This : Pedestrian_Crossing.Object) return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value;
      Decoration  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := This.Get_Stretch_Ref.Dump;

      Decoration.Set_Field ("pedestrianCrossing", "true");

      JSON.Set_Field (Decorations_Field, Decoration);

      return JSON;
   end Dump;

end Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
