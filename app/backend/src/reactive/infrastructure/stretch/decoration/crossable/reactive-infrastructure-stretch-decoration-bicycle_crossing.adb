with Active.Agent;

package body Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing is

   not overriding
   function Create (Stretch_Ref   : Reactive.Infrastructure.Stretch.Reference;
                    Bicycle_Utils : access B_Utils.Object'Class := null)
   return Bicycle_Crossing.Reference
   is
      Decorated_Stretch
      : Infrastructure.Stretch.Decoration.Bicycle_Crossing.Reference
      := new Infrastructure.Stretch.Decoration.Bicycle_Crossing.Object;
   begin
      Decorated_Stretch.Init (Stretch_Ref);
      Zebra_Crossing.Init (Zebra_Crossing.Reference (Decorated_Stretch));

      Decorated_Stretch.Bicycle_Utils := Bicycle_Utils;
      if Decorated_Stretch.Bicycle_Utils = null then
         Decorated_Stretch.Bicycle_Utils := B_Utils.Get_Instance;
      end if;

      return Decorated_Stretch;
   end Create;

   function Has_Priority_Over (
      This         : in out Bicycle_Crossing.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean is
   begin
      return This.Bicycle_Utils.Is_A_Bicycle (Traveller_Id);
   end Has_Priority_Over;

   function Dump (This : Bicycle_Crossing.Object) return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value;
      Decoration  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := This.Get_Stretch_Ref.Dump;

      Decoration.Set_Field ("bicycleCrossing", "true");

      JSON.Set_Field (Decorations_Field, Decoration);

      return JSON;
   end Dump;

end Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
