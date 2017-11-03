package body Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator is

   function Create (Lane_Ref : Reactive.Infrastructure.Lane.Reference;
                    Sign_Ref : Passive.Road_Sign.Reference)
   return Lane_Sign_Decorator.Reference is
      Decorated_Lane :
         Infrastructure.Lane.Decoration.Lane_Sign_Decorator.Reference
            := new Infrastructure.Lane.Decoration.Lane_Sign_Decorator.Object;
   begin
      Decorated_Lane.Init (Lane_Ref);
      Decorated_Lane.Sign_Ref := Sign_Ref;
      return Decorated_Lane;
   end Create;

   procedure Enter (
      This         : in out Lane_Sign_Decorator.Object;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
   -- Stretch will call Lane's Enter each time it is trod

   -- call signal's apply before entering the lane
   -- ^ this enforces the application of the street rules for this road
   -- segment
      This.Sign_Ref.Apply (Traveller_Id);
      This.Get_Lane_Ref.Enter (Traveller_Id);
   end Enter;

   function Dump (This : Lane_Sign_Decorator.Object) return G_JSON.JSON_Value
   is
      JSON        : G_JSON.JSON_Value;
      Decorations : G_JSON.JSON_Value;
      Decoration  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := This.Get_Lane_Ref.Dump;

      Decoration := This.Sign_Ref.Dump;

      JSON.Set_Field (Decorations_Field, Decoration);

      return JSON;
   end Dump;

end Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;
