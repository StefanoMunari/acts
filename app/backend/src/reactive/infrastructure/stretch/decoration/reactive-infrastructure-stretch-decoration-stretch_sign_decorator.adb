package body Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator is

   function Create (Stretch_Ref : Reactive.Infrastructure.Stretch.Reference;
                    Sign_Ref    : Passive.Road_Sign.Reference)
   return Stretch_Sign_Decorator.Reference is
      Decorated_Stretch
      : Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Reference
      := new Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Object;
   begin
      Decorated_Stretch.Init (Stretch_Ref);
      Decorated_Stretch.Sign_Ref := Sign_Ref;
      return Decorated_Stretch;
   end Create;

   procedure Tread (
      This         : in out Stretch_Sign_Decorator.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is
   begin
      This.Get_Stretch_Ref.Tread (Traveller_Id, Advanced);
      if Advanced then
         -- call signal's apply *after* having entered the Stretch
         This.Sign_Ref.Apply (Traveller_Id);
      end if;
   end Tread;

   function Get_Sign (This : Stretch_Sign_Decorator.Object)
   return Passive.Road_Sign.Reference
   is (This.Sign_Ref);

   function Dump (This : Stretch_Sign_Decorator.Object)
   return G_JSON.JSON_Value is
      JSON        : G_JSON.JSON_Value;
      Decorations : G_JSON.JSON_Value;
      Decoration  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := This.Get_Stretch_Ref.Dump;

      Decoration := This.Sign_Ref.Dump;

      Decorations := JSON.Get (Decorations_Field);
      Decorations.Set_Field ("busStop", Decoration);

      JSON.Set_Field (Decorations_Field, Decorations);

      return JSON;
   end Dump;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;
