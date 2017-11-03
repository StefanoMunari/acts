with Active.Agent;

with Passive.Road_Sign;

with Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;

package Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator is

   package Agent renames Active.Agent;

   type Object is
     new Stretch_Decorator.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   function Create (Stretch_Ref : Reactive.Infrastructure.Stretch.Reference;
                    Sign_Ref    : Passive.Road_Sign.Reference)
   return Stretch_Sign_Decorator.Reference;

   overriding
   procedure Tread (
      This         : in out Stretch_Sign_Decorator.Object;
      Traveller_Id : in Agent.Agent_Id;
      Advanced     : out Boolean);

   not overriding
   function Get_Sign (This : Stretch_Sign_Decorator.Object)
   return Passive.Road_Sign.Reference;

   overriding
   function Dump (This : Stretch_Sign_Decorator.Object)
   return G_JSON.JSON_Value;

private

   type Object is
      new Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator.Object
      with record
         Sign_Ref : Passive.Road_Sign.Reference;
   end record;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;
