with Active.Agent;

with Passive.Road_Sign;

with Reactive.Infrastructure.Lane.Decoration.Lane_Decorator;

package Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator is

   package Agent renames Active.Agent;

   type Object is
     new Reactive.Infrastructure.Lane.Decoration.Lane_Decorator.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   function Create (Lane_Ref : Reactive.Infrastructure.Lane.Reference;
                    Sign_Ref : Passive.Road_Sign.Reference)
   return Lane_Sign_Decorator.Reference;

   overriding
   procedure Enter (
      This         : in out Lane_Sign_Decorator.Object;
      Traveller_Id : in     Agent.Agent_Id);

   overriding
   function Dump (This : Lane_Sign_Decorator.Object)
   return G_JSON.JSON_Value;

private

   type Object is
      new Reactive.Infrastructure.Lane.Decoration.Lane_Decorator.Object
      with record
         Sign_Ref : Passive.Road_Sign.Reference;
   end record;

end Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;
