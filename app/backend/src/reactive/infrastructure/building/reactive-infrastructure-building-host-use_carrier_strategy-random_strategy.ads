package Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy is

   type Object is new Use_Carrier_Strategy.Object
   with null record;
   type Reference is access all Object'Class;

   overriding
   function Use_Carrier_Or_Not (This : in out Random_Strategy.Object)
   return Boolean;

end Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy;
