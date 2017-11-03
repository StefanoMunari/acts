package Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy is

   type Object is interface;
   type Reference is access all Object'Class;

   not overriding
   function Use_Carrier_Or_Not (This : in out Use_Carrier_Strategy.Object)
   return Boolean is abstract;

end Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy;
