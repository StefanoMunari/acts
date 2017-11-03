with Shared.Random;

package body Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy is

   package Random renames Shared.Random;

   overriding
   function Use_Carrier_Or_Not (This : in out Random_Strategy.Object)
   return Boolean is
      Decision : Natural;
   begin
      Random.Generate_Random (1, 2, Decision);
      return Decision = 2;
   end Use_Carrier_Or_Not;

end Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy;
