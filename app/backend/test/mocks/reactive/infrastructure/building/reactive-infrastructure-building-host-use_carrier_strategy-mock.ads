package Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock is

   type Object is new Use_Carrier_Strategy.Object
   with private;
   type Reference is access all Object'Class;

   function Create return Use_Carrier_Strategy.Reference;

   overriding
   function Use_Carrier_Or_Not (This : in out Mock.Object)
   return Boolean;

   not overriding
   procedure Set_Return_Value_For_Use_Carrier_Or_Not
     (This         : in out Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Use_Carrier_Or_Not           : Boolean;
      Use_Carrier_Or_Not_Existence : Boolean := FALSE;
   end record;

   type Object is new Use_Carrier_Strategy.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock;
