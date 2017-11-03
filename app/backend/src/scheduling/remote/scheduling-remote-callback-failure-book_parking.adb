package body Scheduling.Remote.Callback.Failure.Book_Parking is

   function Create (
      Traveller        : in     Active.Agent.Agent_Id;
      Vehicle          : in     Active.Agent.Agent_Id;
      Garage           : access Garage_Pkg.Object'Class;
      Traveller_Utils  : access Traveller_Utils_Pkg.Object'Class := null)
   return Failure.Reference
   is
      Instance : Failure.Book_Parking.Reference
         := new Failure.Book_Parking.Object;
   begin
     Instance.Traveller := Traveller;
     Instance.Vehicle := Vehicle;
     Instance.Garage := Garage;

     Instance.Traveller_Utils := Traveller_Utils;
     if Traveller_Utils = null then
       Instance.Traveller_Utils := Traveller_Utils_Pkg.Get_Instance;
     end if;

     return Failure.Reference (Instance);
   end Create;

   procedure Execute (This : in Failure.Book_Parking.Object)
   is
   begin
   -- If booking went bad, unboard vehicle and repark it
      This.Garage.Unboard_Vehicle (This.Vehicle, This.Traveller);

   -- schedule new action for next tick
     This.Traveller_Utils.Defer (This.Traveller, Retry_Action => True);
   end Execute;

end Scheduling.Remote.Callback.Failure.Book_Parking;
