package body Scheduling.Remote.Callback.Failure.Tread is

   function Create (
      Traveller       : in     Active.Agent.Agent_Id;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null)
   return Failure.Reference
   is
      Instance : Failure.Tread.Reference := new Failure.Tread.Object;
   begin
      Instance.Traveller := Traveller;

      Instance.Traveller_Utils := Traveller_Utils;
      if Traveller_Utils = null then
         Instance.Traveller_Utils := Traveller_Utils_Pkg.Get_Instance;
      end if;

      return Failure.Reference (Instance);
   end Create;

   procedure Execute (This : in Failure.Tread.Object)
   is
   begin
      This.Traveller_Utils.Defer (This.Traveller, Retry_Action => TRUE);
   end Execute;

end Scheduling.Remote.Callback.Failure.Tread;
