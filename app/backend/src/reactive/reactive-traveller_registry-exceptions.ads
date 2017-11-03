package Reactive.Traveller_Registry.Exceptions is

   use Agent;

   Traveller_Missing : exception;
   Traveller_Already_Existent : exception;

   procedure Raise_Traveller_Missing_Exception (
      Traveller_Id : in Agent.Agent_Id);

   procedure Raise_Traveller_Already_Existent_Exception (
      Traveller_Id : in Agent.Agent_Id);

end Reactive.Traveller_Registry.Exceptions;
