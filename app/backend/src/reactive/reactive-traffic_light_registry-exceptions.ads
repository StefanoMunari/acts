package Reactive.Traffic_Light_Registry.Exceptions is

   use Agent;

   Traffic_Light_Missing : exception;
   Traffic_Light_Already_Existent : exception;

   procedure Raise_Traffic_Light_Missing_Exception (
      Traffic_Light_Id : in Agent_Id);

   procedure Raise_Traffic_Light_Already_Existent_Exception (
      Traffic_Light_Id : in Agent_Id);

end Reactive.Traffic_Light_Registry.Exceptions;
