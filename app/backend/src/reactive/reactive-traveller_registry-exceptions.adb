with Ada.Strings.Unbounded;

package body Reactive.Traveller_Registry.Exceptions is

   package SU renames Ada.Strings.Unbounded;
   use SU;

   procedure Raise_Traveller_Missing_Exception (
      Traveller_Id : in Agent.Agent_Id) is
   begin
      raise Traveller_Missing
        with "The Traveller with id =>"
        & SU.To_String (Traveller_Id)
        & " is not present into the entity registry ";
   end Raise_Traveller_Missing_Exception;

   procedure Raise_Traveller_Already_Existent_Exception (
      Traveller_Id : in Agent.Agent_Id) is
   begin
      raise Traveller_Already_Existent
        with "The traveller with id =>"
        & SU.To_String (Traveller_Id)
        & " is already present into entity registry";
   end Raise_Traveller_Already_Existent_Exception;

end Reactive.Traveller_Registry.Exceptions;
