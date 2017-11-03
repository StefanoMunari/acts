with Ada.Strings.Unbounded;

package body Reactive.Traffic_Light_Registry.Exceptions is

   package SU renames Ada.Strings.Unbounded;
   use SU;

   procedure Raise_Traffic_Light_Missing_Exception (
      Traffic_Light_Id : in Agent_Id) is
   begin
      raise Traffic_Light_Missing
        with "The Traffic_Light with id =>"
        & SU.To_String (Traffic_Light_Id)
        & " is not present into the entity registry ";
   end Raise_Traffic_Light_Missing_Exception;

   procedure Raise_Traffic_Light_Already_Existent_Exception (
      Traffic_Light_Id : in Agent_Id) is
   begin
      raise Traffic_Light_Already_Existent
        with "The Traffic_Light with id =>"
        & SU.To_String (Traffic_Light_Id)
        & " is already present into entity registry";
   end Raise_Traffic_Light_Already_Existent_Exception;

end Reactive.Traffic_Light_Registry.Exceptions;
