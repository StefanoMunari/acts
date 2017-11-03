-- core
with Ada.Strings.Unbounded;

package body Active.Traveller.Exceptions is

   package SU renames Ada.Strings.Unbounded;

   Null_Travel_Reference : exception;
   Maximum_Speed_Exceeded : exception;
   Unknown_Direction : exception;

   procedure Raise_Null_Travel_Reference
     (Traveller_Id : Agent.Agent_Id) is
   begin
      raise Null_Travel_Reference
        with "The travel of the traveller with id =>"
        & SU.To_String (Traveller_Id);
   end Raise_Null_Travel_Reference;

   procedure Raise_Maximum_Speed_Exceeded_Exception
     (Traveller_Id : Agent.Agent_Id;
      Traveller_Maximum_Speed, New_Speed : Natural) is
   begin
      raise Maximum_Speed_Exceeded
        with "The last speed submitted to the traveller with id =>"
        --& Natural'Image(Traveller_Id)
        --& " is"
        & Natural'Image(New_Speed - Traveller_Maximum_Speed)
        & " Km/h over its maximum speed physical limit";
   end Raise_Maximum_Speed_Exceeded_Exception;

   procedure Raise_Unknown_Direction_Exception
     (Traveller_Id : Agent.Agent_Id) is
   begin
      raise Unknown_Direction
        with "The direction of the traveller with id =>"
        --& Natural'Image(Traveller_Id)
        --& " is unknown";
        ;
   end Raise_Unknown_Direction_Exception;

end Active.Traveller.Exceptions;
