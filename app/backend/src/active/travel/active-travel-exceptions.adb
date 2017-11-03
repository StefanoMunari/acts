package body Active.Travel.Exceptions is

   Missing_Element : exception;

   procedure Raise_Missing_Route_Destination (Traveller_Id : Agent.Agent_Id) is
   begin
      raise Missing_Element
        with ("No Destination Route for Traveller "
              & SU.To_String(Traveller_Id));
   end Raise_Missing_Route_Destination;

   procedure Raise_Missing_Route_Source (Traveller_Id : Agent.Agent_Id) is
   begin
      raise Missing_Element
        with ("No Source Route for Traveller "
              & SU.To_String(Traveller_Id));
   end Raise_Missing_Route_Source;

end Active.Travel.Exceptions;