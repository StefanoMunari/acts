package body Reactive.Host_Registry.Exceptions is

   procedure Raise_Host_Missing_Exception (
      Host_Id : in Infra_Id) is
   begin
      raise Host_Missing
        with "The Host with id =>"
        & Infra_Id'Image (Host_Id)
        & " is not present into the entity registry ";
   end Raise_Host_Missing_Exception;

   procedure Raise_Host_Already_Existent_Exception (
      Host_Id : in Infra_Id) is
   begin
      raise Host_Already_Existent
        with "The Host with id =>"
        & Infra_Id'Image (Host_Id)
        & " is already present into entity registry";
   end Raise_Host_Already_Existent_Exception;

end Reactive.Host_Registry.Exceptions;
