package Reactive.Host_Registry.Exceptions is

   Host_Missing : exception;
   Host_Already_Existent : exception;

   procedure Raise_Host_Missing_Exception (
      Host_Id : in Infra_Id);

   procedure Raise_Host_Already_Existent_Exception (
      Host_Id : in Infra_Id);

end Reactive.Host_Registry.Exceptions;
