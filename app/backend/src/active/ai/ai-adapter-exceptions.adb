package body AI.Adapter.Exceptions is

   AI_Adapter_Exception : exception;

   procedure Raise_Init_Exception is
   begin
      raise AI_Adapter_Exception
        with ("Init failed");
   end Raise_Init_Exception;

   procedure Raise_Find_Exception is
   begin
      raise AI_Adapter_Exception
        with ("Find failed");
   end Raise_Find_Exception;

   procedure Raise_Limit_Exception is
   begin
      raise AI_Adapter_Exception
        with ("Set_Clients_Limit failed");
   end Raise_Limit_Exception;

   procedure Raise_Finalize_Exception is
   begin
      raise AI_Adapter_Exception
        with ("Finalize failed");
   end Raise_Finalize_Exception;

end AI.Adapter.Exceptions;
