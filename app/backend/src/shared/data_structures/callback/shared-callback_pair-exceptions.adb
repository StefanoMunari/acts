package body Shared.Callback_Pair.Exceptions is

   Missing_Field : exception;

   procedure Raise_Missing_Field (Field_Name : String) is
   begin
      raise Missing_Field
        with "The missing field is =>" & Field_Name;
   end Raise_Missing_Field;

end Shared.Callback_Pair.Exceptions;