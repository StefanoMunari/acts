package body Scheduling.Build.Exceptions is

   procedure Raise_Missing_Field_For_Scheduling (Field_Name : in String) is
   begin
      raise Missing_Field_For_Scheduling
        with "Missing set for field '" & Field_Name & "' of Scheduling";
   end Raise_Missing_Field_For_Scheduling;

end Scheduling.Build.Exceptions;