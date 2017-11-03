package body Shared.String_Util is

   function Strip_Borders (Input : in SU.Unbounded_String)
      return String is
   begin 
      return SU.To_String (SU.Delete (SU.Delete (Input, 1,1), 
               Positive(SU.Length(Input)-1), 
               SU.Length(Input)-1));
   end Strip_Borders;

end Shared.String_Util;
