package body Shared.JSON_Array_Utils is

   function Equals (A,B : JSON_Array_Ref) return Boolean
   is begin
      return A = B;
   end;
   
end Shared.JSON_Array_Utils;