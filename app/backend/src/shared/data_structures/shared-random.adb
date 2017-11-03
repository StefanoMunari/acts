with Ada.Numerics.discrete_Random;
with Ada.Text_IO;

package body Shared.Random is

   procedure Generate_Random (Lower_Bound, Upper_Bound : in     Positive;
                              Result                   :    out Positive) is

      use Ada.Text_IO;

      --  define an exception type to handle faulty parameters passing
      Bound_Error : exception;

      --  function that checks if user gave proper (valid) bounds
      function Are_Valid_Bounds (Lower_Bound, Upper_Bound : in Positive)
         return Boolean is
      begin
         if (Lower_Bound <= Upper_Bound) then
            return True;
         else
            return False;
         end if;
      end Are_Valid_Bounds;

      subtype Rand_Range is Natural range Lower_Bound .. Upper_Bound;
      package Rand_Int is new Ada.Numerics.Discrete_Random (Rand_Range);
      Seed : Rand_Int.Generator;
      Num : Rand_Range;

   begin
      --  check if bounds are valid
      if Are_Valid_Bounds (Lower_Bound,Upper_Bound) then
         --  generates a new random number between the given bounds and assign
         --  it to Result (out parameter)
         Rand_Int.Reset (Seed);
         Num := Rand_Int.Random (Seed);
         Result := Positive'Value (Rand_Range'Image (Num));
      else
         raise Bound_Error;
      end if;
   exception
      --  causes invoker termination with Program Error
      when Bound_Error =>
         Put_Line (Standard_Error, "ERROR: range's limits are not valid!!!");
   end Generate_Random;

end Shared.Random;
