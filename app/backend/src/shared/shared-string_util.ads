with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Containers;

package Shared.String_Util is
   package SU renames Ada.Strings.Unbounded;

   function Strip_Borders (Input : in SU.Unbounded_String)
                           return String;
end Shared.String_Util;
