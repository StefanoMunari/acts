-- core
with Ada.Containers.Indefinite_Ordered_Maps;

package Shared.Indefinite_String_Map is

   package Data is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => String,
      Element_Type => String,
       "="             => "=",
       "<"             => "<");

end Shared.Indefinite_String_Map;