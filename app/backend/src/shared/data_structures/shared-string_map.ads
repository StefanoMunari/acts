with Ada.Containers.Indefinite_Ordered_Maps;

package Shared.String_Map is
    new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type        => Natural,
       Element_Type    => String,
       "="             => "=",
       "<"             => "<");
