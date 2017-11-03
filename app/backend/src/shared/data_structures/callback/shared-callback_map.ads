with Ada.Containers.Indefinite_Ordered_Maps;

with Shared.Callback_Pair;

package Shared.Callback_Map is
    new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type        => String,
       Element_Type    => Shared.Callback_Pair.Object,
       "="             => Shared.Callback_Pair."=",
       "<"             => "<");
