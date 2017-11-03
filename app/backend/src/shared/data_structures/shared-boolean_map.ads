with Ada.Containers.Ordered_Maps;

package Shared.Boolean_Map is
    new Ada.Containers.Ordered_Maps
      (Key_Type        => Natural,
       Element_Type    => Boolean,
       "="             => "=",
       "<"             => "<");
