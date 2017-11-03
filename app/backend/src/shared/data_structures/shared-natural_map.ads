with Ada.Containers.Ordered_Maps;

package Shared.Natural_Map is
    new Ada.Containers.Ordered_Maps
      (Key_Type        => Natural,
       Element_Type    => Natural,
       "="             => "=",
       "<"             => "<");

