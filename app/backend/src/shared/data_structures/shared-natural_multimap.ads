with Ada.Containers.Ordered_Maps;
with Shared.Natural_Set;

package Shared.Natural_Multimap is
  new Ada.Containers.Ordered_Maps
    (Key_Type        => Natural,
     Element_Type    => Natural_Set.Set,
     "="             => Natural_Set."=",
     "<"             => "<");
