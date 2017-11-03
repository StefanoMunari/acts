with Ada.Containers.Ordered_Sets;

package Shared.Natural_Set is
    new Ada.Containers.Ordered_Sets
      (Element_Type => Natural,
       "="          => "=");
