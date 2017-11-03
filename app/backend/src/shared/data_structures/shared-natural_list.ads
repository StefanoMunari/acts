with Ada.Containers.Doubly_Linked_Lists;

use Ada.Containers;

package Shared.Natural_List is
  new Ada.Containers.Doubly_Linked_Lists (Element_Type => Natural,
                                          "="          => "=");
