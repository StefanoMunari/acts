with Ada.Containers.Indefinite_Doubly_Linked_Lists;

use Ada.Containers;

package Shared.String_List is
  new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type => String,
                                          "="          => "=");