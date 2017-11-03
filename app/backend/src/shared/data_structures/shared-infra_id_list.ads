with Ada.Containers.Doubly_Linked_Lists;

with Reactive;
use Reactive.Infra_Id_Type;

use Ada.Containers;

package Shared.Infra_Id_List is
  new Ada.Containers.Doubly_Linked_Lists (Element_Type => Infra_Id,
                                          "="          => "=");
