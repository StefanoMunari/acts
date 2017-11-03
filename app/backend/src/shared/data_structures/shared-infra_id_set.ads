with Ada.Containers.Ordered_Sets;

with Reactive;
use Reactive.Infra_Id_Type;

package Shared.Infra_Id_Set is
    new Ada.Containers.Ordered_Sets
      (Element_Type => Infra_Id,
       "="          => "=");
