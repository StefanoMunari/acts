with Ada.Containers.Ordered_Maps;

with Reactive; use Reactive.Infra_Id_Type;

with Shared.Infra_Id_Set;

package Shared.Infra_Id_Multimap is
  new Ada.Containers.Ordered_Maps
    (Key_Type        => Infra_Id,
     Element_Type    => Infra_Id_Set.Set,
     "="             => Infra_Id_Set."=",
     "<"             => "<");
