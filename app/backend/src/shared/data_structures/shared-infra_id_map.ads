with Ada.Containers.Ordered_Maps;

with Reactive; use Reactive.Infra_Id_Type;

package Shared.Infra_Id_Map is
    new Ada.Containers.Ordered_Maps
      (Key_Type        => Infra_Id,
       Element_Type    => Infra_Id,
       "="             => "=",
       "<"             => "<");
