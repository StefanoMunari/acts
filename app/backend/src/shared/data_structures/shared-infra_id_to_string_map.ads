with Ada.Containers.Indefinite_Ordered_Maps;

with Reactive; use Reactive.Infra_Id_Type;

package Shared.Infra_Id_To_String_Map is
    new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type        => Infra_Id,
       Element_Type    => String,
       "="             => "=",
       "<"             => "<");
