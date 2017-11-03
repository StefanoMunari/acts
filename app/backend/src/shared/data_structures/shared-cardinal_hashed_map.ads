with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

with Shared.Direction;

package Shared.Cardinal_Hashed_Map is
    new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Direction.Cardinal,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=",
       "="             => Direction."=");
