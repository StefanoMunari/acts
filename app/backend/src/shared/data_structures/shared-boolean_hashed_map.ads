with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

package Shared.Boolean_Hashed_Map is
    new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Boolean,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");
