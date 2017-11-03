with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Shared.JSON_Array_Utils;

package Shared.JSON_Array_Ref_Hashed_Map is
    new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Shared.JSON_Array_Utils.JSON_Array_Ref,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=",
       "=" => Shared.JSON_Array_Utils.Equals);
