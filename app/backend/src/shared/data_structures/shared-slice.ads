with Ada.Containers.Ordered_Maps;

with Reactive;
use Reactive.Stretch_Type_Package;

with Shared.Infra_Id_List;

package Shared.Slice is
    new Ada.Containers.Ordered_Maps (
       Key_Type        => Stretch_Type,
       Element_Type    => Infra_Id_List.List,
       "="             => Infra_Id_List."=",
       "<"             => "<");
