with Ada.Containers.Ordered_Maps;

with Active.Agent;

with Shared.Infra_Id_List;

package Shared.Agent_Id_To_Infra_Id_List_Map is
  new Ada.Containers.Ordered_Maps
    (Key_Type        => Active.Agent.Agent_Id,
     Element_Type    => Shared.Infra_Id_List.List,
     "="             => Shared.Infra_Id_List."=",
     "<"             => Active.Agent."<");
