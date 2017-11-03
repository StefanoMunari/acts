with Ada.Containers.Ordered_Maps;

with Active.Agent;

with Shared.Agent_Id_List;

 package Shared.Agent_Id_To_Agent_Id_List_Map is
  new Ada.Containers.Ordered_Maps
    (Key_Type        => Active.Agent.Agent_Id,
     Element_Type    => Shared.Agent_Id_List.List,
     "="             => Shared.Agent_Id_List."=",
     "<"             => Active.Agent."<");
