with Ada.Containers.Ordered_Maps;

with Active.Agent;

with Reactive; use Reactive.Infra_Id_Type;

package Shared.Agent_Id_To_Infra_Id_Map is
    new Ada.Containers.Ordered_Maps
      (Key_Type        => Active.Agent.Agent_Id,
       Element_Type    => Infra_Id,
       "="             => "=",
       "<"             => Active.Agent."<");
