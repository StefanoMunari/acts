with Ada.Containers.Ordered_Maps;

with Active.Agent;

package Shared.Agent_Boolean_Map is
    new Ada.Containers.Ordered_Maps
      (Key_Type        => Active.Agent.Agent_Id,
       Element_Type    => Boolean,
       "="             => "=",
       "<"             => Active.Agent."<");
