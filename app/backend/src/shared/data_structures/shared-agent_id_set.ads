with Ada.Containers.Ordered_Sets;

with Active.Agent;
use Active.Agent;

package Shared.Agent_Id_Set is
    new Ada.Containers.Ordered_Sets
      (Element_Type => Agent_Id,
       "="          => "=");
