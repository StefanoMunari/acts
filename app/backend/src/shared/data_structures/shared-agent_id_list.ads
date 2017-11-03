with Ada.Containers.Doubly_Linked_Lists;

with Active.Agent;

use Ada.Containers;

package Shared.Agent_Id_List is
   new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Active.Agent.Agent_Id,
       "="          => Active.Agent."=");
