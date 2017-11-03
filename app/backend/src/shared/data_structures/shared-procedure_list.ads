package Procedure_List_Pkg is -- todo list for a given moment
  new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Active.Agent.Reference,
      "="          => Active.Agent."=");