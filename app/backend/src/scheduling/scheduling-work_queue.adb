package body Scheduling.Work_Queue is

   use Active.Agent;

   function "=" (A, B : Work_Queue_Item) return Boolean is
   begin
      return
         A.Action = B.Action and
         A.Shutdown_Notification = B.Shutdown_Notification and
         A.Id = B.Id;
   end "=";

   protected body The_Queue is

      procedure Add_Item (Work_Item : in Work_Queue_Item) is
      begin
         Work_List.Append (Work_Item);
      end Add_Item;

      procedure Add_Items (Work_Items : in Work_Queue_List_Pkg.List) is
      begin
         for Work_Item of Work_Items loop
            Work_List.Append (Work_Item);
         end loop;
      end Add_Items;

      entry Take_Item (Work_Item : out Work_Queue_Item)
         when not Work_List.Is_Empty is
      begin
         -- extract item from Work_List
         Work_Item := Work_List.First_Element;
         Work_List.Delete_First;
      end Take_Item;

      function Is_Empty return Boolean is (Work_List.Is_Empty);

   end The_Queue;

end Scheduling.Work_Queue;
