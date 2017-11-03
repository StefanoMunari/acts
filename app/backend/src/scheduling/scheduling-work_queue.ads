------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-work_queue
-- @purpose Protected object which acts as a buffer between producers of work
--          items and consumers of work items.
-- @interface Add_Item (Work_Item):
--              Adds an item to the queue
--            Add_Items (List):
--              Adds several work items to the queue
--            Take_Item -> Work_Item:
--              Blocking entry to fetch a work item from the queue
--            Is_Empty -> Boolean:
--              Function to quickly check the queue state (true iff not empty)
-- @dependencies -
-- @details Protected Object. In this package we also have a type definition,
--          Work_Queue_Item: it is used for items in a work queue, and the id
--          field is just a discriminant for being able to load more than one
--          poison pill.
------------------------------------------------------------------------------

package Scheduling.Work_Queue is

   type Work_Queue_Item is record
      Id                    : Natural := 0;
      Action                : Active.Agent.Reference := null;
      Shutdown_Notification : Boolean := False;   -- A.K.A. Poison pill
   end record;

   function "=" (A, B : Work_Queue_Item) return Boolean;

   package Work_Queue_List_Pkg is -- list of work items
     new Ada.Containers.Doubly_Linked_Lists (
         Element_Type => Work_Queue_Item,
         "="          => "=");

   protected type The_Queue is

      procedure Add_Item  (Work_Item  : in     Work_Queue_Item);

      procedure Add_Items (Work_Items : in     Work_Queue_List_Pkg.List);

      entry     Take_Item (Work_Item  :    out Work_Queue_Item);

      function  Is_Empty return Boolean;

   private
      Work_List : Work_Queue_List_Pkg.List := Work_Queue_List_Pkg.Empty_List;
   end The_Queue;
   type Reference is access The_Queue;

end Scheduling.Work_Queue;
