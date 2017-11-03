with Ada.Unchecked_Deallocation;

with AUnit.Assertions;

with Active.Agent.Mock;
use Active.Agent;

package body Scheduling.Work_Queue.Tests is

   package Ass            renames AUnit.Assertions;
   package Agent_Mock_Pkg renames Active.Agent.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   type Work_Queue_Ptr is access Scheduling.Work_Queue.The_Queue;
   Work_Queue_Obj : Work_Queue_Ptr := new Work_Queue.The_Queue;

   procedure Set_Up (T: in out Work_Queue_Test) is
   begin
   null;
      Work_Queue_Obj := new Work_Queue.The_Queue;
   end Set_Up;

   procedure Tear_Down (T: in out Work_Queue_Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Scheduling.Work_Queue.The_Queue, Work_Queue_Ptr);
   begin
   null;
      Free (Work_Queue_Obj);
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add_Item (T : in out TC.Test_Case'Class)
   is
      Work_Item : Work_Queue_Item;
   begin
      Ass.Assert (Work_Queue_Obj.Is_Empty,
                  "The Work_Queue is not empty");
      Work_Queue_Obj.Add_Item (Work_Item);
      Ass.Assert (not Work_Queue_Obj.Is_Empty,
                  "The Work_Queue is empty");
   end Test_Add_Item;

   procedure Test_Add_Items (T : in out TC.Test_Case'Class)
   is
      Work_Items  : Work_Queue_List_Pkg.List := Work_Queue_List_Pkg.Empty_List;
      Work_Item_1 : Work_Queue_Item;
      Work_Item_2 : Work_Queue_Item;
   begin
      Ass.Assert (Work_Queue_Obj.Is_Empty,
                  "The Work_Queue is not empty");
      Work_Items.Append (Work_Item_1);
      Work_Items.Append (Work_Item_2);
      Work_Queue_Obj.Add_Items (Work_Items);
      Ass.Assert (
         not Work_Queue_Obj.Is_Empty,
         "The Work_Queue is empty");
   end Test_Add_Items;

   procedure Test_Take_Item (T : in out TC.Test_Case'Class)
   is
      Def_Item    : Work_Queue_Item;
      Age_Item    : Work_Queue_Item;
      Agent_Mock  : Active.Agent.Reference := Agent_Mock_Pkg.Create;
      Id_Item     : Work_Queue_Item;
      Id          : Natural := 15;
      Shut_Item   : Work_Queue_Item;
      Full_Item   : Work_Queue_Item;
      Return_Item : Work_Queue_Item;
   begin
      Age_Item.Action := Agent_Mock;
      Id_Item.Id := Id;
      Shut_Item.Shutdown_Notification := True;
      Full_Item.Action := Agent_Mock;
      Full_Item.Id := Id;
      Full_Item.Shutdown_Notification := True;

      Ass.Assert (Work_Queue_Obj.Is_Empty,
                  "The Work_Queue is not empty");

      Work_Queue_Obj.Add_Item (Def_Item);
      Work_Queue_Obj.Take_Item (Return_Item);
      Ass.Assert (
         Def_Item = Return_Item,
         "Not getting the default item from the queue");

      Work_Queue_Obj.Add_Item (Age_Item);
      Work_Queue_Obj.Take_Item (Return_Item);
      Ass.Assert (
         Age_Item = Return_Item,
         "Not getting the item with agent from the queue");
      Ass.Assert (
         Age_Item.Action = Return_Item.Action,
         "Not getting the same agent from the work item");

      Work_Queue_Obj.Add_Item (Id_Item);
      Work_Queue_Obj.Take_Item (Return_Item);
      Ass.Assert (
         Id_Item = Return_Item,
         "Not getting the same agent from the queue");
      Ass.Assert (
         Id_Item.Id = Return_Item.Id,
         "Not getting the same id from the work item");

      Work_Queue_Obj.Add_Item (Shut_Item);
      Work_Queue_Obj.Take_Item (Return_Item);
      Ass.Assert (
         Shut_Item = Return_Item,
         "Not getting the same agent from the queue");
      Ass.Assert (
         Shut_Item.Shutdown_Notification = Return_Item.Shutdown_Notification,
         "Not getting the same shutdown notification from the work item");

      Work_Queue_Obj.Add_Item (Full_Item);
      Work_Queue_Obj.Take_Item (Return_Item);
      Ass.Assert (
         Full_Item = Return_Item,
         "Not getting the same agent from the queue");
      Ass.Assert (
         Full_Item.Action = Return_Item.Action,
         "Not getting the same agent from the work item");
      Ass.Assert (
         Full_Item.Id = Return_Item.Id,
         "Not getting the same id from the work item");
      Ass.Assert (
         Full_Item.Shutdown_Notification = Return_Item.Shutdown_Notification,
         "Not getting the same shutdown notification from the work item");
   end Test_Take_Item;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Work_Queue_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Add_Item'Access,
                        Name    => "Test add item");
      Register_Routine (Test    => T,
                        Routine => Test_Add_Items'Access,
                        Name    => "Test add items");
      Register_Routine (Test    => T,
                        Routine => Test_Take_Item'Access,
                        Name    => "Test take item");
   end Register_Tests;

   function Name(T: Work_Queue_Test) return AU.Message_String is
   begin
      return AU.Format ("Work_Queue");
   end Name;
end Scheduling.Work_Queue.Tests;
