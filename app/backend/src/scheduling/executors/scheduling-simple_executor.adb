with Scheduling.Timing.Events;

package body Scheduling.Simple_Executor is

   package Work_Queue_Pkg renames Scheduling.Work_Queue;

   use Real_Time;

   procedure Init (
      This              :    out Simple_Executor.Object;
      Epoch             : in     Real_Time.Time_Span;
      Number_Of_Workers : in     Natural)
   is
   begin
      This.Number_Of_Workers := Number_Of_Workers;
      This.Controller := new Command_Broker.Controller_For_Workers;
      This.Controller.Init (Number_Of_Workers);
      This.Work_List := new Work_Queue.The_Queue;

      for I in 1 .. Number_Of_Workers loop
         declare
            Worker : Worker_Thread_Pkg.Reference := new Worker_Thread_Pkg.T;
         begin
            Worker.Init (This.Work_List, This.Controller);
            This.Workers.Append (Worker);
         end;
      end loop;

      This.State := new Simple_Executor.State;
      This.State.Init (Epoch);
   end Init;

   procedure Register (This      : in out Simple_Executor.Object;
                       Agent_Ref : in     Active.Agent.Reference;
                       Deferral  : in     Float)
   is
      Span              : constant Real_Time.Time_Span :=
         Real_Time.Milliseconds (Natural (Deferral * 1000.0));
      Absolute_Deferral : Real_Time.Time := Real_Time.Clock + Span;
   begin
      This.Register (Agent_Ref, Absolute_Deferral);
   end Register;

   procedure Register (This      : in out Simple_Executor.Object;
                       Agent_Ref : in     Active.Agent.Reference;
                       Deferral  : in     Real_Time.Time)
   is
      Timing_Event : Scheduling.Timing.Events.Reference;
   begin
      This.State.Register (Agent_Ref, Deferral);
      Timing_Event :=
         Scheduling.Timing.Events.Register (
            Agent_Ref, This'Unchecked_Access, Deferral);
   end Register;

   procedure Execute (This      : in out Simple_Executor.Object;
                      Agent_Ref : in     Active.Agent.Reference)
   is
      Stopped   : Boolean := False;
      Work_Item : Work_Queue_Pkg.Work_Queue_Item;
   begin
      This.State.Cancel (Agent_Ref, Stopped);
      if Stopped then
         return;
      end if;
      Work_Item.Action                := Agent_Ref;
      Work_Item.Shutdown_Notification := False;
      This.Work_List.Add_Item (Work_Item);
   end Execute;

   procedure Shutdown (This : in out Simple_Executor.Object)
   is
   begin
      This.State.Stop;

      for I in 1 .. This.Number_Of_Workers loop
         declare
            Work_Item : Work_Queue_Pkg.Work_Queue_Item;
         begin
         -- IMPORTANT!!! Id is needed so that we actually append
         -- Number_Of_Workers elements
            Work_Item.Id := I;
            Work_Item.Shutdown_Notification := True;
            This.Work_List.Add_Item (Work_Item);
         end;
      end loop;

      This.Controller.Wait_For_Workers;
   end Shutdown;

   function Dump (This : in Simple_Executor.Object)
   return G_JSON.JSON_Value
   is
   begin
      return This.State.Dump;
   end Dump;

   -------------
   --- AGENDA
   -------------

   protected body State is
      procedure Init (Epoch_Arg : Real_Time.Time_Span) is
      begin
         Start_Time := Real_Time.Clock;
         Epoch := Epoch_Arg;
      end Init;

      procedure Register (Agent_Ref : Active.Agent.Reference;
                          Deferral  : Real_Time.Time)
      is
      begin
         Agenda.Include (Agent_Ref.Get_Id, Deferral);
      end Register;

      procedure Cancel (Agent_Ref : in     Active.Agent.Reference;
                        Stopped   :    out Boolean)
      is
         Id : Active.Agent.Agent_Id := Agent_Ref.Get_Id;
      begin
         if Running and Agenda.Contains (Id) then
            Agenda.Delete (Id);
         end if;
      -- execute Action only if not running
         Stopped := not Running;
      end Cancel;

      procedure Stop is
      begin
         Stop_Time := Real_Time.Clock;
         Running := False;
      end Stop;

      function Dump
      return G_JSON.JSON_Value
      is
         JSON       : G_JSON.JSON_Value := G_JSON.Create_Object;
         Elapsed    : Real_Time.Time;
         SC         : Real_Time.Seconds_Count;
         TS         : Real_Time.Time_Span;
         Elapsed_Ms : Integer;  -- milliseconds
      begin
         JSON.Set_Field (Actions_Field, Agenda_To_Json (Agenda, Stop_Time));

         Elapsed := Real_Time.Time_Of (0, Epoch + Stop_Time - Start_Time);
         Real_Time.Split (Elapsed, SC, TS);
         Elapsed_Ms := Integer (Float (SC) * 1000.0);
         if Elapsed_Ms < 0 then
            Elapsed_Ms := 0;
         end if;
         JSON.Set_Field (Elapsed_Field, Elapsed_Ms);

         return JSON;
      end Dump;
   end State;

end Scheduling.Simple_Executor;
