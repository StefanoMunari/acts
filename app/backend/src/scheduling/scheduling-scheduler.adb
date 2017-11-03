-- core
with Ada.Containers; use Ada.Containers;

with Scheduling.Simple_Executor;
with Scheduling.Timing.Events;

package body Scheduling.Scheduler is

   package Real_Time renames Ada.Real_Time;

   protected body Instance is

      entry Start (
         Initial_Agenda :        Agenda_Pkg.Map := Agenda_Pkg.Empty_Map;
         District       : in     Reactive.District.Reference;
         Epoch          : in     Real_Time.Time_Span;
         Traffic_Light_Utils_Arg :
            access Traffic_Light_Utils_Pkg.Object'Class := null)
      when not Initialized
      is
         Who_Will_Act_Id : Active.Agent.Agent_Id;
         Who_Will_Act    : Active.Agent.Reference;
         When_Will_Act   : Real_Time.Time;
      begin -- Start
         Traffic_Light_Utils := Traffic_Light_Utils_Arg;
         if Traffic_Light_Utils_Arg = null then
            Traffic_Light_Utils := Traffic_Light_Utils_Pkg.Get_Instance;
         end if;

         Traveller_Executor_Ref := new Scheduling.Simple_Executor.Object;
         Traveller_Executor_Ref.Init (Epoch, 4); -- 4 worker threads

         Traffic_Light_Executor_Ref := new Scheduling.Simple_Executor.Object;
         Traffic_Light_Executor_Ref.Init (Epoch, 1); -- a single worker thread

         Initialized := True;

         -- Schedule actions in agenda
         for Action in Initial_Agenda.Iterate loop
            Who_Will_Act_Id := Agenda_Pkg.Key(Action);
            When_Will_Act   := Agenda_Pkg.Element(Action);
            Who_Will_Act    := District.Find_Agent_By_Id (Who_Will_Act_Id);
            if Traffic_Light_Utils.Is_A_Traffic_Light (Who_Will_Act_Id) then
               Traffic_Light_Executor_Ref.Register (
                  Who_Will_Act, When_Will_Act);
            else
               Traveller_Executor_Ref.Register (
                  Who_Will_Act, When_Will_Act);
            end if;
         end loop;
      end Start;

      procedure Schedule (Agent       : in     Active.Agent.Reference;
                          Deferred_To : in     Float;
                          Scheduled   :    out Boolean)
      is
      begin
         if Traffic_Light_Utils.Is_A_Traffic_Light (Agent.Get_Id) then
            Traffic_Light_Executor_Ref.Register (Agent, Deferred_To);
         else
            Traveller_Executor_Ref.Register (Agent, Deferred_To);
         end if;
         Scheduled := True;
      end Schedule;

      entry Shutdown
         when Initialized is
      begin -- Shutdown
         Traffic_Light_Executor_Ref.Shutdown;
         Traveller_Executor_Ref.Shutdown;
         Initialized := False;
      end Shutdown;

      function Dump return G_JSON.JSON_Value
      is
         Traveller_JSON       : G_JSON.JSON_Value;
         Traveller_Array      : G_JSON.JSON_Array;
         Single_Traveller     : G_JSON.JSON_Value;
         Traveller_L          : Integer;
         Traffic_Light_JSON   : G_JSON.JSON_Value;
         Traffic_Light_Array  : G_JSON.JSON_Array;
         Single_Traffic_Light : G_JSON.JSON_Value;
         Traffic_Light_L      : Integer;
         Whole_Array          : G_JSON.JSON_Array := G_JSON.Empty_Array;
         Whole_JSON           : G_JSON.JSON_Value;
      begin
         Traveller_JSON := Traveller_Executor_Ref.Dump;
         Traveller_Array := Traveller_JSON.Get;
         Traffic_Light_JSON := Traffic_Light_Executor_Ref.Dump;
         Traffic_Light_Array := Traffic_Light_JSON.Get;

         Traveller_L := G_JSON.Length (Traveller_Array);
         for I in 1 .. Traveller_L loop
            Single_Traveller := G_JSON.Get (Traveller_Array, I);
            G_JSON.Append (Whole_Array, Single_Traveller);
         end loop;

         Traffic_Light_L := G_JSON.Length (Traffic_Light_Array);
         for I in 1 .. Traffic_Light_L loop
            Single_Traffic_Light := G_JSON.Get (Traffic_Light_Array, I);
            G_JSON.Append (Whole_Array, Single_Traffic_Light);
         end loop;

         Whole_JSON := G_JSON.Create (Whole_Array);
         return Whole_JSON;
      end Dump;

   end Instance;

end Scheduling.Scheduler;
