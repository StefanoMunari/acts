------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-scheduler
-- @purpose Facade for the scheduler
-- @interface Start (Agenda, District, Time):
--              Blocking entry which returns only when the scheduler has been
--              started
--            Schedule (Agent, Natural) -> Boolean:
--              Register a callback for the given agent. Returns true if no
--              error occurs
--            Shutdown:
--              Stops the scheduler instance, as well as the tasks running in
--              it
--            Dump -> JSON:
--              Dumps the state of the scheduler
-- @dependencies application-backend::scheduling-executor
-- @details Singleton. Protected Object.
------------------------------------------------------------------------------

with Active.Traffic_Light.Utils;

with Reactive.District;

with Scheduling.Executor;

package Scheduling.Scheduler is

   package Traffic_Light_Utils_Pkg renames Active.Traffic_Light.Utils;

   protected Instance is

      not overriding
      entry Start (
         Initial_Agenda :        Agenda_Pkg.Map := Agenda_Pkg.Empty_Map;
         District       : in     Reactive.District.Reference;
         Epoch          : in     Real_Time.Time_Span;
         Traffic_Light_Utils_Arg :
            access Traffic_Light_Utils_Pkg.Object'Class := null);

      not overriding
      procedure Schedule (Agent       : in     Active.Agent.Reference;
                          Deferred_To : in     Float;
                          Scheduled   :    out Boolean);

      not overriding
      entry Shutdown;

      not overriding
      function Dump return G_JSON.JSON_Value;

   private
      Traveller_Executor_Ref     : Scheduling.Executor.Reference;
      Traffic_Light_Executor_Ref : Scheduling.Executor.Reference;
      Initialized                : Boolean := False;
      Traffic_Light_Utils        :
         access Traffic_Light_Utils_Pkg.Object'Class := null;
   end Instance;

-- JSON FIELDS CONSTANTS
   function Ticks_Field return String is ("remainingTicks");

end Scheduling.Scheduler;
