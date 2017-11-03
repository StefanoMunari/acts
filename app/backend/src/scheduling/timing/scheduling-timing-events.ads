------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-timing-events
-- @purpose Callbacks for events deferred with a given time. At the moment of
--          their actual fulfillment, they asks an Executor to be executed
-- @interface Register(Agent, Executor, Span) -> Callback:
--              Fires a callback with a relative delay
--            Register(Agent, Executor, Time) -> Callback:
--              Fires a callback with an absolute delay
-- @dependencies application-backend::scheduling-executor
-- @details Handlers is just a loopback to Executor, which caused a callback
--          to be issued in the first place
------------------------------------------------------------------------------

with Ada.Real_Time.Timing_Events;

with Scheduling.Executor;

package Scheduling.Timing.Events is

   package Real_Time     renames Ada.Real_Time;
   package Timing_Events renames Ada.Real_Time.Timing_Events;

   type Object is new Timing_Events.Timing_Event with record
      Action_Ref   : Active.Agent.Reference;
      Executor_Ref : Scheduling.Executor.Reference;
   end record;
   type Reference is access all Scheduling.Timing.Events.Object'Class;

-- Actually, the following two could be procedures. Making them functions
--+ might be handy for testing
   function Register (
      Action_Ref   : Active.Agent.Reference;
      Executor_Ref : Scheduling.Executor.Reference;
      Span         : Real_Time.Time_Span)
   return Scheduling.Timing.Events.Reference;

   function Register (
      Action_Ref    : Active.Agent.Reference;
      Executor_Ref  : Scheduling.Executor.Reference;
      Deferral_Time : Real_Time.Time)
   return Scheduling.Timing.Events.Reference;

   protected Handlers is
      procedure Handle (Event : in out Timing_Events.Timing_Event);
   end Handlers;

end Scheduling.Timing.Events;
