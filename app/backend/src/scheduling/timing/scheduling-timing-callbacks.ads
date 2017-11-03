------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-timing-callbacks
-- @purpose Timers which check whether a given request has been satisfied by a
--          callback
-- @interface Register(Agent, Executor, Span) -> Callback:
--              Fires a callback with a relative delay
-- @dependencies application-backend::scheduling-executor
-- @details -
------------------------------------------------------------------------------

with Ada.Real_Time.Timing_Events;
with Ada.Strings.Unbounded;

package Scheduling.Timing.Callbacks is

   package Real_Time     renames Ada.Real_Time;
   package Timing_Events renames Ada.Real_Time.Timing_Events;
   package SU            renames Ada.Strings.Unbounded;

   type Object is new Timing_Events.Timing_Event with record
      Request_Id : SU.Unbounded_String;
   end record;
   type Reference is access all Scheduling.Timing.Callbacks.Object'Class;

   procedure Register (Request_Id : SU.Unbounded_String);

   protected Handlers is
      procedure Handle (Event : in out Timing_Events.Timing_Event);
   end Handlers;

end Scheduling.Timing.Callbacks;
