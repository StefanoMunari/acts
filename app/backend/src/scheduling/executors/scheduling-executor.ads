------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-executor
-- @purpose Interface for actions executors
-- @interface Register (Object, Agent, Time):
--              registers a callback for agent, given a deferral
--            Register (Object, Agent, Natural):
--              converts the number to Time and then calls the other Register
--            Execute (Object, Agent):
--              executes a given action without introducing any deferral
--            Shutdown (Object):
--              gracefully terminates the executor
-- @dependencies application-backend::active-agent
--               application-backend::shared-dumpable
-- @details Interface. The @interface API is incomplete for the sake of
--          clarity.
------------------------------------------------------------------------------

with Ada.Real_Time;

with Active.Agent;

with Shared.Dumpable;

package Scheduling.Executor is

   package Real_Time renames Ada.Real_Time;
   package Dumpable  renames Shared.Dumpable;

   type Object is abstract new Dumpable.Object with null record;
   type Reference is access all Executor.Object'Class;

   not overriding
   procedure Init (
      This              :    out Executor.Object;
      Epoch             : in     Real_Time.Time_Span;
      Number_Of_Workers : in     Natural)
   is abstract;

   not overriding
   procedure Register (This      : in out Executor.Object;
                       New_Agent : in     Active.Agent.Reference;
                       Span      : in     Float) is abstract;

   not overriding
   procedure Register (This      : in out Executor.Object;
                       New_Agent : in     Active.Agent.Reference;
                       Deferral  : in     Real_Time.Time) is abstract;

   not overriding
   procedure Execute (This   : in out Executor.Object;
                      Agent  : in     Active.Agent.Reference) is abstract;

   not overriding
   procedure Shutdown (This : in out Executor.Object) is abstract;

end Scheduling.Executor;
