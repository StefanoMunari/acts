------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-simple_executor
-- @purpose Class which implements the Executor interface. It uses a pool of
--          tasks in order to enable potential parallelism, though it is quite
--          simple because it uses just a set of static tasks and a single
--          work queue.
-- @interface @inherit
-- @dependencies application-backend::active-agent
--               application-backend::scheduling-command_broker
--               application-backend::scheduling-executor
--               application-backend::scheduling-work_queue
--               application-backend::scheduling-worker_thread
-- @details Object with state implemented as a Protected Object to make it
--          thread safe
------------------------------------------------------------------------------

with Active.Agent;

with Scheduling.Command_Broker;
with Scheduling.Executor;
with Scheduling.Work_Queue;
with Scheduling.Worker_Thread;

package Scheduling.Simple_Executor is

   package Real_Time         renames Ada.Real_Time;
   package Worker_Thread_Pkg renames Scheduling.Worker_Thread;

   type Object is new Executor.Object with private;
   type Reference is access all Simple_Executor.Object'Class;

   package Workers_List_Pkg is -- list of work items
     new Ada.Containers.Doubly_Linked_Lists (
         Element_Type => Worker_Thread_Pkg.Reference,
         "="          => Worker_Thread_Pkg."=");

   overriding
   procedure Init (
      This              :    out Simple_Executor.Object;
      Epoch             : in     Real_Time.Time_Span;
      Number_Of_Workers : in     Natural);

   overriding
   procedure Register (This      : in out Simple_Executor.Object;
                       Agent_Ref : in     Active.Agent.Reference;
                       Deferral  : in     Float);

   overriding
   procedure Register (This      : in out Simple_Executor.Object;
                       Agent_Ref : in     Active.Agent.Reference;
                       Deferral  : in     Real_Time.Time);

   overriding
   procedure Execute (This      : in out Simple_Executor.Object;
                      Agent_Ref : in     Active.Agent.Reference);

   overriding
   procedure Shutdown (This : in out Simple_Executor.Object);

   overriding
   function Dump (This : in Simple_Executor.Object)
   return G_JSON.JSON_Value;

   protected type State is
      procedure Init (Epoch_Arg : Real_Time.Time_Span);

      procedure Register (Agent_Ref : Active.Agent.Reference;
                          Deferral  : Real_Time.Time);

      procedure Cancel (Agent_Ref : in     Active.Agent.Reference;
                        Stopped   :    out Boolean);

      procedure Stop;

      function Dump return G_JSON.JSON_Value;
   private
      Agenda     : Agenda_Pkg.Map := Agenda_Pkg.Empty_Map;
      Epoch      : Real_Time.Time_Span;
   -- Start_Time is the clock time got when the system started
      Start_Time : Real_Time.Time;
   -- Stop_Time is the clock time got when the system stopped
      Stop_Time  : Real_Time.Time;
   -- Running tells if the system is being shutting down (False) or not (True)
      Running    : Boolean := True;
   end State;

private

   type Object is new Executor.Object with record
      Number_Of_Workers : Integer;
      Work_List  : Work_Queue.Reference;
      Controller : Command_Broker.Controller_Reference;
      Workers    : Workers_List_Pkg.List := Workers_List_Pkg.Empty_List;
      State      : access Simple_Executor.State;
   end record;

end Scheduling.Simple_Executor;
