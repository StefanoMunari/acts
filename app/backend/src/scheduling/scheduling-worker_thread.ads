------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-worker_thread
-- @purpose Task which executes actions. After being initialized, it repeatedly
--          takes work items from the head of the work queue, until it fetches
--          a poison pill, which causes it to stop.
--          Before stopping, the worker thread notifies the controller that it
--          is going to stop.
-- @interface Init (Work_Queue, Controller):
--              Initializes the worker thread with a work queue and a
--              controller.
-- @dependencies application-backend::scheduling-command_broker
--               application-backend::scheduling-work_queue
-- @details Task
------------------------------------------------------------------------------

with Scheduling.Command_Broker;
with Scheduling.Work_Queue;

package Scheduling.Worker_Thread is

   package Work_Queue_Pkg renames Scheduling.Work_Queue;

   task type T is
      entry Init    (Q  : in out Work_Queue_Pkg.Reference;
                     C  : in     Command_Broker.Controller_Reference);
   end T;

   type Reference is access T;


   function "=" (A, B : Reference) return Boolean
   is (A = B);

end Scheduling.Worker_Thread;
