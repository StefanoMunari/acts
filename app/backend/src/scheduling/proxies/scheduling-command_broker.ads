------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-command_broker
-- @purpose Controller which comes handy to shut down the executor, which can
--          wait on the Wait_For_Workers entry after feeding them with a poison
--          pill.
-- @interface Init (Integer):
--              Initializes the controller with the total number of workers
--            Worker_Done:
--              Keeps track that a worker thread stopped
--            Wait_For_Workers:
--              Blocking entry to wait for all workers to stop
-- @dependencies -
-- @details Protected Object.
------------------------------------------------------------------------------

package Scheduling.Command_Broker is

   protected type Controller_For_Workers is

      procedure Init (Maximum : in Integer);

      procedure Worker_Done;

      entry Wait_For_Workers;

   private

      Total_Workers       : Integer := 0;

      Stopped_Workers     : Integer := 0;

   end Controller_For_Workers;

   type Controller_Reference is access Controller_For_Workers;

end Scheduling.Command_Broker;
