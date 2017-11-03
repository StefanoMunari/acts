------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-system
-- @purpose This is a static package which enables to manage the system
--          procedures of Scheduling. It manages the initialization
--          (instantiation) of the Scheduling package, the start and the
--          shutdown of the Scheduling system.
--          Start and shutdown are procedures triggered by a signal coming from
--          Interface_Layer.Skeleton. Init is directly invoked by the
--          Interface_Layer.Init process.
-- @interface Start:
--              Initializes and starts the scheduling subsystem
--            Shutdown:
--              Terminates the scheduling subsystem
--            Dump
--              Dumps the state of the scheduling subsystem
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Scheduling.System is

   procedure Start;

   procedure Dump;

   function Shutdown return Boolean;

private
   protected State is
      procedure Set_Running (
         New_State : in     Boolean;
         Old_State :    out Boolean);
      function Get_State return Boolean;
   private
      Booted : Boolean := False;
   end State;

end Scheduling.System;
