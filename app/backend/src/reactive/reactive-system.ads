--------------------------------------------------------------------------------
-- @author <stefanomunari.sm@gmail.com>
-- @author <valle.sebastiano93@gmail.com>
-- @author <gianmarco.midena@gmail.com>
-- @context application-backend::shared-shared_references
-- @purpose This is a static package which enables to manage the system
--          procedures of Reactive. It manages the initialization
--          (instantiation) of the Reactive entities.
--          Init is directly invoked by the Interface_Layer.Init process.
-- @interface Init:
-- @dependencies
-- @details Boot and Shutdown of Reactive are managed by the Scheduler
--------------------------------------------------------------------------------
package Reactive.System is

   procedure Init;

   procedure Dump;

end Reactive.System;
