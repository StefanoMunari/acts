------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-system
-- @purpose This is a static package which enables to manage the system
--          procedures of Active. It manages the initialization
--          (instantiation) of the Active entities.
-- @interface Init:
--              Initializes the active subsystem
--            Dump
--              Dumps the state of the active subsystem
-- @dependencies -
-- @details -
------------------------------------------------------------------------------
package Active.System is

   procedure Init;

   procedure Dump;

end Active.System;
