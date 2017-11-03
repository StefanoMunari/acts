------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote-callback-failure
-- @purpose Type which executes failed replies to remote requests
-- @interface @inherit
-- @dependencies -
-- @details Interface.
------------------------------------------------------------------------------
-- core
with Ada.Finalization;

package Scheduling.Remote.Callback.Failure is

   type Object is abstract
   new Ada.Finalization.Controlled
   and Callback.Object
   with null record;
   type Reference is access all Failure.Object'Class;

   overriding
   procedure Execute (This : in Failure.Object)
   is abstract;

end Scheduling.Remote.Callback.Failure;
