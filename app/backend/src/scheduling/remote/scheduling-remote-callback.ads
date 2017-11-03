------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote
-- @purpose Type which executes replies to remote requests
-- @interface Execute (Object):
--              executes the callback
-- @dependencies -
-- @details Interface
------------------------------------------------------------------------------
package Scheduling.Remote.Callback is

   type Object is interface;
   type Reference is access all Callback.Object'Class;

   not overriding
   procedure Execute (This : in Callback.Object)
   is abstract;


end Scheduling.Remote.Callback;
