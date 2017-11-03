------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-bus_service
-- @purpose Interface for entities which are affected by going over a bus stop
-- @interface On_Bus_Stop (Object):
--              callback used when entering a bus stop
-- @dependencies -
-- @details Interface
------------------------------------------------------------------------------

package Active.Bus_Service is

   type Object is interface;
   type Reference is access all Bus_Service.Object'Class;

   not overriding
   procedure On_Bus_Stop (This : in out Bus_Service.Object) is abstract;

end Active.Bus_Service;
