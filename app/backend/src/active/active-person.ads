------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-person
-- @purpose Interface for people
-- @interface Act (Object):
--              performs an action
-- @dependencies -
-- @details Interface
------------------------------------------------------------------------------

package Active.Person is

   type Object is interface;
   type Reference is access all Person.Object'Class;

   not overriding
   procedure Act (This : in out Person.Object) is abstract;

end Active.Person;
