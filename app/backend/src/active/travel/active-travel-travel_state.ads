------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-travel_state
-- @purpose Root of the hierarchy of a family of singletons, each of one
--          representing a given phase of a travel
-- @interface Plan (Object, Travel):
--              plans a travel
--            Advance (Object, Travel):
--              makes the travel advance
--            Has_Next_Step (Object, Travel) -> Boolean:
--              look for next steps
--            Is_Progressing (Object, Travel) -> Boolean:
--              is the travel in progress or paused?
-- @dependencies -
-- @details Interface, Limited
------------------------------------------------------------------------------

package Active.Travel.Travel_State is

   type Object is limited interface;
   type Reference is access all Travel_State.Object'Class;

   not overriding
   procedure Plan (This   : in out Travel_State.Object;
                   Travel : in out Active.Travel.Object'Class) is abstract;

   not overriding
   procedure Advance (This   : in out Travel_State.Object;
                      Travel : in out Active.Travel.Object'Class) is abstract;

   not overriding
   function Has_Next_Step (This   : in Travel_State.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean is abstract;

   not overriding
   function Is_Progressing (This   : in Travel_State.Object;
                            Travel : in Active.Travel.Object'Class)
   return Boolean is abstract;

   not overriding
   function Dump (This : Travel_State.Object) return SU.Unbounded_String
   is abstract;

end Active.Travel.Travel_State;
