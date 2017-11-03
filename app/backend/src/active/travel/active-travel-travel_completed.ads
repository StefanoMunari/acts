------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-travel_completed
-- @purpose Represents the end of a travel
-- @interface Get_Instance
--              returns an instance for this package
--            Plan (Object, Travel):
--              does nothing (the travel is completed)
--            Advance (Object, Travel):
--              makes the traveller enter the host facility and be re-scheduled
--              for a later moment
--            Has_Next_Step (Object, Travel) -> Boolean:
--              false (trivial)
--            Is_Progressing (Object, Travel) -> Boolean:
--              false (trivial)
-- @dependencies application-backend::active-people_carrier-utils
--               application-backend::active-space_master
--               application-backend::active-travel-travel_state
--               application-backend::active-travel-travel_planning
--               application-backend::active-traveller-utils
--               application-backend::reactive-infrastructure-building-host-utils
-- @details Singleton
------------------------------------------------------------------------------

with Active.People_Carrier.Utils;
with Active.Space_Master;
with Active.Travel.Travel_State;
limited with Active.Travel.Travel_Planning;
limited with Active.Traveller.Utils;

with Reactive.Infrastructure.Building.Host.Utils;
limited with Reactive.Infrastructure.Stretch.Utils;

package Active.Travel.Travel_Completed is

   type Object (<>)
   is limited new Travel_State.Object with private;
   type Reference is access all Travel_Completed.Object'Class;

   function Get_Instance (
      Host_Utils : access
      Reactive.Infrastructure.Building.Host.Utils.Object'Class
         := null;
      Travel_Planning : access Active.Travel.Travel_Planning.Object'Class
         := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class
         := null;
      Space_Master : access Active.Space_Master.Object'Class
         := null;
      PC_Utils : access Active.People_Carrier.Utils.Object'Class
         := null;
      Stretch_Utils : access Reactive.Infrastructure.Stretch.Utils.Object'Class
         := null) return Travel_Completed.Reference;

   overriding
   procedure Plan (This   : in out Travel_Completed.Object;
                   Travel : in out Active.Travel.Object'Class) is null;

   overriding
   procedure Advance (This   : in out Travel_Completed.Object;
                      Travel : in out Active.Travel.Object'Class);

   overriding
   function Has_Next_Step (This   : in Travel_Completed.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Is_Progressing (This   : in Travel_Completed.Object;
                            Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Dump (This : Travel_Completed.Object) return SU.Unbounded_String;

private
   type Object is limited new Travel_State.Object with record
      Host_Utils : access
         Reactive.Infrastructure.Building.Host.Utils.Object'Class
         := null;
      Travel_Planning : access Active.Travel.Travel_Planning.Object'Class
         := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class
         := null;
      Space_Master : access Active.Space_Master.Object'Class
         := null;
      PC_Utils : access Active.People_Carrier.Utils.Object'Class
         := null;
      Stretch_Utils : access Reactive.Infrastructure.Stretch.Utils.Object'Class
         := null;
   end record;

   Instance : Travel_Completed.Reference := null;

   TIME_SPENT_IN_HOST : constant Natural := 60;

end Active.Travel.Travel_Completed;
