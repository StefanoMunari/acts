------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-travel_progress
-- @purpose Represents the progress of a travel
-- @interface Get_Instance
--              returns an instance for this package
--            Plan (Object, Travel):
--              does nothing (travel is in progress)
--            Advance (Object, Travel):
--              causes the traveller to tread the next treadable piece of
--              infrastructure
--            Has_Next_Step (Object, Travel) -> Boolean:
--              true iff the traveller is not at the last stretch of its route
--            Is_Progressing (Object, Travel) -> Boolean:
--              true (trivial)
-- @dependencies application-backend::active-space_master
--               application-backend::active-travel-travel_state
--               application-backend::active-travel-travel_completed
--               application-backend::reactive-infrastructure-utils
-- @details Singleton
------------------------------------------------------------------------------

with Active.Space_Master;
with Active.Travel.Travel_State;
with Active.Travel.Travel_Completed;

with Reactive.Infrastructure.Utils;

package Active.Travel.Travel_Progress is

   type Object (<>)
   is limited new Travel_State.Object with private;
   type Reference is access all Travel_Progress.Object'Class;

   function Get_Instance (
      Travel_Completed :
         access Active.Travel.Travel_Completed.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Space_Master :
         access Active.Space_Master.Object'Class := null)
   return Travel_Progress.Reference;

   overriding
   procedure Plan (This   : in out Travel_Progress.Object;
                   Travel : in out Active.Travel.Object'Class) is null;

   overriding
   procedure Advance (This   : in out Travel_Progress.Object;
                      Travel : in out Active.Travel.Object'Class);

   overriding
   function Has_Next_Step (This   : in Travel_Progress.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Is_Progressing (This   : in Travel_Progress.Object;
                            Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Dump (This : Travel_Progress.Object) return SU.Unbounded_String;

private
   type Object is limited new Travel_State.Object with record
      Travel_Completed     :
         access Active.Travel.Travel_Completed.Object'Class;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class;
      Space_Master         :
         access Active.Space_Master.Object'Class := null;
   end record;

   Instance : Travel_Progress.Reference := null;

end Active.Travel.Travel_Progress;
