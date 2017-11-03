------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-travel_planning
-- @purpose Represents the planning of a travel
-- @interface Get_Instance
--              returns an instance for this package
--            Plan (Object, Travel):
--              plans the travel for a given traveller
--            Get_Route_From_A_To_B (Integer, Integer, Travel, String, String)
--                                  -> List:
--              returns the route for a traveller, given its type, the source
--              and the destination
--            Advance (Object, Travel):
--              causes the traveller to plan its travel and be re-scheduled
--              for a later moment
--            Has_Next_Step (Object, Travel) -> Boolean:
--              false (trivial)
--            Is_Progressing (Object, Travel) -> Boolean:
--              false (trivial)
-- @dependencies application-backend::active-space_master
--               application-backend::active-travel-travel_state
--               application-backend::active-travel-travel_progress
--               application-backend::active-traveller-utils
--               application-backend::reactive
--               application-backend::reactive-infrastructure-utils
--               application-backend::reactive-infrastructure-street-utils
--               application-backend::reactive-infrastructure-stretch-utils
--               application-backend::reactive-infrastructure-building-host-utils
--               application-backend::reactive-infrastructure-intersection-utils
--               application-backend::shared-infra_id_set
-- @details Singleton
------------------------------------------------------------------------------

with Active.Space_Master;
with Active.Travel.Travel_State;
with Active.Travel.Travel_Progress;
with Active.Traveller.Utils;

with Reactive;
with Reactive.Infrastructure.Utils;
with Reactive.Infrastructure.Street.Utils;
with Reactive.Infrastructure.Stretch.Utils;
with Reactive.Infrastructure.Building.Host.Utils;
with Reactive.Infrastructure.Intersection.Utils;

with Shared.Infra_Id_Set;

package Active.Travel.Travel_Planning is

   package Infra_Id_Set renames Shared.Infra_Id_Set;
   use Reactive.Infra_Id_Type;
   use Reactive.Stretch_Type_Package;

   type Object (<>)
   is limited new Travel_State.Object with private;
   type Reference is access all Travel_Planning.Object'Class;

   not overriding
   function Get_Instance (
      Travel_Progress :
         access Active.Travel.Travel_Progress.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Street_Utils :
         access Reactive.Infrastructure.Street.Utils.Object'Class := null;
      Stretch_Utils :
         access Reactive.Infrastructure.Stretch.Utils.Object'Class := null;
      Intersection_Utils :
         access Reactive.Infrastructure.Intersection.Utils.Object'Class
            := null;
      Host_Utils :
         access Reactive.Infrastructure.Building.Host.Utils.Object'Class
            := null;
      Space_Master :
         access Active.Space_Master.Object'Class := null;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class := null)
      return Travel_Planning.Reference;

   overriding
   procedure Plan (This   : in out Travel_Planning.Object;
                   Travel : in out Active.Travel.Object'Class);

   function Get_Route_From_A_To_B (
      A          : in     Infra_Id;
      B          : in     Infra_Id;
      Travel_Obj : in out Active.Travel.Object'Class;
      S_Type     : in     String;
      Agent_Id   : in     String)
   return Infra_Id_List.List;

   overriding
   procedure Advance (This   : in out Travel_Planning.Object;
                      Travel : in out Active.Travel.Object'Class);

   overriding
   function Has_Next_Step (This   : in Travel_Planning.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Is_Progressing (This   : in Travel_Planning.Object;
                            Travel : in Active.Travel.Object'Class)
   return Boolean;

   overriding
   function Dump (This : Travel_Planning.Object) return SU.Unbounded_String;

private
   type Object is limited new Travel_State.Object with record
      Travel_Progress      :
         access Active.Travel.Travel_Progress.Object'Class;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class;
      Street_Utils         :
         access Reactive.Infrastructure.Street.Utils.Object'Class;
      Stretch_Utils        :
         access Reactive.Infrastructure.Stretch.Utils.Object'Class;
      Intersection_Utils   :
         access Reactive.Infrastructure.Intersection.Utils.Object'Class;
      Host_Utils           :
         access Reactive.Infrastructure.Building.Host.Utils.Object'Class;
      Space_Master         :
         access Active.Space_Master.Object'Class := null;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class := null;
   end record;

   Instance : Travel_Planning.Reference := null;

end Active.Travel.Travel_Planning;
