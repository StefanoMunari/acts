with Ada.Finalization;

with Reactive.District;
with Reactive.Infrastructure.Street.Utils;
with Reactive.Infrastructure.Intersection.Utils;
with Reactive.Infrastructure.Lane.Utils;

with Shared.Infra_Id_List;

package Reactive.Infrastructure.Intersection.Intersection_Builder is

   package Infra_Id_List renames Shared.Infra_Id_List;

   type Object is abstract new Ada.Finalization.Limited_Controlled with private;
   type Reference is access all Intersection_Builder.Object;

   not overriding
   procedure With_Street (This      : in out Intersection_Builder.Object;
                          Street_Id : in     Infra_Id;
                          Stretches : in     Infra_Id_List.List;
                          Direction : in     Shared.Direction.Cardinal);

   procedure With_Traffic_Light (
      This           : in out Intersection_Builder.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal);

   not overriding
   function Get_Result (This : in Intersection_Builder.Object)
   return Infra_Id;

private
   type Object
   is abstract new Ada.Finalization.Limited_Controlled with record
      Intersection       : access Infrastructure.Intersection.Object'Class;
      District           : access Reactive.District.Object'Class;
      Street_Utils       : access Street.Utils.Object'Class;
      Intersection_Utils : access Infrastructure.Intersection.Utils.Object'Class;
      Lane_Utils         : access Lane.Utils.Object'Class;
   end record;

   procedure Init (
      Builder            :
         in out Intersection.Intersection_Builder.Object'Class;
      Intersection_Id    : in Infra_Id;
      Intersection_Type  : in Intersection.Intersection_Type;
      District           : access Reactive.District.Object'Class := null;
      Street_Utils       : access Street.Utils.Object'Class := null;
      Intersection_Utils : access Intersection.Utils.Object'Class := null;
      Lane_Utils         : access Lane.Utils.Object'Class := null;
      Intersection       :
         access Infrastructure.Intersection.Object'Class := null;
      Traveller_Utils    : access Active.Traveller.Utils.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Crossing_Strategy  :
         access Infrastructure.Intersection.Crossing.Object'Class := null);

end Reactive.Infrastructure.Intersection.Intersection_Builder;
