with Active.Agent;

limited with Reactive.District;

package Reactive.Infrastructure.Lane.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Lane.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Lane.Utils.Reference;

   not overriding
   procedure Enter (
      This         : in     Lane.Utils.Object;
      Lane_Id      : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id);

   not overriding
   function Find_Street (This    : in Lane.Utils.Object;
                         Lane_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Find_Intersections (This    : in Lane.Utils.Object;
                                Lane_Id : in Infra_Id)
   return Infra_Id_Set.Set;

   not overriding
   function Count_Stretches (This    : in Lane.Utils.Object;
                             Lane_Id : in Infra_Id)
   return Natural;

   not overriding
   procedure Find_Stretch_Position (
      This                : in     Lane.Utils.Object;
      Lane_Id, Stretch_Id : in     Infra_Id;
      Stretch_Position    :    out Natural;
      Found               :    out Boolean);

   not overriding
   function Get_Stretch_By_Position (
      This             : in Lane.Utils.Object;
      Lane_Id          : in Infra_Id;
      Stretch_Position : in Natural)
   return Infra_Id;

   not overriding
   function Get_Direction (This    : in Lane.Utils.Object;
                           Lane_Id : in Infra_Id)
   return Direction.Straight;

   not overriding
   procedure Add_Intersection (
      This            : in     Lane.Utils.Object;
      Lane_Id,
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

   not overriding
   function Is_Contained_By (This                  : in Lane.Utils.Object;
                             Lane_Id, Container_Id : in Infra_Id)
   return Boolean;

   not overriding
   procedure Attempt_Overtake (
      This                : in Lane.Utils.Object;
      Lane_Id             : in Infra_Id;
      Stretch_Id          : in Infra_Id;
      Max_Overtake_Length : in Natural;
      Traveller_Id        : in Agent.Agent_Id);

   not overriding
   function Dump (This : Lane.Utils.Object; Lane_Id : in Infra_Id)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Lane.Utils.Reference;

end Reactive.Infrastructure.Lane.Utils;
