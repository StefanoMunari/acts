with Reactive.District;

package body Reactive.Infrastructure.Lane.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Lane.Utils.Reference is
   begin
      if Lane.Utils.Instance = null then
         Lane.Utils.Instance := new Lane.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Lane.Utils.Instance;
   end Get_Instance;

   procedure Enter (
      This         : in     Lane.Utils.Object;
      Lane_Id      : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.District
        .Find_Lane_By_Id (Lane_Id)
        .Enter (Traveller_Id => Traveller_Id);
   end Enter;

   function Find_Street (This    : in Lane.Utils.Object;
                         Lane_Id : in Infra_Id)
   return Infra_Id is
   begin
      return This.District
        .Find_Lane_By_Id (Lane_Id)
        .Find_Street;
   end Find_Street;

   function Find_Intersections (This    : in Lane.Utils.Object;
                                Lane_Id : in Infra_Id)
                                return Infra_Id_Set.Set is
   begin
      return This.District
        .Find_Lane_By_Id (Lane_Id)
        .Find_Intersections;
   end Find_Intersections;

   function Count_Stretches (This    : in Lane.Utils.Object;
                             Lane_Id : in Infra_Id) return Natural is
   begin
      return This.District
        .Find_Lane_By_Id (Lane_Id)
        .Count_Stretches;
   end Count_Stretches;

   procedure Find_Stretch_Position (
      This                : in     Lane.Utils.Object;
      Lane_Id, Stretch_Id : in     Infra_Id;
      Stretch_Position    :    out Natural;
      Found               :    out Boolean) is
   begin
      This.District
        .Find_Lane_By_Id (Lane_Id)
        .Find_Stretch_Position (Stretch_Id       => Stretch_Id,
                                Stretch_Position => Stretch_Position,
                                Found            => Found);
   end Find_Stretch_Position;

   not overriding
   function Get_Stretch_By_Position (
      This             : in Lane.Utils.Object;
      Lane_Id          : in Infra_Id;
      Stretch_Position : in Natural)
   return Infra_Id
   is (This.District
           .Find_Lane_By_Id (Lane_Id)
           .Get_Stretch_By_Position (Stretch_Position));

   function Get_Direction (This    : in Lane.Utils.Object;
                           Lane_Id : in Infra_Id)
                           return Direction.Straight is
   begin
      return This.District
        .Find_Lane_By_Id (Lane_Id)
        .Get_Direction;
   end Get_Direction;

   procedure Add_Intersection (
      This            : in     Lane.Utils.Object;
      Lane_Id,
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean) is
   begin
      This.District
        .Find_Lane_By_Id (Lane_Id)
        .Add_Intersection (Intersection_Id => Intersection_Id,
                           Added           => Added);
   end Add_Intersection;

   function Is_Contained_By (This                  : in Lane.Utils.Object;
                             Lane_Id, Container_Id : in Infra_Id)
   return Boolean is
   begin
      return This.District
        .Find_Lane_By_Id (Lane_Id)
        .Is_Contained_By (Container_Id => Container_Id);
   end Is_Contained_By;

   procedure Attempt_Overtake (
      This                : in Lane.Utils.Object;
      Lane_Id             : in Infra_Id;
      Stretch_Id          : in Infra_Id;
      Max_Overtake_Length : in Natural;
      Traveller_Id        : in Agent.Agent_Id) is
   begin
      This.District.Find_Lane_By_Id (Lane_Id)
         .Attempt_Overtake (Stretch_Id, Max_Overtake_Length, Traveller_Id);
   end Attempt_Overtake;

   function Dump (This : in Lane.Utils.Object; Lane_Id : in Infra_Id)
   return G_JSON.JSON_Value is
   begin
      return This.District
                 .Find_Lane_By_Id (Lane_Id)
                 .Dump;
   end Dump;

end Reactive.Infrastructure.Lane.Utils;
