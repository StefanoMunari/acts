with Reactive.Infrastructure.Exceptions;
with Reactive.Infrastructure.Intersection.Crossing;

use Reactive.Infrastructure.Exceptions;

package body Reactive.Infrastructure.Intersection.Intersection_Builder is

   procedure Init (
      Builder            : in out Intersection_Builder.Object'Class;
      Intersection_Id    : in     Infra_Id;
      Intersection_Type  : in     Intersection.Intersection_Type;
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
         access Infrastructure.Intersection.Crossing.Object'Class := null)
   is
      Strategy : access Infrastructure.Intersection.Crossing.Object'Class
         := Crossing_Strategy;
   begin
      if District = null then
         Builder.District := Reactive.District.Get_Instance;
      else
         Builder.District := District;
      end if;

      if Street_Utils = null then
         Builder.Street_Utils := Street.Utils.Get_Instance;
      else
         Builder.Street_Utils := Street_Utils;
      end if;

      if Intersection_Utils = null then
         Builder.Intersection_Utils :=
            Infrastructure.Intersection.Utils.Get_Instance;
      else
         Builder.Intersection_Utils := Intersection_Utils;
      end if;

      if Lane_Utils = null then
         Builder.Lane_Utils := Lane.Utils.Get_Instance;
      else
         Builder.Lane_Utils := Lane_Utils;
      end if;

      if Intersection = null then
         Builder.Intersection := new Infrastructure.Intersection.Object;
      else
         Builder.Intersection := Intersection;
      end if;

      Builder.Intersection.Set_Id (Intersection_Id);
      Builder.Intersection.Set_Intersection_Type (Intersection_Type);

      if Traveller_Utils = null then
         Builder.Intersection
           .Set_Traveller_Utils (Active.Traveller.Utils.Get_Instance);
      else
         Builder.Intersection.Set_Traveller_Utils (Traveller_Utils);
      end if;

      if Infrastructure_Utils = null then
         Builder.Intersection
           .Set_Infrastructure_Utils (
               Reactive.Infrastructure.Utils.Get_Instance);
      else
         Builder.Intersection.Set_Infrastructure_Utils (Infrastructure_Utils);
      end if;

      if Strategy = null then
         Strategy := new Infrastructure.Intersection.Crossing.Object;
      end if;
      Builder.Intersection.Set_Crossing_Strategy (Strategy);
   end Init;

   procedure With_Street (This      : in out Intersection_Builder.Object;
                          Street_Id : in     Infra_Id;
                          Stretches : in     Infra_Id_List.List;
                          Direction : in     Shared.Direction.Cardinal)
   is
   begin
      This.Intersection.Connect_Street(Street_Id => Street_Id,
                                       Stretches => Stretches,
                                       Direction => Direction);
      This.Intersection.Increment_Streets;
   end With_Street;

   procedure With_Traffic_Light (
      This           : in out Intersection_Builder.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal) is
   begin -- With_Traffic_Light
      This.Intersection.Crossing_Strategy.Set_Traffic_Light (
         Traffic_Light, Exit_Direction);

      This.District
          .Find_Traffic_Light_By_Id (Traffic_Light)
          .Set_Intersection_Id (This.Intersection.Get_Id);
   end With_Traffic_Light;

   function Get_Result (This : in Intersection_Builder.Object)
   return Infra_Id
   is
      Streets_Count     : Natural;
      Intersection_Id   : Infra_Id := This.Intersection.Get_Id;
      Intersection_Type : Intersection.Intersection_Type;
      Added             : Boolean;
   begin
      if This.Intersection.Is_Not_Fully_Connected then
         Streets_Count     := This.Intersection.Count_Streets;
         Intersection_Type := This.Intersection.Get_Intersection_Type;
         Raise_Few_Streets_Connected_With_Exception (
            Intersection_Id       => Intersection_Id,
            Streets_Count         => Streets_Count,
            Intersection_Type     => Intersection_Type,
            Intersection_Ways_Num => This.Intersection.Get_Size);
      end if;

      This.District.Add_Intersection (This.Intersection.all, Added);

      return Intersection_Id;
   end Get_Result;

end Reactive.Infrastructure.Intersection.Intersection_Builder;
