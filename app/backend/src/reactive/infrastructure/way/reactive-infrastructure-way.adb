with Active.Traveller.Exceptions;

with Reactive.Infrastructure.Street.Utils;
with Reactive.Infrastructure.Lane.Utils;
with Reactive.Infrastructure.Exceptions;

use Reactive.Infrastructure.Exceptions;
use Active.Traveller.Exceptions;

package body Reactive.Infrastructure.Way is

   procedure Init (
      Way : in out Infrastructure.Way.Object'Class;
      Id  : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null) is
   begin
      Way.Id := Id;

      if Traveller_Utils = null then
         Way.Traveller_Utils := Traveller.Utils.Get_Instance;
      else
         Way.Traveller_Utils := Traveller_Utils;
      end if;

      if Street_Utils = null then
         Way.Street_Utils := Street.Utils.Get_Instance;
      else
         Way.Street_Utils := Street_Utils;
      end if;

      if Lane_Utils = null then
         Way.Lane_Utils := Lane.Utils.Get_Instance;
      else
         Way.Lane_Utils := Lane_Utils;
      end if;
   end Init;

   procedure Set_Street (This      :    out Way.Object;
                         Street_Id : in     Infra_Id) is
   begin
      This.Street_Id := Street_Id;
   end Set_Street;

   procedure Add_Lane (This    : in out Way.Object;
                       Lane_Id : in     Infra_Id;
                       Added   :    out Boolean)
   is
   begin
      Added := FALSE;
      If not This.Direct_Lane_Existence then
         This.Direct_Lane           := Lane_Id;
         This.Direct_Lane_Existence := TRUE;
         Added := TRUE;
      else
         if not This.Inverse_Lane_Existence
           and Direction."="(
                  Direction.Get_Inverse_Direction (
                     This.Lane_Utils.Get_Direction (
                        Lane_Id => This.Direct_Lane)),
                  This.Lane_Utils.Get_Direction (Lane_Id => Lane_Id)
               )
         then
            This.Inverse_Lane           := Lane_Id;
            This.Inverse_Lane_Existence := TRUE;
            Added := TRUE;
         end if;
      end if;
   end Add_Lane;

   not overriding
   procedure Validate (This : in out Way.Object)
   is
      Lanes                 : Infra_Id_List.List;
      Lane_Travel_Direction : Direction.Straight;
      Street_Id             : Infra_Id := This.Find_Street;
      Street_Orientation    : Direction.Orientation;
      Directions_Compatible_With_Street_Orientation :
         Direction.Straight_Direction_List.List;
   begin
      if This.Direct_Lane_Existence then
         Lanes.Append (This.Direct_Lane);
      end if;
      if This.Inverse_Lane_Existence then
         Lanes.Append (This.Inverse_Lane);
      end if;
      for Lane_Id of Lanes loop
         Lane_Travel_Direction :=
            This.Lane_Utils.Get_Direction (Lane_Id => Lane_Id);
         Street_Orientation :=
            This.Street_Utils.Get_Orientation (Street_Id => Street_Id);
         Directions_Compatible_With_Street_Orientation
            := Direction.Find_Possible_Straight_Directions (
               Street_Orientation);
         if not Directions_Compatible_With_Street_Orientation
                  .Contains (Lane_Travel_Direction)
         then
            Raise_Wrong_Travel_Direction_Exception (
               Lane_Id               => Lane_Id,
               Lane_Travel_Direction => Lane_Travel_Direction,
               Street_Id             => Street_Id,
               Street_Orientation    =>
                  This.Street_Utils.Get_Orientation (Street_Id => Street_Id));
         end if;
      end loop;
   end Validate;

   procedure Find_Lane_By_Direction (
      This             : in     Way.Object;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean)
   is
   begin
      Found := FALSE;

      if This.Direct_Lane_Existence then

         if Direction."="(
            This.Lane_Utils.Get_Direction (Lane_Id => This.Direct_Lane),
            Travel_Direction)
         then
            Lane_Id := This.Direct_Lane;
            Found := TRUE;
         end if;
      end if;

      if not Found then

         if This.Inverse_Lane_Existence then

            if Direction."="(This.Lane_Utils.Get_Direction
                             (Lane_Id => This.Inverse_Lane),
                             Travel_Direction)
            then
               Lane_Id := This.Inverse_Lane;
               Found := TRUE;
            end if;

         end if;
      end if;
   end Find_Lane_By_Direction;

   function Find_Street (This : in Way.Object)
                         return Infra_Id is
   begin
      return This.Street_Id;
   end Find_Street;

   function Get_Id (This : in Way.Object) return Infra_Id is
   begin
      return This.Id;
   end Get_Id;

   function Is_Contained_By (This         : in Way.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if This.Street_Id = Container_Id then
         return TRUE;
      end if;
      return This.Street_Utils.Is_Contained_By (Street_Id    => This.Street_Id,
                                                Container_Id => Container_Id);
   end Is_Contained_By;

   function Get_Stretches_On_Other_Lane (
      This         : Way.Object;
      Lane_Id      : Infra_Id;
      Stretch_List : Natural_List.List)
   return Infra_Id_List.List
   is
      Stretch_Id_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Other_Lane      : Infra_Id;
   begin
      if (not This.Direct_Lane_Existence)
         or
         (not This.Inverse_Lane_Existence)
      then
         return Stretch_Id_List;
      end if;

      if Lane_Id = This.Direct_Lane then
         Other_Lane := This.Inverse_Lane;
      else
         Other_Lane := This.Direct_Lane;
      end if;

      for Stretch of Stretch_List loop
         Stretch_Id_List.Append (
            This.Lane_Utils.Get_Stretch_By_Position (Other_Lane, Stretch));
      end loop;

      return Stretch_Id_List;
   end Get_Stretches_On_Other_Lane;

   function Dump (This : Way.Object) return G_JSON.JSON_Value
   is
      JSON         : G_JSON.JSON_Value := G_JSON.Create_Object;
      Lanes        : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Direct_Lane  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Inverse_Lane : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON.Set_Field (Id_Field, Integer (This.Id));

      if This.Direct_Lane_Existence then
         Direct_Lane := This.Lane_Utils.Dump (This.Direct_Lane);
         G_JSON.Append (Lanes, Direct_Lane);
      end if;

      if This.Inverse_Lane_Existence then
         Inverse_Lane := This.Lane_Utils.Dump (This.Inverse_Lane);
         G_JSON.Append (Lanes, Inverse_Lane);
      end if;

      JSON.Set_Field (Lanes_Field, Lanes);

      return JSON;
   end Dump;

   protected body Protected_Object is

      function Find_Last_Traveller_Lane (Traveller_Id : in Agent.Agent_Id)
      return Infra_Id
      is (Travellers.Element (Traveller_Id));

      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean
      is (Travellers.Contains (Key => Traveller_Id));

   end Protected_Object;

end Reactive.Infrastructure.Way;
