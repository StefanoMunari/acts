with Reactive.Infrastructure.Way.Roadway.Utils;
with Reactive.Infrastructure.Way.Footway.Utils;
with Reactive.Infrastructure.Way.Bikeway.Utils;

package body Reactive.Infrastructure.Street is

   package Roadway renames Reactive.Infrastructure.Way.Roadway;
   package Footway renames Reactive.Infrastructure.Way.Footway;
   package Bikeway renames Reactive.Infrastructure.Way.Bikeway;

   function Create (
      Id              : in     Infra_Id;
      Orientation     : in     Direction.Orientation;
      Roadway_Utils   : access Roadway.Utils.Object'Class := null;
      Footway_Utils   : access Footway.Utils.Object'Class := null;
      Bikeway_Utils   : access Bikeway.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Street.Reference
   is
      Street : Infrastructure.Street.Reference
        := new Infrastructure.Street.Object;
   begin
      Street.Id := Id;
      Street.Orientation := Orientation;
      if Roadway_Utils = null then
         Street.Roadway_Utils := Roadway.Utils.Get_Instance;
      else
         Street.Roadway_Utils := Roadway_Utils;
      end if;
      if Footway_Utils = null then
         Street.Footway_Utils := Footway.Utils.Get_Instance;
      else
         Street.Footway_Utils := Footway_Utils;
      end if;
      if Bikeway_Utils = null then
         Street.Bikeway_Utils := Bikeway.Utils.Get_Instance;
      else
         Street.Bikeway_Utils := Bikeway_Utils;
      end if;
      if Traveller_Utils = null then
         Street.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      else
         Street.Traveller_Utils := Traveller_Utils;
      end if;
      return Street;
   end Create;

   function Is_Treadable_In_Direction (
      This      : in Street.Object;
      Direction : Shared.Direction.Cardinal)
   return Boolean
   is
      Street_Orientation : Shared.Direction.Orientation
        := This.Get_Orientation;
      Directions_Compatible_With_Street_Orientation
      : Shared.Direction.Cardinal_Direction_List.List;
   begin
      Directions_Compatible_With_Street_Orientation
        := Shared.Direction.Find_Possible_Cardinal_Directions
            (Street_Orientation);
      return Directions_Compatible_With_Street_Orientation
        .Contains (Direction);
   end;

   function Is_Not_Treadable_In_Direction (
      This : in Street.Object;
      Direction : Shared.Direction.Cardinal)
   return Boolean is
   begin
      return not This.Is_Treadable_In_Direction (Direction);
   end Is_Not_Treadable_In_Direction;

   function Get_Id (This : in Street.Object) return Infra_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Orientation (This: in Street.Object)
                             return Direction.Orientation is
   begin
      return This.Orientation;
   end Get_Orientation;

   procedure Add_Roadway (This       :    out Street.Object;
                          Roadway_Id : in     Infra_Id) is
   begin
      This.Roadway_Ids.Append (Roadway_Id);
      This.Roadway_Utils.Set_Street (Roadway_Id, This.Id);
      This.Roadway_Utils.Validate (Roadway_Id);
   end Add_Roadway;

   procedure Add_Footway (This       :    out Street.Object;
                          Footway_Id : in     Infra_Id) is
   begin
      This.Footway_Ids.Append (Footway_Id);
      This.Footway_Utils.Set_Street (Footway_Id, This.Id);
      This.Footway_Utils.Validate (Footway_Id);
   end Add_Footway;

   procedure Add_Bikeway (This       :    out Street.Object;
                          Bikeway_Id : in     Infra_Id) is
   begin
      This.Bikeway_Ids.Append (Bikeway_Id);
      This.Bikeway_Utils.Set_Street (Bikeway_Id, This.Id);
      This.Bikeway_Utils.Validate (Bikeway_Id);
   end Add_Bikeway;

   function Get_Footways (This : Street.Object)
   return Infra_Id_List.List
   is
      Footways : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      for Footway of This.Footway_Ids loop
         Footways.Append (Footway);
      end loop;
      return Footways;
   end Get_Footways;

   function "=" (This, Outher : in Street.Object) return Boolean is
   begin
      return This.Id = Outher.Id;
   end "=";

   function Find_Street (This : in Street.Object)
                         return Infra_Id is
   begin
      return This.Id;
   end Find_Street;

   function Find_Lanes_By_Direction (
      This             : in Street.Object;
      Travel_Direction : in Direction.Straight)
   return Infra_Id_Set.Set is
      Lanes   : Infra_Id_Set.Set;
      Lane_Id : Infra_Id;
      Found   : Boolean := FALSE;
   begin

      if not This.Roadway_Ids.Is_Empty then
         This.Roadway_Utils.Find_Lane_By_Direction
           (Roadway_Id       => This.Roadway_Ids.First_Element,
            Travel_Direction => Travel_Direction,
            Lane_Id          => Lane_Id,
            Found            => Found);
         if Found then
            if not Lanes.Contains (Lane_Id) then
               Lanes.Insert (Lane_Id);
            end if;
         end if;
      end if;

      if not This.Footway_Ids.Is_Empty then
         This.Footway_Utils.Find_Lane_By_Direction
           (Footway_Id       => This.Footway_Ids.First_Element,
            Travel_Direction => Travel_Direction,
            Lane_Id          => Lane_Id,
            Found            => Found);
         if Found then
            if not Lanes.Contains (Lane_Id) then
               Lanes.Insert (Lane_Id);
            end if;
         end if;
      end if;

      if not This.Bikeway_Ids.Is_Empty then
         This.Bikeway_Utils.Find_Lane_By_Direction
           (Bikeway_Id       => This.Bikeway_Ids.First_Element,
            Travel_Direction => Travel_Direction,
            Lane_Id          => Lane_Id,
            Found            => Found);
         if Found then
            if not Lanes.Contains (Lane_Id) then
               Lanes.Insert (Lane_Id);
            end if;
         end if;
      end if;

      return Lanes;
   end Find_Lanes_By_Direction;

   function Is_Contained_By (This         : in Street.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      return FALSE;
   end Is_Contained_By;

   function Dump (This : Street.Object) return G_JSON.JSON_Value
   is
      JSON     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bikeways : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bikeway  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Footways : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Footway  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Roadways : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Roadway  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON.Set_Field (Id_Field, Integer (This.Id));
      JSON.Set_Field (Orientation_Field,
                      Shared.Direction.Orientation'Image (This.Orientation));

      for Bikeway_Id of This.Bikeway_Ids loop
         Bikeway := This.Bikeway_Utils.Dump (Bikeway_Id);
         G_JSON.Append (Bikeways, Bikeway);
      end loop;
      JSON.Set_Field (Bikeways_Field, Bikeways);

      for Footway_Id of This.Footway_Ids loop
         Footway := This.Footway_Utils.Dump (Footway_Id);
         G_JSON.Append (Footways, Footway);
      end loop;
      JSON.Set_Field (Footways_Field, Bikeways);

      for Roadway_Id of This.Roadway_Ids loop
         Roadway := This.Roadway_Utils.Dump (Roadway_Id);
         G_JSON.Append (Roadways, Roadway);
      end loop;
      JSON.Set_Field (Roadways_Field, Bikeways);

      return JSON;
   end Dump;

end Reactive.Infrastructure.Street;
