-- core
with Ada.Strings.Unbounded;
with Passive.Road_Sign.Speed_Limit;

with Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;
with Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
with Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;

with Shared.Agent_Id_To_Agent_Id_List_Map;
with Shared.Agent_Id_To_Infra_Id_List_Map;

package body Reactive.Infrastructure.Factory.Street_Factory is

   package SU    renames Ada.Strings.Unbounded;
   package Bicycle_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
   package Pedestrian_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
   package Stretch_Sign_Decorator
   renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;
   package Lane_Sign_Decorator
      renames Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;
   package Speed_Limit_Pkg
      renames Passive.Road_Sign.Speed_Limit;
   package Agent_Id_To_Agent_Id_List_Map
      renames Shared.Agent_Id_To_Agent_Id_List_Map;
   package Agent_Id_To_Infra_Id_List_Map
      renames Shared.Agent_Id_To_Infra_Id_List_Map;
   use Speed_Limit_Pkg;

   procedure Init (
      This        : in out Street_Factory.Object;
      District    : access Reactive.District.Object'Class := null) is
   begin
      This.District := District;
      if This.District = null then
         This.District := Reactive.District.Get_Instance;
      end if;
   end Init;

   function Decorate_Stretch (
      This        : in out Street_Factory.Object;
      Stretch_Ref : in     Stretch.Reference;
      Decoration  : in     G_JSON.JSON_Value)
   return Stretch.Reference
   is
      Decorated_Stretch  : Stretch.Reference := Stretch_Ref;
   -- bus stop variables
      Bus_Stops          : G_JSON.JSON_Array;
      Bus_Stop_JSON      : G_JSON.JSON_Value;
      Bus_Stop_Id        : Agent.Agent_Id;
      Bus_Stop_Ids       : Agent_Id_List.List;
      Bus_Stop_Wait      : Agent_Id_List.List;
      Bus_Stop_Wait_Map  : Agent_Id_To_Agent_Id_List_Map.Map;
      Bus_Stop_Stops     : Infra_Id_List.List;
      Bus_Stop_Stops_Map : Agent_Id_To_Infra_Id_List_Map.Map;
      Bus_Stop_Sign      : Passive.Road_Sign.Reference;
   -- host variables
      Host_Id_Int        : Integer;
      Travellers_JSON    : G_JSON.JSON_Array;
      Traveller_JSON     : G_JSON.JSON_Value;
      Traveller_SU       : SU.Unbounded_String;
      Traveller          : Agent.Agent_Id;
   -- json fields
      Pedestrian_Crossing_Field  : constant String := "pedestrianCrossing";
      Bicycle_Crossing_Field     : constant String := "bicycleCrossing";
      Facility_Field             : constant String := "facilityId";
      Id_Field                   : constant String := "id";
      Travellers_Field           : constant String := "travellers";
   begin
      if Decoration.Has_Field (Pedestrian_Crossing_Field) then
         Decorated_Stretch :=
            Stretch.Reference (Pedestrian_Crossing.Create (Decorated_Stretch));
      end if;

      if Decoration.Has_Field (Bicycle_Crossing_Field) then
         Decorated_Stretch :=
            Stretch.Reference (Bicycle_Crossing.Create (Decorated_Stretch));
      end if;

      if Decoration.Has_Field (Bus_Stop_Field) then
         Bus_Stops := Decoration.Get (Bus_Stop_Field);

      --+ Iterate through all bus ids in array
         for I in 1 .. G_JSON.Length (Bus_Stops) loop
            Bus_Stop_JSON  := G_JSON.Get (Bus_Stops, I);
            Bus_Stop_Id    := Get_Id_For_Bus (Bus_Stop_JSON);
            Bus_Stop_Wait  := Get_Waiting_List_For_Bus (Bus_Stop_JSON);
            Bus_Stop_Stops := Get_Stops_For_Bus (Bus_Stop_JSON);
            Bus_Stop_Ids.Append (Bus_Stop_Id);
            Bus_Stop_Wait_Map.Include (
               Key => Bus_Stop_Id, New_Item => Bus_Stop_Wait);
            Bus_Stop_Stops_Map.Include (
               Key => Bus_Stop_Id, New_Item => Bus_Stop_Stops);
         end loop;
      --+ Decorate stretch with bus stops list
         Bus_Stop_Sign := Passive.Road_Sign.Reference (
            Passive.Road_Sign.Bus_Stop.Create (
               Buses       => Bus_Stop_Ids,
               Waiting_Ids => Bus_Stop_Wait_Map,
               Stops       => Bus_Stop_Stops_Map));

         Decorated_Stretch := Stretch.Reference (
            Stretch_Sign_Decorator.Create (Decorated_Stretch, Bus_Stop_Sign));
      end if;

      if Decoration.Has_Field (Facility_Field) then
         Host_Id_Int := Decoration.Get (Facility_Field);
         Decorated_Stretch.Set_Host (Infra_Id (Host_Id_Int));
      end if;

      return Decorated_Stretch;
   end Decorate_Stretch;

   function Decorate_Lane (
      This       : in out Street_Factory.Object;
      Lane_Ref   : in     Reactive.Infrastructure.Lane.Reference;
      Decoration : in     G_JSON.JSON_Value)
   return Reactive.Infrastructure.Lane.Reference
   is
      Decorated_Lane    : Lane.Reference := Lane_Ref;
      Speed_Limit       : Natural;
      Speed_Limit_Sign  : Passive.Road_Sign.Reference;
   begin
      if Decoration.Has_Field (Speed_Limit_Field) then
         Speed_Limit := Decoration.Get (Speed_Limit_Pkg.Speed_Limit_Field);
         Speed_Limit_Sign := Passive.Road_Sign.Reference (
            Speed_Limit_Pkg.Create (Speed_Limit));
         Decorated_Lane := Lane.Reference (
            Lane_Sign_Decorator.Create (Decorated_Lane, Speed_Limit_Sign));
      end if;

      return Decorated_Lane;
   end Decorate_Lane;

   procedure Set_Stretch_Id (This : in out Street_Factory.Object;
                             Id   : in     Infra_Id) is
   begin
      This.Stretch_Id := Id;
   end Set_Stretch_Id;

   procedure Set_Stretch_Size (This : in out Street_Factory.Object;
                               Size : in     Natural) is
   begin
      This.Stretch_Size := Size;
   end Set_Stretch_Size;

   procedure Set_Stretch_Travellers (
      This       : in out Street_Factory.Object;
      Travellers : in     Agent_Id_List.List) is
   begin
      This.Stretch_Travellers := Travellers;
   end Set_Stretch_Travellers;

   procedure Set_Lane_Id (This : in out Street_Factory.Object;
                          Id   : in     Infra_Id) is
   begin
      This.Lane_Id := Id;
   end Set_Lane_Id;

   procedure Set_Lane_Direction (
      This      : in out Street_Factory.Object;
      Direction : in     Shared.Direction.Straight) is
   begin
      This.Lane_Direction := Direction;
   end Set_Lane_Direction;

   procedure Set_Lane_Stretches (
      This        : in out Street_Factory.Object;
      Stretches   : in     Infra_Id_List.List) is
   begin
      This.Lane_Stretches := Stretches;
   end Set_Lane_Stretches;

-- private

   function Get_Id_For_Bus (Bus_Stop_JSON : in G_JSON.JSON_Value)
   return Agent.Agent_Id is
      Bus_Stop_Int : Integer;
   begin
      Bus_Stop_Int := Bus_Stop_JSON.Get (Bus_Id_Field);
      return Agent.Create_Id_From_Natural (Bus_Stop_Int);
   end Get_Id_For_Bus;

   function Get_Waiting_List_For_Bus (Bus_Stop_JSON : G_JSON.JSON_Value)
   return Agent_Id_List.List
   is
      Waiting_JSON : G_JSON.JSON_Array;
      Waiting_SU   : SU.Unbounded_String;
      Waiting_List : Agent_Id_List.List := Agent_Id_List.Empty_List;
   begin
      Waiting_JSON := Bus_Stop_JSON.Get (Bus_Waiting_Field);
      for I in 1 .. G_JSON.Length (Waiting_JSON) loop
         Waiting_SU := G_JSON.Get (Waiting_JSON, I).Get;
         Waiting_List.Append (Waiting_SU);
      end loop;
      return Waiting_List;
   end Get_Waiting_List_For_Bus;

   function Get_Stops_For_Bus (Bus_Stop_JSON : G_JSON.JSON_Value)
   return Infra_Id_List.List
   is
      Stops_JSON : G_JSON.JSON_Array;
      Stops_Int  : Integer;
      Stops_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      Stops_JSON := Bus_Stop_JSON.Get (Stops_Field);
      for I in 1 .. G_JSON.Length (Stops_JSON) loop
         Stops_Int := G_JSON.Get (Stops_JSON, I).Get;
         Stops_List.Append (Infra_Id (Stops_Int));
      end loop;
      return Stops_List;
   end Get_Stops_For_Bus;

end Reactive.Infrastructure.Factory.Street_Factory;
