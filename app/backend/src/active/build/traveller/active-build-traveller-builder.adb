-- active build pkgs
with Active.Build.Traveller.Config_Reader;
-- travel related pkgs
with Active.Travel;
with Active.Travel.Bus_Travel;
with Active.Travel.Travel_Completed;
with Active.Travel.Travel_Planning;
with Active.Travel.Travel_Progress;
-- traveller related pkgs
with Active.People_Carrier;
with Active.Traveller.Pedestrian;
with Active.Traveller.Vehicle.Bicycle;
with Active.Traveller.Vehicle.Bus;

with Active.Build.Exceptions;
use Active.Build.Exceptions;

with Reactive.District;

package body Active.Build.Traveller.Builder is

-- Active pkgs
   package Travel_Pkg           renames Active.Travel;
   package Bus_Travel_Pkg       renames Active.Travel.Bus_Travel;
   package Travel_Completed_Pkg renames Travel_Pkg.Travel_Completed;
   package Travel_Planning_Pkg  renames Travel_Pkg.Travel_Planning;
   package Travel_Progress_Pkg  renames Travel_Pkg.Travel_Progress;
   package People_Carrier_Pkg   renames Active.People_Carrier;
   package Pedestrian_Pkg       renames Active.Traveller.Pedestrian;
   package Bicycle_Pkg          renames Active.Traveller.Vehicle.Bicycle;
   package Bus_Pkg              renames Active.Traveller.Vehicle.Bus;
-- use
   use Reactive.Stretch_Type_Package;

   function With_Id (
      This : in out Traveller.Builder.Object;
      Id   : in     Agent_Id)
   return Builder.Reference is
   begin
      This.Id := Id;
      return This'Unchecked_Access;
   end With_Id;

   function With_Src (
      This       : in out Traveller.Builder.Object;
      Slice_JSON : in     G_JSON.JSON_Value)
   return Builder.Reference
   is
      Foot_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      if not Slice_JSON.Has_Field (Foot_Type) then
         Raise_Missing_Field_For_Slice (Foot_Type);
      end if;
      Foot_List := Get_Stretches_For (Slice_JSON, Foot_Type);
      This.Travel_Source.Include (FOOT, Foot_List);

      if not Slice_JSON.Has_Field (Bike_Type) then
         Raise_Missing_Field_For_Slice (Bike_Type);
      end if;
      Bike_List := Get_Stretches_For (Slice_JSON, Bike_Type);
      This.Travel_Source.Include (BIKE, Bike_List);

      if not Slice_JSON.Has_Field (Road_Type) then
         Raise_Missing_Field_For_Slice (Bike_Type);
      end if;
      Road_List := Get_Stretches_For (Slice_JSON, Road_Type);
      This.Travel_Source.Include (ROAD, Road_List);

      return This'Unchecked_Access;
   end With_Src;

   function With_Dst (
      This       : in out Traveller.Builder.Object;
      Slice_JSON : in     G_JSON.JSON_Value)
   return Builder.Reference
   is
      Foot_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin

      if not Slice_JSON.Has_Field (Foot_Type) then
         Raise_Missing_Field_For_Slice (Foot_Type);
      end if;
      Foot_List := Get_Stretches_For (Slice_JSON, Foot_Type);
      This.Travel_Destination.Include (FOOT, Foot_List);

      if not Slice_JSON.Has_Field (Bike_Type) then
         Raise_Missing_Field_For_Slice (Bike_Type);
      end if;
      Bike_List := Get_Stretches_For (Slice_JSON, Bike_Type);
      This.Travel_Destination.Include (BIKE, Bike_List);

      if not Slice_JSON.Has_Field (Road_Type) then
         Raise_Missing_Field_For_Slice (Bike_Type);
      end if;
      Road_List := Get_Stretches_For (Slice_JSON, Road_Type);
      This.Travel_Destination.Include (ROAD, Road_List);

      return This'Unchecked_Access;
   end With_Dst;

   function With_Residual_Travel (
      This            : in out Traveller.Builder.Object;
      Residual_Travel : in     G_JSON.JSON_Array)
   return Builder.Reference
   is
      Step_Int : Integer;
      Step     : Infra_Id;
   begin
      for I in 1 .. G_JSON.Length (Residual_Travel) loop
         Step_Int := G_JSON.Get (Residual_Travel, I).Get;
         Step     := Infra_Id (Step_Int);
         This.Residual_Travel.Append (Step);
      end loop;

      return This'Unchecked_Access;
   end With_Residual_Travel;

   function With_Travel_State (
      This         : in out Traveller.Builder.Object;
      Travel_State : in     SU.Unbounded_String)
   return Builder.Reference is
   begin
      if Travel_State = SU.To_Unbounded_String (Completed_State) then
         This.Travel_State := Travel_State_Pkg.Reference(
            Travel_Completed_Pkg.Get_Instance);
      elsif Travel_State = SU.To_Unbounded_String (Planning_State) then
         This.Travel_State := Travel_State_Pkg.Reference(
            Travel_Planning_Pkg.Get_Instance);
      elsif Travel_State = SU.To_Unbounded_String (Progress_State) then
         This.Travel_State := Travel_State_Pkg.Reference(
            Travel_Progress_Pkg.Get_Instance);
      else
         Raise_Invalid_Field_For_Traveller (Config_Reader.Travel_State_Field);
      end if;

      return This'Unchecked_Access;
   end With_Travel_State;

   function With_Current_Speed (
      This  : in out Traveller.Builder.Object;
      Speed : in     Natural)
   return Builder.Reference is
   begin
      This.Current_Speed := Speed;
      return This'Unchecked_Access;
   end With_Current_Speed;

   function With_Max_Speed (
      This  : in out Traveller.Builder.Object;
      Speed : in     Natural)
   return Builder.Reference is
   begin
      This.Max_Speed := Speed;
      return This'Unchecked_Access;
   end With_Max_Speed;

   function With_Current_Position (
      This     : in out Traveller.Builder.Object;
      Position : in     Infra_Id)
   return Builder.Reference is
   begin
      This.Current_Position := Position;
      return This'Unchecked_Access;
   end With_Current_Position;

   function With_Passengers (
      This       : in out Traveller.Builder.Object;
      Passengers : in     G_JSON.JSON_Array)
   return Builder.Reference is
      Passenger_SU : SU.Unbounded_String;
      Passenger    : Agent_Id;
   begin
      for I in 1 .. G_JSON.Length (Passengers) loop
         Passenger_SU := G_JSON.Get (Passengers, I).Get;
         Passenger     := Agent_Id (Passenger_SU);
         This.Passengers.Append (Passenger);
      end loop;

      return This'Unchecked_Access;
   end With_Passengers;

   function With_Max_Passengers (
      This           : in out Traveller.Builder.Object;
      Max_Passengers : in     Natural)
   return Builder.Reference
   is
   begin
      This.Max_Passengers := Max_Passengers;
      return This'Unchecked_Access;
   end With_Max_Passengers;

   function With_Pvt_Type (
      This     : in out Traveller.Builder.Object;
      Pvt_Type : in     SU.Unbounded_String)
   return Builder.Reference is
   begin
      if SU.To_String (Pvt_Type) /= "" then
         This.Pvt_Type :=
            PVT_Pkg.Private_Motor_Vehicle_Type'Value (SU.To_String (Pvt_Type));
      end if;

      return This'Unchecked_Access;
   end With_Pvt_Type;

   function With_Bus_Stops (
      This  : in out Traveller.Builder.Object;
      Stops : in     G_JSON.JSON_Array)
   return Builder.Reference
   is
      Stop_Int : Integer;
      Stop     : Infra_Id;
   begin
      for I in 1 .. G_JSON.Length (Stops) loop
         Stop_Int := G_JSON.Get (Stops, I).Get;
         Stop     := Infra_Id (Stop_Int);
         This.Bus_Stops.Append (Stop);
      end loop;

      return This'Unchecked_Access;
   end With_Bus_Stops;

   function With_Route_Stops (
      This  : in out Traveller.Builder.Object;
      Stops : in     G_JSON.JSON_Array)
   return Builder.Reference
   is
      Stop_Int : Integer;
      Stop     : Infra_Id;
   begin
      for I in 1 .. G_JSON.Length (Stops) loop
         Stop_Int := G_JSON.Get (Stops, I).Get;
         Stop     := Infra_Id (Stop_Int);
         This.Route_Stops.Append (Stop);
      end loop;

      return This'Unchecked_Access;
   end With_Route_Stops;

   function With_Is_Waiting (
      This       : in out Traveller.Builder.Object;
      Is_Waiting : in     Boolean)
   return Builder.Reference is
   begin
      This.Is_Waiting := Is_Waiting;
      return This'Unchecked_Access;
   end With_Is_Waiting;

   function Get_Result (
      This   : in out Traveller.Builder.Object;
      T_Type : in     SU.Unbounded_String)
   return Agent_Id
   is
      Travel       : Travel_Pkg.Reference;
      Traveller    : Traveller_Pkg.Reference;
      Added        : Boolean := False;
   begin
      -- Create Travel
      if T_Type = SU.To_Unbounded_String (Bus_Type) then
         Travel :=
            Travel_Pkg.Reference(
               Bus_Travel_Pkg.Create (
                  This.Travel_Source, This.Travel_Destination,
                  This.Id, This.Travel_State)
            );
      else
         Travel :=
            Travel_Pkg.Create (
               This.Travel_Source, This.Travel_Destination,
               This.Id, This.Travel_State);
      end if;
      Travel.Set_Residual_Route (This.Residual_Travel);

      if T_Type = SU.To_Unbounded_String (Pedestrian_Type) then
         Traveller :=
            Traveller_Pkg.Reference (
               Pedestrian_Pkg.Create (
                  Id            => This.Id,
                  Maximum_Speed => This.Max_Speed,
                  Is_Waiting    => This.Is_Waiting,
                  Travel_Ref    => Travel));
      elsif T_Type = SU.To_Unbounded_String (Bicycle_Type) then
         Traveller :=
            Traveller_Pkg.Reference (
               Bicycle_Pkg.Create (
                  This.Id, This.Max_Speed, This.Max_Passengers, Travel));
         Put_Passengers (Traveller, This.Passengers);
      elsif T_Type = SU.To_Unbounded_String (Bus_Type) then
         Traveller :=
            Traveller_Pkg.Reference (
               Bus_Pkg.Create (
                  Id             => This.Id,
                  Maximum_Speed  => This.Max_Speed,
                  Max_Passengers => This.Max_Passengers,
                  Travel_Ref     => Travel,
                  Bus_Stops      => This.Bus_Stops,
                  Route_Stops    => This.Route_Stops));
         Put_Passengers (Traveller, This.Passengers);
      elsif T_Type = SU.To_Unbounded_String (Private_Motor_Type) then
         Traveller :=
            Traveller_Pkg.Reference (
               PVT_Pkg.Create (
                  This.Id, This.Max_Speed, This.Max_Passengers,
                  This.PVT_Type, Travel));
         Put_Passengers (Traveller, This.Passengers);
      else
         Raise_Invalid_Field_For_Traveller (Config_Reader.Travel_State_Field);
      end if;
      Traveller.Set_Current_Speed (This.Current_Speed);
      Traveller.Set_Position      (This.Current_Position);

      Reactive.District.Get_Instance.Add_Traveller (
         Traveller => Traveller, Added => Added);

      Travel.Add_AI (Traveller.Get_Stretch_Type);

      return This.Id;
   end Get_Result;

-- private part

   function Get_Stretches_For (
      Slice_JSON : in G_JSON.JSON_Value;
      Type_Field : in String)
   return Infra_Id_List.List
   is
      Stretch_List_JSON : G_JSON.JSON_Array;
      Stretch_Int       : Integer;
      Stretch           : Infra_Id;
      Stretch_List      : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      Stretch_List_JSON := Slice_JSON.Get (Type_Field);
      for I in 1 .. G_JSON.Length (Stretch_List_JSON) loop
         Stretch_Int := G_JSON.Get (Stretch_List_JSON, I).Get;
         Stretch     := Infra_Id (Stretch_Int);
         Stretch_List.Append (Stretch);
      end loop;
      return Stretch_List;
   end Get_Stretches_For;

   procedure Put_Passengers (
      Traveller  : Traveller_Pkg.Reference;
      Passengers : Agent_Id_List.List)
   is
      People_Carrier : People_Carrier_Pkg.Reference
         := People_Carrier_Pkg.Reference (Traveller);
      Boarded : Boolean := False;
   begin
      for Passenger of Passengers loop
         People_Carrier.Board (Passenger, Boarded);
         if not Boarded then
            Raise_Invalid_Field_For_Traveller (Config_Reader.Passengers_Field);
         end if;
      end loop;
   end Put_Passengers;

end Active.Build.Traveller.Builder;
