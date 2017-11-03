-- core
with Ada.Strings.Unbounded;

with Active.Agent;

with Reactive.Infrastructure.Factory.Street_Factory.Bikeway_Factory;
with Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory;
with Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory;
with Reactive.Infrastructure.Lane;
with Reactive.Infrastructure.Street;
with Reactive.Infrastructure.Stretch;
with Reactive.Infrastructure.Way;

with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

with Shared.Agent_Id_List;
with Shared.Direction;
with Shared.Shared_References_Street;

package body Reactive.Infrastructure.Build.Street_Builder is

   package SU            renames Ada.Strings.Unbounded;
   package Agent         renames Active.Agent;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Street_Pkg    renames Reactive.Infrastructure.Street;
   use Street_Pkg;
   use Way_Pkg;
   package Lane_Pkg      renames Reactive.Infrastructure.Lane;
   use Lane_Pkg;
   package Stretch_Pkg   renames Reactive.Infrastructure.Stretch;
   use Stretch_Pkg;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   function Create (
      District        : access Reactive.District.Object'Class := null;
      Bikeway_Factory : access Street_Factory.Object'Class    := null;
      Footway_Factory : access Street_Factory.Object'Class    := null;
      Roadway_Factory : access Street_Factory.Object'Class    := null;
      Roadway_Utils   : access Roadway_Pkg.Utils.Object'Class := null;
      Footway_Utils   : access Footway_Pkg.Utils.Object'Class := null;
      Bikeway_Utils   : access Bikeway_Pkg.Utils.Object'Class := null)
   return Street_Builder.Reference is
      Instance : Street_Builder.Reference := new Street_Builder.Object;
   begin
      Instance.District := District;
      if Instance.District = null then
         Instance.District := Reactive.District.Get_Instance;
      end if;

      Instance.Bikeway_Factory := Bikeway_Factory;
      if Instance.Bikeway_Factory = null then
         Instance.Bikeway_Factory
            := new Reactive.Infrastructure.Factory.Street_Factory.Bikeway_Factory.Object;
         Instance.Bikeway_Factory.Init;
      end if;

      Instance.Footway_Factory := Footway_Factory;
      if Instance.Footway_Factory = null then
         Instance.Footway_Factory
            := new Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory.Object;
         Instance.Footway_Factory.Init;
      end if;

      Instance.Roadway_Factory := Roadway_Factory;
      if Instance.Roadway_Factory = null then
         Instance.Roadway_Factory
            := new Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory.Object;
         Instance.Roadway_Factory.Init;
      end if;

      Instance.Bikeway_Utils := Bikeway_Utils;
      if Instance.Bikeway_Utils = null then
         Instance.Bikeway_Utils := Bikeway_Pkg.Utils.Get_Instance;
      end if;

      Instance.Footway_Utils := Footway_Utils;
      if Instance.Footway_Utils = null then
         Instance.Footway_Utils := Footway_Pkg.Utils.Get_Instance;
      end if;

      Instance.Roadway_Utils := Roadway_Utils;
      if Instance.Roadway_Utils = null then
         Instance.Roadway_Utils := Roadway_Pkg.Utils.Get_Instance;
      end if;

      return Instance;
   end Create;

   procedure Reset (This : in out Street_Builder.Object)
   is
   begin
      This.Bikeways := Infra_Id_List.Empty_List;
      This.Footways := Infra_Id_List.Empty_List;
      This.Roadways := Infra_Id_List.Empty_List;
   end Reset;

   function With_Bikeway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id is
      Bikeway     : aliased Reactive.Infrastructure.Way.Bikeway.Reference;
      Way_Id_Int  : Integer;
      Way_Id      : Infra_Id;
      Lanes       : Infra_Id_List.List;
      Added       : Boolean := False;
   begin
      if not JSON.Has_Field (Way_Pkg.Id_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Id_Field);
      end if;
      Way_Id_Int := JSON.Get (Way_Pkg.Id_Field);
      Way_Id     := Infra_Id (Natural ( Way_Id_Int));

      if not JSON.Has_Field (Way_Pkg.Lanes_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Lanes_Field);
      end if;

      Lanes      :=
         This.Get_Lanes (JSON.Get (Way_Pkg.Lanes_Field), This.Bikeway_Factory);

      Bikeway := Reactive.Infrastructure.Way.Bikeway.Create (Way_Id);

      for Lane of Lanes loop
         Bikeway.Add_Lane (Lane, Added);
      end loop;

      This.District.Add_Bikeway (Bikeway, Added);

      This.Bikeways.Append (Way_Id);
      return Way_Id;
   end With_Bikeway;

   function With_Footway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id is
      Footway     : aliased Reactive.Infrastructure.Way.Footway.Reference;
      Way_Id_Int  : Integer;
      Way_Id      : Infra_Id;
      Lanes       : Infra_Id_List.List;
      Added       : Boolean;
   begin
      if not JSON.Has_Field (Way_Pkg.Id_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Id_Field);
      end if;

      Way_Id_Int := JSON.Get (Way_Pkg.Id_Field);
      Way_Id     := Infra_Id (Natural ( Way_Id_Int));

      if not JSON.Has_Field (Way_Pkg.Lanes_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Lanes_Field);
      end if;
      Lanes      :=
         This.Get_Lanes (JSON.Get (Way_Pkg.Lanes_Field), This.Footway_Factory);
      Footway := Reactive.Infrastructure.Way.Footway.Create (Way_Id);
      for Lane of Lanes loop
         Footway.Add_Lane (Lane, Added);
      end loop;
      This.District.Add_Footway (Footway, Added);
      This.Footways.Append (Way_Id);

      return Way_Id;
   end With_Footway;

   function With_Roadway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id is
      Roadway     : aliased Reactive.Infrastructure.Way.Roadway.Reference;
      Way_Id_Int  : Integer;
      Way_Id      : Infra_Id;
      Lanes       : Infra_Id_List.List;
      Added       : Boolean;
   begin
      if not JSON.Has_Field (Way_Pkg.Id_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Id_Field);
      end if;
      Way_Id_Int := JSON.Get (Way_Pkg.Id_Field);
      Way_Id     := Infra_Id (Natural ( Way_Id_Int));

      if not JSON.Has_Field (Way_Pkg.Lanes_Field) then
         Raise_Missing_Field_For_Way (Way_Pkg.Lanes_Field);
      end if;
      Lanes      :=
         This.Get_Lanes (JSON.Get (Way_Pkg.Lanes_Field), This.Roadway_Factory);

      Roadway := Reactive.Infrastructure.Way.Roadway.Create (Way_Id);
      for Lane of Lanes loop
         Roadway.Add_Lane (Lane, Added);
      end loop;
      This.District.Add_Roadway (Roadway, Added);
      This.Roadways.Append (Way_Id);

      return Way_Id;
   end With_Roadway;

   function Get_Street (This : in out Street_Builder.Object;
                        JSON : in     G_JSON.JSON_Value)
   return Infra_Id
   is
      SR_Street             : SR_Street_Pkg.Shared_Reference;
      Street                : aliased Street_Pkg.Reference;
      Street_Id_Int         : Integer;
      Street_Id             : Infra_Id;
      Street_Orientation_SU : SU.Unbounded_String;
      Street_Orientation    : Shared.Direction.Orientation;
      Added                 : Boolean;
   begin
      if not JSON.Has_Field (Street_Pkg.Id_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Id_Field);
      end if;
      Street_Id_Int := JSON.Get (Street_Pkg.Id_Field);
      Street_Id     := Infra_Id (Natural (Street_Id_Int));

      if not JSON.Has_Field (Street_Pkg.Orientation_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Orientation_Field);
      end if;
      Street_Orientation_SU := JSON.Get (Street_Pkg.Orientation_Field).Get;
      Street_Orientation :=
         Shared.Direction.Orientation'Value (
            SU.To_String (Street_Orientation_SU));

      Street := Reactive.Infrastructure.Street.Create (
         Id            => Street_Id,
         Orientation   => Street_Orientation,
         Bikeway_Utils => This.Bikeway_Utils,
         Footway_Utils => This.Footway_Utils,
         Roadway_Utils => This.Roadway_Utils);
      -- add Street, Way, Lane (the next one needs the previous one)
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street));
      This.District.Add_Street (SR_Street, Added);

      for Bikeway of This.Bikeways loop
         Street.Add_Bikeway (Bikeway);
      end loop;
      for Footway of This.Footways loop
         Street.Add_Footway (Footway);
      end loop;
      for Roadway of This.Roadways loop
         Street.Add_Roadway (Roadway);
      end loop;

      return Street_Id;
   end Get_Street;

--------------------
-- private functions
--------------------

   function Get_Stretches (This      : in out Street_Builder.Object;
                           Stretches : in     G_JSON.JSON_Array;
                           Factory   :        Street_Factory.Reference;
                           Lane_Id   : in     Infra_Id)
   return Infra_Id_List.List is
   use Street_Factory;
      Stretch             : G_JSON.JSON_Value;
      Stretch_Id_Int      : Integer;
      Stretch_Id          : Infra_Id;
      Stretch_Size        : Integer;
      Travellers_JSON     : G_JSON.JSON_Array;
      Traveller_JSON      : G_JSON.JSON_Value;
      Traveller_Int       : Integer;
      Traveller           : Agent.Agent_Id;
      Booker_SU           : SU.Unbounded_String;
      Booker              : Agent.Agent_Id;
      Booked              : Boolean;
      Stretch_Travellers  : Agent_Id_List.List;
      Number_Of_Stretches : Natural;
      Result              : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Added               : Boolean;
   begin -- Get_Stretches
      Number_Of_Stretches := G_JSON.Length (Stretches);
      for I in 1 .. Number_Of_Stretches loop
         Stretch := G_JSON.Get (Stretches, I);
         if not Stretch.Has_Field (Stretch_Pkg.Id_Field) then
            Raise_Missing_Field_For_Stretch (Stretch_Pkg.Id_Field);
         end if;
         Stretch_Id_Int := Stretch.Get (Stretch_Pkg.Id_Field).Get;
         Stretch_Id := Infra_Id (Natural (Stretch_Id_Int));

         Factory.Set_Stretch_Id (Stretch_Id);
         if not Stretch.Has_Field (Stretch_Pkg.Size_Field) then
            Raise_Missing_Field_For_Stretch (Stretch_Pkg.Size_Field);
         end if;
         Stretch_Size := Stretch.Get (Stretch_Pkg.Size_Field).Get;
         Factory.Set_Stretch_Size (Stretch_Size);

         if not Stretch.Has_Field (Stretch_Pkg.Travellers_Field) then
            Raise_Missing_Field_For_Stretch (
               Stretch_Pkg.Travellers_Field);
         end if;
         Travellers_JSON := Stretch.Get (Stretch_Pkg.Travellers_Field).Get;
         for J in 1 .. G_JSON.Length (Travellers_JSON) loop
            Traveller_Int := G_JSON.Get (Travellers_JSON, J).Get;
            Traveller     := Agent.Create_Id_From_Natural (Traveller_Int);
            Stretch_Travellers.Append (Traveller);
         end loop;
         Factory.Set_Stretch_Travellers (Stretch_Travellers);

         Result.Append (Stretch_Id);
         declare
            Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference;
         begin
            Stretch_Ref := Factory.Create_Stretch;
            Stretch_Ref.Set_Lane (Lane_Id);
            if Stretch.Has_Field (Stretch_Pkg.Overtake_Field) then
               Booker_SU := Stretch.Get (Stretch_Pkg.Overtake_Field);
               Booker := Agent.Agent_Id (Booker_SU);
               Booked := Stretch_Ref.Book (Booker);
            end if;
            if Stretch.Has_Field (Stretch_Pkg.Decorations_Field) then
               Stretch_Ref :=
                  Factory.Decorate_Stretch (
                     Stretch_Ref,
                     Stretch.Get (Stretch_Pkg.Decorations_Field));
            end if;
            This.District.Add_Stretch (Stretch_Ref, Added);
         end;
      end loop;

      return Result;
   end Get_Stretches;

   function Get_Lanes (This    : in out Street_Builder.Object;
                       Lanes   : in     G_JSON.JSON_Array;
                       Factory :        Street_Factory.Reference)
   return Infra_Id_List.List is
   use Street_Factory;
      Lane              : G_JSON.JSON_Value;
      Lane_Id_Int       : Integer;
      Lane_Id           : Infra_Id;
      Lane_Direction_SU : SU.Unbounded_String;
      Lane_Direction    : Shared.Direction.Straight;
      Stretches_JSON    : G_JSON.JSON_Array;
      Stretches         : Infra_Id_List.List;
      Number_Of_Lanes   : Natural;
      Result            : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Added             : Boolean;
   begin -- Get_Lanes
      Number_Of_Lanes := G_JSON.Length (Lanes);
      for I in 1 .. Number_Of_Lanes loop
         Lane := G_JSON.Get (Lanes, I);
         if not Lane.Has_Field (Lane_Pkg.Id_Field) then
            Raise_Missing_Field_For_Lane (Lane_Pkg.Id_Field);
         end if;
         Lane_Id_Int := Lane.Get (Lane_Pkg.Id_Field).Get;
         Lane_Id := Infra_Id (Natural (Lane_Id_Int));

         Factory.Set_Lane_Id (Lane_Id);
         if not Lane.Has_Field (Lane_Pkg.Direction_Field) then
            Raise_Missing_Field_For_Lane (Lane_Pkg.Direction_Field);
         end if;

         Lane_Direction_SU := Lane.Get (Lane_Pkg.Direction_Field).Get;
         Lane_Direction :=
            Shared.Direction.Straight'Value (SU.To_String (Lane_Direction_SU));
         Factory.Set_Lane_Direction (Lane_Direction);
         if not Lane.Has_Field (Lane_Pkg.Stretches_Field) then
            Raise_Missing_Field_For_Lane (Lane_Pkg.Stretches_Field);
         end if;
         Stretches_JSON := Lane.Get (Lane_Pkg.Stretches_Field).Get;
         Stretches := This.Get_Stretches (Stretches_JSON, Factory, Lane_Id);
         Factory.Set_Lane_Stretches (Stretches);

         Result.Append (Lane_Id);
         declare
            Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference;
         begin
            Lane_Ref := Factory.Create_Lane;
            if Lane.Has_Field (Lane_Pkg.Decorations_Field) then
               Lane_Ref :=
                  Factory.Decorate_Lane (
                     Lane_Ref, Lane.Get (Lane_Pkg.Decorations_Field));
            end if;
            for Stretch of Stretches loop
               Lane_Ref.Append_Stretch (Stretch, Added);
            end loop;
            This.District.Add_Lane (Lane_Ref, Added);
         end;
      end loop;

      return Result;
   end Get_Lanes;

end Reactive.Infrastructure.Build.Street_Builder;
