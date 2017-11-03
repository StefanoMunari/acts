
with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;

package body Reactive.Infrastructure_Registry is

   function Get_Instance return Infrastructure_Registry.Reference is
   begin
      if Instance = null then
         Instance := new Infrastructure_Registry.Object;
      end if;

      return Instance;
   end Get_Instance;

   function Contains_Infrastructure (
      This : in Infrastructure_Registry.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean
   is (This.Intersection_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Street_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Roadway_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Footway_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Bikeway_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Lane_Directory.Contains_Infrastructure (Infrastructure_Id))
      or
      (This.Stretch_Directory.Contains_Infrastructure (Infrastructure_Id));

   function Contains_Treadable (
      This         : in Infrastructure_Registry.Object;
      Treadable_Id : in Infra_Id)
   return Boolean
   is
      Intersection_Found : Boolean := False;
      Stretch_Found      : Boolean := False;
   begin
      if This.Intersection_Directory.Contains_Infrastructure (Treadable_Id)
      then
         return True;
      end if;
      if This.Stretch_Directory.Contains_Infrastructure (Treadable_Id)
      then
         return True;
      end if;
      return False;
   end Contains_Treadable;

   function Find_Infrastructure_By_Id (
      This : in Infrastructure_Registry.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference
   is
      Requested_Infrastructure : Infrastructure.Reference;
      Found : Boolean := FALSE;
   begin
      Requested_Infrastructure := Infrastructure.Reference
         (This.Street_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure := Infrastructure.Reference
         (This.Intersection_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure := Infrastructure.Reference
         (This.Roadway_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure :=
         Infrastructure.Reference(
            This.Footway_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure :=
         Infrastructure.Reference(
            This.Bikeway_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure :=
         Infrastructure.Reference(
            This.Lane_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Requested_Infrastructure :=
         Infrastructure.Reference(
            This.Stretch_Directory.Safe_Find_By_Id (Infrastructure_Id, Found));
      if(Found = True) then
         return Requested_Infrastructure;
      end if;

      Raise_Infrastructure_Missing_Exception (Infrastructure_Id);
      return null;
   end Find_Infrastructure_By_Id;

   function Find_Treadable_By_Id (
      This              : in Infrastructure_Registry.Object;
      Treadable_Id : in Infra_Id)
   return Treadable.Reference is
      Requested_Treadable : Treadable.Reference;
      Found               : Boolean := FALSE;
   begin
      Requested_Treadable :=
         Treadable.Reference (
            This.Intersection_Directory.Safe_Find_By_Id (
               Treadable_Id, Found));
      if(Found = True) then
         return Requested_Treadable;
      end if;

      Requested_Treadable :=
         Treadable.Reference(
            This.Stretch_Directory.Safe_Find_By_Id (Treadable_Id, Found));
      if(Found = True) then
         return Requested_Treadable;
      end if;

      return null;
   end Find_Treadable_By_Id;

   function Find_Intersection_By_Id (
      This            : in Infrastructure_Registry.Object;
      Intersection_Id : in Infra_Id)
   return Intersection.Reference
   is (This.Intersection_Directory.Find_By_Id (Intersection_Id));

   function Find_Street_By_Id (
      This : in Infrastructure_Registry.Object;
      Street_Id : in Infra_Id)
   return Street.Reference
   is (This.Street_Directory.Find_By_Id (Street_Id));

   function Find_Way_By_Id (
      This : in Infrastructure_Registry.Object;
      Way_Id : in Infra_Id)
   return Way.Reference
   is
      Requested_Way : Way.Reference;
      Present : Boolean := FALSE;
   begin
      Requested_Way :=
         Way.Reference(This.Roadway_Directory.Safe_Find_By_Id (Way_Id, Present));
      if(Present = True) then
         return Requested_Way;
      end if;
      Requested_Way :=
         Way.Reference(This.Footway_Directory.Safe_Find_By_Id (Way_Id, Present));
      if(Present = True) then
         return Requested_Way;
      end if;
      Requested_Way :=
         Way.Reference(This.Bikeway_Directory.Safe_Find_By_Id (Way_Id, Present));
      if(Present = True) then
         return Requested_Way;
      end if;
      Raise_Way_Missing_Exception (Way_Id);
      return null;
   end Find_Way_By_Id;

   function Find_Roadway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Roadway_Id : in Infra_Id)
   return Roadway.Reference
   is (This.Roadway_Directory.Find_By_Id (Roadway_Id));

   function Find_Footway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Footway_Id : in Infra_Id)
   return Footway.Reference
   is (This.Footway_Directory.Find_By_Id (Footway_Id));

   function Find_Bikeway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Bikeway_Id : in Infra_Id)
   return Bikeway.Reference
   is (This.Bikeway_Directory.Find_By_Id (Bikeway_Id));

   function Find_Lane_By_Id (
      This    : in Infrastructure_Registry.Object;
      Lane_Id : in Infra_Id)
   return Lane.Reference
   is (This.Lane_Directory.Find_By_Id (Lane_Id));

   function Find_Stretch_By_Id (
      This       : in Infrastructure_Registry.Object;
      Stretch_Id : in Infra_Id)
   return Stretch.Reference
   is (This.Stretch_Directory.Find_By_Id (Stretch_Id));

   procedure Add_Intersection (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean) is
   begin
      This.Intersection_Directory.Add
            (Intersection.Object'Class (Infrastructure),
             Added => Added);
   end Add_Intersection;

   procedure Add_Street (
      This      : in out Infrastructure_Registry.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean) is
   begin
      This.Street_Directory.Add (SR_Street, Added => Added);
   end Add_Street;

   procedure Add_Roadway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean) is
   begin
      This.Roadway_Directory.Add (Infrastructure, Added => Added);
   end Add_Roadway;

   procedure Add_Footway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean) is
   begin
      This.Footway_Directory.Add (Infrastructure, Added => Added);
   end Add_Footway;

   procedure Add_Bikeway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean) is
   begin
      This.Bikeway_Directory.Add (Infrastructure, Added => Added);
   end Add_Bikeway;

   procedure Add_Lane (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean) is
   begin
      This.Lane_Directory.Add (Infrastructure, Added => Added);
   end Add_Lane;

   procedure Add_Stretch (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          : out Boolean) is
   begin
      This.Stretch_Directory.Add (Infrastructure, Added => Added);
   end Add_Stretch;

   procedure Clear (This : in out Infrastructure_Registry.Object) is
   begin
      This.Intersection_Directory.Clear;
      This.Street_Directory.Clear;
      This.Roadway_Directory.Clear;
      This.Footway_Directory.Clear;
      This.Bikeway_Directory.Clear;
      This.Lane_Directory.Clear;
      This.Stretch_Directory.Clear;
   end Clear;

   not overriding
   function Dump (This : in Infrastructure_Registry.Object)
   return G_JSON.JSON_Value
   is
      JSON              : G_JSON.JSON_Value := G_JSON.Create_Object;
      Streets_JSON      : G_JSON.JSON_Value;
      Intersection_JSON : G_JSON.JSON_Value;
   begin
      Streets_JSON := This.Street_Directory.Dump;
      JSON.Set_Field ("streets", Streets_JSON);
      Intersection_JSON := This.Intersection_Directory.Dump;
      JSON.Set_Field ("intersections", Intersection_JSON);
      return JSON;
   end Dump;

end Reactive.Infrastructure_Registry;
