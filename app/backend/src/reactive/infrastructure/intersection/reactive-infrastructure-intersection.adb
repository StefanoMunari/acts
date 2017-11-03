with Ada.Assertions;

with Reactive.Infrastructure.Exceptions;
with Reactive.Infrastructure.Intersection.Crossing;

use Reactive.Infrastructure.Exceptions;

package body Reactive.Infrastructure.Intersection is

   use Agent;

   T_JUNCTION_WAYS_NUM : Natural := 3;
   CROSSROADS_WAYS_NUM : Natural := 4;

   procedure Tread (
      This         : in out Intersection.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean)
   is separate;

   procedure Leave (
      This         : in out Intersection.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean)
   is separate;

   function Get_Street (This      : in     Intersection.Object;
                        Direction : in out Shared.Direction.Cardinal)
      return Infra_Id
   is separate;

   procedure Find_Street_Direction (
      This             :        Intersection.Object;
      Street_Id        : in     Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean)
   is separate;

   function Find_Streets_Connected_With_Intersection (
      This : in Intersection.Object) return Infra_Id_Set.Set
   is separate;

   function Get_Id (This : in Intersection.Object) return Infra_Id
   is (This.Id);

   function Get_Intersection_Type (This : in Intersection.Object)
      return Intersection_Type
   is (This.Intersection_Type);

   function Find_Intersections (This : in Intersection.Object)
      return Infra_Id_Set.Set
   is separate;

   function Count_Streets (This : in Intersection.Object) return Natural is
   begin
      return This.Streets_Count;
   end Count_Streets;

   function Exists_Street_For_Direction (
      This      : in Intersection.Object;
      Direction : in Shared.Direction.Cardinal)
   return Boolean is
   begin
      return This.Streets_Existence (Direction);
   end Exists_Street_For_Direction;

   function "="(This, Other : Intersection.Object) return Boolean is
   begin
      return This.Id = Other.Id
        and This.Streets = This.Streets
        and This.Streets_Existence = This.Streets_Existence;
   end "=";

   procedure Initialize (This : in out Intersection.Object)
   is separate;

   procedure Set_Id (This : in out Intersection.Object;
                     Id   : in     Infra_Id) is
   begin
      This.Id := Id;
   end Set_Id;

   procedure Update_Size (
      This              : in out Intersection.Object;
      Intersection_Type : in Intersection.Intersection_Type)
   is separate;

   procedure Set_Intersection_Type (
      This              : in out Intersection.Object;
      Intersection_Type : in     Intersection.Intersection_Type)
   is separate;

   procedure Set_Traveller_Utils (
      This            : in out Intersection.Object;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class) is
   begin
      This.Traveller_Utils := Traveller_Utils;
   end Set_Traveller_Utils;

   procedure Set_Infrastructure_Utils (
      This                 : in out Intersection.Object;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class) is
   begin
      This.Infrastructure_Utils := Infrastructure_Utils;
   end Set_Infrastructure_Utils;

   procedure Set_Crossing_Strategy (
      This              : in out Intersection.Object;
      Crossing_Strategy : access Intersection.Crossing.Object'Class) is
   begin
      This.Crossing_Strategy := Crossing_Strategy;
   end Set_Crossing_Strategy;

   procedure Increment_Streets (This : in out Intersection.Object) is
   begin
      This.Streets_Count := This.Streets_Count + 1;
   end Increment_Streets;

   procedure Connect_Street(This      : in out Intersection.Object;
                            Street_Id : in     Infra_Id;
                            Stretches : in     Infra_Id_List.List;
                            Direction : in     Shared.Direction.Cardinal)
   is separate;

   function Get_Size (This : in Intersection.Object) return Natural is
   begin
      return This.Size;
   end Get_Size;

   function Is_Fully_Connected (This : in Intersection.Object)
   return Boolean is
   begin
      return This.Streets_Count >= This.Size;
   end Is_Fully_Connected;

   function Is_Not_Fully_Connected (This : in Intersection.Object)
   return Boolean is
   begin
      return This.Streets_Count < This.Size;
   end Is_Not_Fully_Connected;

   function Is_Contained_By (This         : in Intersection.Object;
                             Container_Id : in Infra_Id)
      return Boolean
   is separate;

   function Dump (This : Intersection.Object) return G_JSON.JSON_Value
   is separate;

   protected body Protected_Entries is

      procedure Try_To_Enter (
         Traveller_Id    : in     Agent.Agent_Id;
         Source_Entry    : in     Direction.Cardinal;
         Entered         :    out Boolean;
         Already_In      :    out Boolean)
      is
      begin -- Try_To_Enter
         Entered := not Entries (Source_Entry);

         if Entered then
            Entries (Source_Entry) := TRUE;
            Travellers (Source_Entry) := Traveller_Id;
            Already_In := False;
         else
            Already_In := Traveller_Id /= Travellers (Source_Entry);
         end if;
      end Try_To_Enter;

      procedure Leave (Traveller_Id : in Agent.Agent_Id)
      is
         Current_Traveller : Agent.Agent_Id;
      begin -- Leave
         for Source in Travellers'Range loop
            Current_Traveller := Travellers(Source);
            if Current_Traveller = Traveller_Id then
               Entries (Source) := False;
            end if;
         end loop;
      end Leave;

      function Number_Of_Travellers return Integer is
      begin
         return Travellers'Length;
      end Number_Of_Travellers;

      function Has_Traveller_Id (Source_Entry : in Direction.Cardinal)
        return Boolean
      is
      begin
            return Entries (Source_Entry);
      end Has_Traveller_Id;

      function Get_Traveller_Id (Source_Entry : in Direction.Cardinal)
         return Agent.Agent_Id
      is
      begin
         pragma Assert (Entries (Source_Entry) = True,
                     "Protected_Entries::Get_Traveller_Id - Undefined Entry");
         return Travellers (Source_Entry);
      end Get_Traveller_Id;

   end Protected_Entries;

end Reactive.Infrastructure.Intersection;
