with Ada.Unchecked_Deallocation;

with Reactive.Infrastructure.Stretch.Utils;
with Reactive.Infrastructure.Way.Utils;
with Reactive.Infrastructure.Exceptions;

use Reactive.Infrastructure.Exceptions;

package body Reactive.Infrastructure.Lane is

   procedure Init (
      Lane      : in out Infrastructure.Lane.Object'Class;
      Id        : in Infra_Id;
      Direction : in Shared.Direction.Straight;
      Stretch_Utils   : access Stretch.Utils.Object'Class := null;
      Way_Utils       : access Way.Utils.Object'Class := null;
      Traveller_Utils : access Traveller.Utils.Object'Class := null) is
   begin
      Lane.Id := Id;
      Lane.Direction := Direction;

      if Stretch_Utils = null then
         Lane.Stretch_Utils := Stretch.Utils.Get_Instance;
      else
         Lane.Stretch_Utils := Stretch_Utils;
      end if;

      if Way_Utils = null then
         Lane.Way_Utils := Way.Utils.Get_Instance;
      else
         Lane.Way_Utils := Way_Utils;
      end if;

      if Traveller_Utils = null then
         Lane.Traveller_Utils := Traveller.Utils.Get_Instance;
      else
         Lane.Traveller_Utils := Traveller_Utils;
      end if;

      Lane.Initialize;
   end Init;

   procedure Set_Way (This : in out Lane.Object; Way_Id : in Infra_Id) is
   begin
      This.Way_Id := Way_Id;
   end Set_Way;

   procedure Enter (
      This         : in out Lane.Object;
      Traveller_Id : in     Agent.Agent_Id)
   is
   begin -- Enter
      if This.Protected_Stretches.Count_Stretches = 0 then
         Raise_No_Stretches_In_Lane_Exception (Lane_Id => This.Id);
      end if;
   end Enter;

   procedure Append_Stretch (
      This       : in out Lane.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean) is
   begin
      This.Protected_Stretches.Append_Stretch (Stretch_Id, Added);
   end Append_Stretch;

   function Count_Stretches (This : in Lane.Object) return Natural is
   begin
      return This.Protected_Stretches.Count_Stretches;
   end Count_Stretches;

   procedure Find_Stretch_Position (
      This             : in     Lane.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean) is
   begin
      This.Protected_Stretches.Find_Stretch_Position (
         Stretch_Id       => Stretch_Id,
         Stretch_Position => Stretch_Position,
         Found            => Found);
   end Find_Stretch_Position;

   function Get_Stretch_By_Position (
      This             : in Lane.Object;
      Stretch_Position : in Natural)
   return Infra_Id
   is
      Old_Stretch : Infra_Id := This.Protected_Stretches.Get_First_Stretch;
      Stretch     : Infra_Id := Old_Stretch;
      I           : Natural := 1;
      Found       : Boolean := True;
   begin
      while I < Stretch_Position loop
         This.Protected_Stretches.Get_Next_Stretch (
            Old_Stretch, Stretch, Found);
         I := I + 1;
      end loop;
      return Stretch;
   end Get_Stretch_By_Position;

   function "=" (This, Other : in Lane.Object) return Boolean is
   begin
      return This.Id = Other.Id;
   end "=";

   function Find_Street (This : in Lane.Object)
   return Infra_Id is
   begin
      return This.Way_Utils.Find_Street (This.Way_Id);
   end Find_Street;

   function Get_Id (This : in Lane.Object) return Infra_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Direction (This : Lane.Object)
   return Shared.Direction.Straight is
   begin
      return This.Direction;
   end Get_Direction;

   procedure Initialize (This : in out Lane.Object) is
   begin
      This.Protected_Stretches := new Protected_Stretches;
   end Initialize;

   procedure Adjust (This : in out Lane.Object) is
   begin
      This.Protected_Stretches := new Protected_Stretches;
   end Adjust;

   type Protected_Stretches_Reference is access all Protected_Stretches;

   procedure Finalize (This : in out Lane.Object) is
      procedure Free is
        new Ada.Unchecked_Deallocation (
            Object => Protected_Stretches,
            Name   => Protected_Stretches_Reference);
   begin
      Free (This.Protected_Stretches);
   end Finalize;

   function Find_Intersections (This : in Lane.Object) return Infra_Id_Set.Set
   is
      Intersections : Infra_Id_Set.Set := Infra_Id_Set.Empty_Set;
   begin
      if This.Intersection_Existence then
         Intersections.Insert (New_Item => This.Intersection_Id);
      end if;
      return Intersections;
   end Find_Intersections;

   procedure Add_Intersection (
      This            : in out Lane.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean) is
   begin
      Added := FALSE;

      If not This.Intersection_Existence then
         This.Intersection_Id := Intersection_Id;
         Added := TRUE;
      end if;

      This.Intersection_Existence := TRUE;
   end Add_Intersection;

   function Is_Contained_By (This         : in Lane.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if This.Way_Id = Container_Id then
         return TRUE;
      end if;
      return This.Way_Utils.Is_Contained_By (Way_Id       => This.Way_Id,
                                             Container_Id => Container_Id);
   end Is_Contained_By;

   procedure Attempt_Overtake (
      This                : in Lane.Object;
      Stretch_Id          : in Infra_Id;
      Max_Overtake_Length : in Natural;
      Traveller_Id        : in Agent.Agent_Id)
   is
      Stretch_Position     : Natural;
      Lane_Length          : Natural := This.Count_Stretches;
      Found                : Boolean;
      Residual_Stretches   : Natural;
      Other_Stretches      : Natural_List.List := Natural_List.Empty_List;
      Overtake_List        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Other_Stretches_Id   : Infra_Id_List.List;
      Final_Stretch        : Infra_Id;
      Overtake_Will_Happen : Boolean;
   begin
   -- If there are not enough stretches, don't try an overtake
      This.Find_Stretch_Position (Stretch_Id, Stretch_Position, Found);
      Residual_Stretches := Lane_Length - Stretch_Position;

      if Residual_Stretches < 2 then
         return;
      end if;

   --+ 1) Compute list of stretches on which overtake will take place
      Other_Stretches.Append (Lane_Length - Stretch_Position + 1);

      Overtake_List :=
         This.Way_Utils.Get_Stretches_On_Other_Lane (
            This.Way_Id, This.Id, Other_Stretches);
      Final_Stretch := This.Get_Stretch_By_Position (Stretch_Position + 2);
      Overtake_List.Append (Final_Stretch);

      Overtake_Will_Happen := Natural (Overtake_List.Length) > 0;

   --+ 2) Call Book on each of them until one returns False
      for Overtake_Stretch of Overtake_List loop
         Overtake_Will_Happen :=
            This.Stretch_Utils.Attempt_Overtake (
               Overtake_Stretch, Traveller_Id);
      end loop;

      if Overtake_Will_Happen then
   --+ 3.1) If all true, then change traveller route
         Overtake_List.Prepend (Stretch_Id);
         This.Traveller_Utils.Modify_Travel_Beginning (
            Traveller_Id, Overtake_List);
      else
   --+ 3.2) If some false, then Unbook all previous prenotations
         for Overtake_Stretch of Overtake_List loop
            This.Stretch_Utils.Unbook (Overtake_Stretch, Traveller_Id);
         end loop;
      end if;
   end Attempt_Overtake;

   function Dump (This : Lane.Object) return G_JSON.JSON_Value
   is
      JSON           : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretches      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch        : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretch_Id     : Infra_Id;
      Stretch_Id_Aux : Infra_Id;
      Bool_Next      : Boolean;
   begin
      JSON.Set_Field (Id_Field, Integer (This.Id));
      JSON.Set_Field (Direction_Field,
                      Shared.Direction.Straight'Image (This.Direction));

      Stretch_Id := This.Protected_Stretches.Get_First_Stretch;
      while This.Protected_Stretches.Has_Next_Stretch (Stretch_Id) loop
         This.Protected_Stretches.Get_Next_Stretch (
            Stretch_Id, Stretch_Id_Aux, Bool_Next);
         Stretch_Id := Stretch_Id_Aux;
         Stretch := This.Stretch_Utils.Dump (Stretch_Id);
         G_JSON.Append (Stretches, Stretch);
      end loop;
      JSON.Set_Field (Stretches_Field, Stretches);

      JSON.Set_Field (Decorations_Field, G_JSON.Create_Object);

      return JSON;
   end Dump;

   protected body Protected_Stretches is

      function Count_Stretches return Natural is
      begin
         return Natural (Stretches.Length);
      end Count_Stretches;

      function Contains_Stretch (Stretch_Id : Infra_Id) return Boolean
      is (Stretches.Contains (Stretch_Id));

      procedure Find_Stretch_Position (
         Stretch_Id       : in     Infra_Id;
         Stretch_Position :    out Natural;
         Found            :    out Boolean)
      is
         Stretch_Count     : Natural := 0;
         Stretch_Set       : Infra_Id_List.List := Stretches;
         Passed_Stretch_Id : Infra_Id renames Stretch_Id;
      begin
         -- not efficient implementation
         for Stretch_Id of Stretch_Set
         loop
            if Stretch_Id = Passed_Stretch_Id then
               Stretch_Position := Stretch_Count;
               Found := TRUE;
               return;
            end if;
            Stretch_Count := Stretch_Count + 1;
         end loop;
         Found := FALSE;
      end Find_Stretch_Position;

      procedure Append_Stretch (
         Stretch_Id : in Infra_Id;
         Added      : out Boolean) is
      begin
         Added := FALSE;
         if not Stretches.Contains (Stretch_Id) then
            Stretches.Append (Stretch_Id);
            Added := Stretches.Contains (Stretch_Id);
         end if;
      end Append_Stretch;

      function Get_First_Stretch return Infra_Id
      is (Stretches.First_Element);

      function Has_Next_Stretch (Stretch_Id : in Infra_Id)
         return Boolean
      is
         Stretch_Cursor, Next_Stretch_Cursor : Infra_Id_List.Cursor
            := Infra_Id_List.No_Element;
      begin
         Stretch_Cursor := Stretches.Find (Stretch_Id);

         if Infra_Id_List.Has_Element (Stretch_Cursor) then
            Next_Stretch_Cursor := Infra_Id_List.Next (
               Position => Stretch_Cursor);

            return Infra_Id_List.Has_Element (Next_Stretch_Cursor);
         end if;

         return FALSE;
      end Has_Next_Stretch;

      procedure Get_Next_Stretch (
         Stretch_Id      : in     Infra_Id;
         Next_Stretch_Id :    out Infra_Id;
         Found           :    out Boolean)
      is
         Stretch_Cursor, Next_Stretch_Cursor : Infra_Id_List.Cursor;
      begin
         if Has_Next_Stretch (Stretch_Id) then
            Stretch_Cursor := Stretches.Find (Stretch_Id);

            Next_Stretch_Cursor := Infra_Id_List.Next (Stretch_Cursor);

            Next_Stretch_Id := Infra_Id_List.Element (Next_Stretch_Cursor);
            Found := TRUE;
         else
            Found := FALSE;
         end if;
      end Get_Next_Stretch;

   end Protected_Stretches;

end Reactive.Infrastructure.Lane;
