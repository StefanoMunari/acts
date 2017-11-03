with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Reactive.Infrastructure.Lane.Utils;
with Reactive.Infrastructure.Exceptions;

use Reactive.Infrastructure.Exceptions;

package body Reactive.Infrastructure.Stretch is

   package SU renames Ada.Strings.Unbounded;
   use Agent;

   procedure Init (
      Stretch         : in out Infrastructure.Stretch.Object'Class;
      Id              : in     Infra_Id;
      Size            : in     Natural;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null) is
   begin
      Stretch.Id := Id;
      Stretch.Size := Size;

      if Lane_Utils = null then
         Stretch.Lane_Utils := Lane.Utils.Get_Instance;
      else
         Stretch.Lane_Utils := Lane_Utils;
      end if;

      if Traveller_Utils = null then
         Stretch.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      else
         Stretch.Traveller_Utils := Traveller_Utils;
      end if;
      Stretch.Protected_Travellers_Queue.Set_Traveller_Utils (
         Stretch.Traveller_Utils);
      Stretch.Protected_Travellers_Queue.Set_Size (Size);
   end Init;

   procedure Set_Lane (
      This    : in out Stretch.Object;
      Lane_Id : in     Infra_Id)
   is
   begin
      This.Lane_Id := Lane_Id;
   end Set_Lane;

   procedure Tread (This         : in out Stretch.Object;
                    Traveller_Id : in     Agent_Id;
                    Advanced     :    out Boolean) is
   begin
      This.Lane_Utils.Enter (Lane_Id      => This.Lane_Id,
                             Traveller_Id => Traveller_Id);

      This.Enter_If_Not_Already_Inside (Traveller_Id => Traveller_Id,
                                        Entered      => Advanced);
   end Tread;

   function Is_Waiting_To_Enter_Stretch (
      This : in Stretch.Object;
      Traveller_Id : in Agent_Id) return Boolean is
   begin
      return This.Protected_Travellers_Queue.Is_Waiting_To_Enter_Stretch (
         Traveller_Id);
   end Is_Waiting_To_Enter_Stretch;

   procedure Leave (
      This         : in out Stretch.Object;
      Traveller_Id : in     Agent_Id;
      Left         :    out Boolean) is
   begin
      This.Protected_Travellers_Queue.Exit_From_Stretch (Traveller_Id, Left);
      This.Protected_Travellers_Queue.Unbook (Traveller_Id);
   end Leave;

   function Get_Id (This : in Stretch.Object) return Infra_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Size (This : in Stretch.Object) return Natural is
   begin
      return This.Size;
   end Get_Size;

   function Find_Street (This : in Stretch.Object)
   return Infra_Id is
   begin
      return This.Lane_Utils.Find_Street (Lane_Id => This.Lane_Id);
   end Find_Street;

   function Find_Lane (This : in Stretch.Object)
   return Infra_Id is
   begin
      return This.Lane_Id;
   end Find_Lane;

   function Calculate_Position (This : in Stretch.Object)
   return Natural
   is
      Stretch_Position : Natural;
      Found_Stretch    : Boolean;
   begin
      This.Lane_Utils.Find_Stretch_Position (
         Lane_Id          => This.Lane_Id,
         Stretch_Id       => This.Id,
         Stretch_Position => Stretch_Position,
         Found            => Found_Stretch);

      if Found_Stretch = FALSE then
         Raise_Stretch_Missing_Into_Lane_Exception
           (Stretch_Id => This.Id,
            Lane_Id => This.Lane_Id);
      end if;

      return Stretch_Position;
   end Calculate_Position;

   function Is_Before (This, Other: in Stretch.Object)
   return Boolean
   is
      Stretches_Count_For_Lane : Natural :=
         This.Lane_Utils.Count_Stretches (Lane_Id => This.Lane_Id);
      This_Position            : Natural := This.Calculate_Position;
      Other_Position           : Natural := Other.Calculate_Position;
   begin
      if Direction ."="(
         This.Lane_Utils.Get_Direction (Lane_Id => This.Lane_Id),
         This.Lane_Utils.Get_Direction (Lane_Id => Other.Lane_Id))
      then
         if This_Position < Other_Position then
            return TRUE;
         end if;
      else
         if This_Position < Stretches_Count_For_Lane - Other_Position then
            return TRUE;
         end if;
      end if;
      return FALSE;
   end Is_Before;

   procedure Set_Host (
      This    : in out Stretch.Object;
      Host_Id : in     Infra_Id) is
   begin
      This.Host_Id     := Host_Id;
      This.Has_Host_Id := True;
   end Set_Host;

   function Get_Host (This : in Stretch.Object)
   return Infra_Id is
   begin
      return This.Host_Id;
   end Get_Host;

   function Has_Host (This : in Stretch.Object)
   return Boolean is (This.Has_Host_Id);

   procedure Initialize (This : in out Stretch.Object) is
   begin
      This.Protected_Travellers_Queue := new Protected_Travellers_Queue;
   end Initialize;

   procedure Adjust (This : in out Stretch.Object) is
   begin
      This.Protected_Travellers_Queue := new Protected_Travellers_Queue;
   end Adjust;

   type Protected_Travellers_Queue_Reference is
      access all Protected_Travellers_Queue;

   procedure Finalize (This : in out Stretch.Object) is
      procedure Free is
         new Ada.Unchecked_Deallocation (
            Object => Protected_Travellers_Queue,
            Name   => Protected_Travellers_Queue_Reference);
   begin
      Free (This.Protected_Travellers_Queue);
   end Finalize;

   function Find_Intersections (This : in Stretch.Object)
   return Infra_Id_Set.Set is
   begin
      return This.Lane_Utils.Find_Intersections (Lane_Id => This.Lane_Id);
   end Find_Intersections;

   function Is_Contained_By (This         : in Stretch.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if This.Lane_Id = Container_Id then
         return TRUE;
      end if;
      return This.Lane_Utils.Is_Contained_By (Lane_Id      => This.Lane_Id,
                                              Container_Id => Container_Id);
   end Is_Contained_By;

   procedure Enter_If_Not_Already_Inside (
      This         : in Infrastructure.Stretch.Object;
      Traveller_Id : in     Agent_Id;
      Entered      :    out Boolean) is
   begin
      Entered := FALSE;
      This.Protected_Travellers_Queue.Enter_If_Not_Already_Inside (
         Traveller_Id => Traveller_Id,
         Entered      => Entered);
   end Enter_If_Not_Already_Inside;

   function Is_Empty (This : in Stretch.Object) return Boolean
   is (This.Protected_Travellers_Queue.Is_Stretch_Empty);

   procedure Put_Traveller (This         : in Stretch.Object;
                            Traveller_Id : in Agent_Id) is
   begin
      This.Protected_Travellers_Queue.Put_Traveller (Traveller_Id);
   end Put_Traveller;

   function Book (
      This         : in Stretch.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean
   is
      Booked : Boolean := False;
   begin
      This.Protected_Travellers_Queue.Book (Traveller_Id, Booked);
      return Booked;
   end Book;

   procedure Unbook (This         : in Stretch.Object;
                     Traveller_Id : in Agent.Agent_Id) is
   begin -- Unbook
      This.Protected_Travellers_Queue.Unbook (Traveller_Id);
   end Unbook;

   function Dump (This : Stretch.Object) return G_JSON.JSON_Value
   is
      JSON            : G_JSON.JSON_Value := G_JSON.Create_Object;
      Travellers      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Traveller       : G_JSON.JSON_Value := G_JSON.Create_Object;
      Treading_Id     : Agent_Id;
      Is_Booked       : Boolean;
      Booker          : Agent.Agent_Id;
   begin
      JSON.Set_Field (Id_Field, Integer (This.Id));
      JSON.Set_Field (Size_Field, Integer (This.Size));

      if This.Protected_Travellers_Queue.Has_Treading then
         Treading_Id := This.Protected_Travellers_Queue.Get_First_Treading;
         while This.Protected_Travellers_Queue.Has_Next_Treading (Treading_Id)
         loop
            Treading_Id :=
               This.Protected_Travellers_Queue.Get_Next_Treading (Treading_Id);
            Traveller.Set_Field (Id_Field, Treading_Id);
            G_JSON.Append (Travellers, Traveller);
         end loop;
      end if;
      JSON.Set_Field (Travellers_Field, Travellers);

      JSON.Set_Field (Decorations_Field, G_JSON.Create_Object);

      This.Protected_Travellers_Queue.Get_Overtaking (Is_Booked, Booker);
      if Is_Booked then
         JSON.Set_Field (Overtake_Field, Booker);
      end if;

      return JSON;
   end Dump;

   function Get_Travellers_Queue (This : in Stretch.Object)
   return access Stretch.Protected_Travellers_Queue
   is (This.Protected_Travellers_Queue);


   --------------------
   --- TRAVELLERS P.O.
   --------------------

   protected body Protected_Travellers_Queue is

      procedure Enter_If_Not_Already_Inside (
         Traveller_Id : in     Agent_Id;
         Entered      :    out Boolean)
      is
      begin
         if not Grant_Tread (Traveller_Id) then
            Entered := False;
            return;
         end if;

         Enter_Into_Stretch (Traveller_Id, Entered);
      end Enter_If_Not_Already_Inside;

      function Is_Stretch_Full return Boolean is
      begin
         return Size_Of_Treading_Travellers = Stretch_Size;
      end Is_Stretch_Full;

      function Is_Stretch_Not_Full return Boolean is
      begin
         return not Is_Stretch_Full;
      end Is_Stretch_Not_Full;

      function Is_Stretch_Empty return Boolean is
      begin
         return Size_Of_Treading_Travellers = 0;
      end Is_Stretch_Empty;

      function Size_Of_Treading_Travellers return Natural
      is
         Total_Size : Natural := 0;
      begin
         for Traveller of Treading_Travellers loop
            Total_Size := Total_Size + Traveller_Utils.Get_Size (Traveller);
         end loop;
         return Total_Size;
      end Size_Of_Treading_Travellers;

      procedure Enter_Into_Stretch (
         Traveller_Id : in     Agent_Id;
         Entered      :    out Boolean)
      is
         Traveller_Size : Natural := Traveller_Utils.Get_Size (Traveller_Id);
      begin
         if Treading_Travellers.Contains (Traveller_Id) then
            Entered := True;
            return;
         end if;
      -- Check size of traveller to see if it fits in the stretch
         if Size_Of_Treading_Travellers + Traveller_Size > Stretch_Size then
            Entered := False;
            return;
         end if;
      -- If there is room for traveller, add it to the list of treading travs
         if not Treading_Travellers.Contains (Traveller_Id) then
            Treading_Travellers.Append (Traveller_Id);
            Entered := True;
         end if;
      end Enter_Into_Stretch;

      procedure Exit_From_Stretch (
         Traveller_Id : in     Agent_Id;
         Exited       :    out Boolean)
      is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor := Treading_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Treading_Travellers.Delete (Traveller_Cursor);
            Exited := not Treading_Travellers.Contains (Traveller_Id);
         end if;
      end Exit_From_Stretch;

      procedure Enter_Into_Waiting_List (
         Traveller_Id : in     Agent_Id;
         Entered      :    out Boolean) is
      begin
         if not Waiting_Travellers.Contains (Traveller_Id) then
            Waiting_Travellers.Append (Traveller_Id);
            Entered := Waiting_Travellers.Contains (Traveller_Id);
         end if;
      end Enter_Into_Waiting_List;

      procedure Exit_From_Waiting_List (
         Traveller_Id : in     Agent_Id;
         Exited       :    out Boolean)
      is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor := Waiting_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Waiting_Travellers.Delete (Traveller_Cursor);
            Exited := not Waiting_Travellers.Contains (Traveller_Id);
         end if;
      end Exit_From_Waiting_List;

      function Is_Waiting_To_Enter_Stretch (Traveller_Id : in Agent_Id)
         return Boolean is
      begin
         return Waiting_Travellers.Contains (Traveller_Id);
      end Is_Waiting_To_Enter_Stretch;

      function Is_Inside_The_Stretch (Traveller_Id : in Agent_Id)
         return Boolean is
      begin
         return Treading_Travellers.Contains (Traveller_Id);
      end Is_Inside_The_Stretch;

      procedure Put_Traveller (
         Traveller_Id : in Agent_Id) is
      begin
         Treading_Travellers.Append (Traveller_Id);
      end Put_Traveller;

      function Has_Treading return Boolean is (Treading_Travellers.Is_Empty);

      function Get_First_Treading return Agent_Id
      is (Treading_Travellers.First_Element);

      function Has_Next_Treading (Traveller_Id : Agent_Id) return Boolean
      is
         Treading_Cursor, Next_Treading_Cursor : Agent_Id_List.Cursor
           := Agent_Id_List.No_Element;
      begin
         Treading_Cursor := Treading_Travellers.Find (Traveller_Id);

         if Agent_Id_List.Has_Element (Treading_Cursor)
         then
            Next_Treading_Cursor := Agent_Id_List.Next (
               Position => Treading_Cursor);

            return Agent_Id_List.Has_Element (Next_Treading_Cursor);
         end if;

         return False;
      end Has_Next_Treading;

      function Get_Next_Treading (Traveller_Id : Agent_Id)
      return Agent_Id
      is
         Treading_Cursor, Next_Treading_Cursor : Agent_Id_List.Cursor;
      begin
         Treading_Cursor := Treading_Travellers.Find (Traveller_Id);

         Next_Treading_Cursor := Agent_Id_List.Next (Treading_Cursor);

         return Agent_Id_List.Element (Next_Treading_Cursor);
      end Get_Next_Treading;

      procedure Set_Size (New_Size : Natural) is
      begin
         Stretch_Size := New_Size;
      end Set_Size;

      procedure Set_Traveller_Utils (
         T_U : access Active.Traveller.Utils.Object'Class := null)
      is
      begin
         Traveller_Utils := T_U;
      end Set_Traveller_Utils;

      function Grant_Tread (
         Traveller_Id : in Agent.Agent_Id)
      return Boolean
      is
      begin
         return (not Booked)
                or
                (Booked and Booker = Traveller_Id);
      end Grant_Tread;

      procedure Book (
         Traveller_Id  : in     Agent_Id;
         Booked_Return :    out Boolean) is
      begin
      -- Check stretch is free before booking
         if not
            (Treading_Travellers.Is_Empty and Waiting_Travellers.Is_Empty)
         then
            Booked_Return := False;
            return;
         end if;

         if Booked then
            Booked_Return := False;
         else
             Booker:= Traveller_Id;
             Booked := True;
         end if;
      end Book;

      procedure Unbook (Traveller_Id : in Agent_Id)
      is
      begin
         if Booked and Booker = Traveller_Id then
            Booked := False;
         end if;
      end Unbook;

      procedure Get_Overtaking (
         Is_Booked    : out Boolean;
         Traveller_Id : out Agent.Agent_Id) is
      begin
         Is_Booked := Booked;
         Traveller_Id := Booker;
      end Get_Overtaking;

      procedure Clear is
      begin
         Waiting_Travellers.Clear;
         Treading_Travellers.Clear;
      end Clear;

   end Protected_Travellers_Queue;

end Reactive.Infrastructure.Stretch;
