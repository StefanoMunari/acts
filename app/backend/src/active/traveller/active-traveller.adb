with Active.Traveller.Exceptions;

with Reactive.Infrastructure.Intersection.Utils;
with Reactive.Intersectable;
with Reactive.Treadable;

with Shared.Random;

use Active.Traveller.Exceptions;

package body Active.Traveller is

   use Agent; -- for = operator between ids

   package Infrastructure renames Reactive.Infrastructure;
   package Intersectable  renames Reactive.Intersectable;
   package Treadable      renames Reactive.Treadable;
   package Random_Pkg     renames Shared.Random;

   procedure Init (
      Traveller     : in out Active.Traveller.Object'Class;
      Id            : in     Agent.Agent_Id;
      Maximum_Speed : in     Natural;
      Travel_Ref    : access Active.Travel.Object'Class;
      Infrastructure_Utils :
         access Infrastructure.Utils.Object'Class := null;
      Intersection_Utils :
         access Reactive.Infrastructure.Intersection.Utils.Object'Class := null
   )
   is
   begin
      Traveller.Id := Id;
      Traveller.Maximum_Speed := Maximum_Speed;

      if Travel_Ref = null then
         Raise_Null_Travel_Reference (Id);
      end if;
      -- reach this program point if Travel_Ref is not null
      Traveller.Travel_Ref := Travel_Ref;

      if Infrastructure_Utils = null then
         Traveller.Infrastructure_Utils := Infrastructure.Utils.Get_Instance;
      else
         Traveller.Infrastructure_Utils := Infrastructure_Utils;
      end if;

      if Intersection_Utils = null then
         Traveller.Intersection_Utils :=
            Reactive.Infrastructure.Intersection.Utils.Get_Instance;
      else
         Traveller.Intersection_Utils := Intersection_Utils;
      end if;

      Traveller.Current_Speed := Maximum_Speed;
   end Init;

   function Get_Id (This : in Traveller.Object) return Agent.Agent_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Position (This : in Traveller.Object) return Infra_Id is
   begin
      return This.Current_Position;
   end Get_Position;

   procedure Set_Position (This         :    out Traveller.Object;
                           New_Position : in     Infra_Id) is
   begin
      This.Current_Position := New_Position;
   end Set_Position;

   function Get_Maximum_Speed (This : in Traveller.Object) return Natural is
   begin
      return This.Maximum_Speed;
   end Get_Maximum_Speed;

   function Get_Current_Speed (This : in Traveller.Object) return Natural is
   begin
      return This.Current_Speed;
   end Get_Current_Speed;

   procedure Set_Current_Speed (This      : in out Traveller.Object;
                                New_Speed : in     Natural) is
   begin
      if New_Speed > This.Maximum_Speed then
         Raise_Maximum_Speed_Exceeded_Exception
           (Traveller_Id            => This.Id,
            Traveller_Maximum_Speed => This.Maximum_Speed,
            New_Speed               => New_Speed);
      else
         This.Current_Speed := New_Speed;
      end if;
   end Set_Current_Speed;

   function Get_Scheduled_For (This : in Traveller.Object) return Float is
   begin
      return This.Scheduled_For;
   end Get_Scheduled_For;

   procedure Set_Scheduled_For (This              :    out Traveller.Object;
                                New_Scheduled_For : in     Float) is
   begin
      This.Scheduled_For := New_Scheduled_For;
   end Set_Scheduled_For;

   procedure Set_Travel (This       : in out Traveller.Object;
                         Travel_Ref : access Active.Travel.Object'Class) is
   begin
      This.Travel_Ref := Travel_Ref;
   end Set_Travel;

   function Get_Travel_Source (This : in Traveller.Object)
      return Slice.Map is
   begin
      return This.Travel_Ref.Get_Route_Source;
   end Get_Travel_Source;

   function Get_Travel_Destination (This : in Traveller.Object)
      return Slice.Map is
   begin
      return This.Travel_Ref.Get_Route_Destination;
   end Get_Travel_Destination;

   function Look_Ahead_Step (
      This  : in Traveller.Object;
      Index : in Natural)
   return Infra_Id
   is
      Residual_Route : Infra_Id_List.List
         := This.Travel_Ref.Get_Residual_Route;
      The_Cursor     : Infra_Id_List.Cursor := Residual_Route.First;
   begin
      for I in 1..Index loop
         Infra_Id_List.Next (The_Cursor);
      end loop;
      return Infra_Id_List.Element (The_Cursor);
   end Look_Ahead_Step;

   function Does_Travel_Contain_Step (This : in Traveller.Object;
                                      Step : in Infra_Id)
   return Boolean is
   begin
      return This.Travel_Ref.Contains (Step);
   end Does_Travel_Contain_Step;

   not overriding
   function Does_Travel_Contain_Steps (This : in Traveller.Object;
                                       Step : in Slice.Map)
   return Boolean is
   begin
      return This.Travel_Ref.Contains (Step);
   end Does_Travel_Contain_Steps;

   procedure Modify_Travel_Beginning (
      This          : Traveller.Object;
      New_Beginning : Infra_Id_List.List) is
   begin
      This.Travel_Ref.Modify_Beginning (New_Beginning);
   end Modify_Travel_Beginning;

   procedure Erase_Route (This : in out Traveller.Object) is
   begin
      This.Travel_Ref.Set_Residual_Route (Infra_Id_List.Empty_List);
   end Erase_Route;

   function Has_Next_Step (This : in Traveller.Object) return Boolean is
   begin
      return This.Travel_Ref.Has_Next_Step;
   end Has_Next_Step;

   not overriding
   procedure Consume_Step (This : in Traveller.Object) is
   begin
      This.Travel_Ref.Consume_Step;
   end;

   not overriding
   function Is_Travelling (This : in Traveller.Object) return Boolean is
   begin
      return This.Travel_Ref.Is_Progressing;
   end Is_Travelling;

   procedure Travel (This : in out Traveller.Object) is
   begin
      This.Travel_Ref.Advance;
   end Travel;

   function Get_List_From_Slice (This      : in Traveller.Object;
                                 The_Slice : in Slice.Map)
   return Infra_Id_List.List
   is
      This_Disp : Traveller.Object'Class := This;
   begin
     return The_Slice.Element (Key => This_Disp.Get_Stretch_Type);
   end Get_List_From_Slice;

   procedure Act (This : in out Traveller.Object) is
   begin
      This.Travel;
   end Act;

   function "=" (This, Other : Traveller.Object) return Boolean is
   begin
      if This.Id = Other.Id then
         if This.Maximum_Speed = Other.Maximum_Speed then
            return True;
         end if;
      end if;
      return False;
   end;

   function Dump (This : in Traveller.Object) return G_JSON.JSON_Value
   is
      JSON             : G_JSON.JSON_Value := G_JSON.Create_Object;
      Source           : Slice.Map;
      Source_JSON      : G_JSON.JSON_Value := G_JSON.Create_Object;
      Destination      : Slice.Map;
      Destination_JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
      S_Type           : Stretch_Type;
      Id_List          : Infra_Id_List.List;
      Id_List_JSON     : G_JSON.JSON_Array;
      Id_JSON          : G_JSON.JSON_Value;
      Residual_Route   : Infra_Id_List.List;
   begin
      JSON.Set_Field (Id_Field, This.Id);

   -- Source slice
      Source := This.Get_Travel_Source;
      for I in Stretch_Type'Range loop
         S_Type := Stretch_Type (I);
         Id_List := Source.Element (S_Type);
         Id_List_JSON := G_JSON.Empty_Array;
         for Id of Id_List loop
            Id_JSON := G_JSON.Create_Object;
            Id_JSON.Set_Field ("id", Integer (Id));
            G_JSON.Append (Id_List_JSON, Id_JSON);
         end loop;
         Source_JSON.Set_Field (Stretch_Type'Image (S_Type), Id_List_JSON);
      end loop;
      JSON.Set_Field (Source_Field, Source_JSON);

   -- Destination slice
      Destination := This.Get_Travel_Destination;
      for I in Stretch_Type'Range loop
         S_Type := Stretch_Type (I);
         Id_List := Destination.Element (S_Type);
         Id_List_JSON := G_JSON.Empty_Array;
         for Id of Id_List loop
            Id_JSON := G_JSON.Create_Object;
            Id_JSON.Set_Field ("id", Integer (Id));
            G_JSON.Append (Id_List_JSON, Id_JSON);
         end loop;
         Destination_JSON.Set_Field (Stretch_Type'Image (S_Type),
                                     Id_List_JSON);
      end loop;
      JSON.Set_Field (Destination_Field, Destination_JSON);

   -- Residual travel
      Residual_Route := This.Travel_Ref.Get_Residual_Route;
      Id_List_JSON := G_JSON.Empty_Array;
      for Id of Id_List loop
         Id_JSON := G_JSON.Create (Integer (Id));
         G_JSON.Append (Id_List_JSON, Id_JSON);
      end loop;
      JSON.Set_Field (Residual_Route_Field, Id_List_JSON);

      JSON.Set_Field (Maximum_Speed_Field, This.Maximum_Speed);
      JSON.Set_Field (Current_Speed_Field, This.Current_Speed);
      JSON.Set_Field (Current_Position_Field, Integer (This.Current_Position));
      JSON.Set_Field (Travel_State_Field, This.Travel_Ref.Dump_State);
      return JSON;
   end Dump;

end Active.Traveller;
