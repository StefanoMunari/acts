with Active.Travel.Travel_Planning;
with Active.Travel.Travel_Progress;

with Reactive;

package body Active.Travel.Bus_Travel is

   package Planning_Pkg renames Active.Travel.Travel_Planning;
   package Progress_Pkg renames Active.Travel.Travel_Progress;

   use Reactive.Stretch_Type_Package;

   function Create (
      Route_Source      : in Slice.Map;
      Route_Destination : in Slice.Map;
      Traveller_Id      : in Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class
        := null;
      Bus_Utils         : access Bus_Utils_Pkg.Object'Class
        := null;
      Space_Master      : access Space_Master_Pkg.Object'Class := null)
   return Bus_Travel.Reference
   is
      Bus_Travel_Ref : Active.Travel.Bus_Travel.Reference
         := new Bus_Travel.Object;
   begin
      Travel.Init (
         Active.Travel.Reference (Bus_Travel_Ref),
         Route_Source, Route_Destination, Traveller_Id, Travel_State,
         Traveller_Utils);

      if Bus_Utils = null then
        Bus_Travel_Ref.Bus_Utils := Bus_Utils_Pkg.Get_Instance;
      else
        Bus_Travel_Ref.Bus_Utils := Bus_Utils;
      end if;

      if Space_Master = null then
        Bus_Travel_Ref.Space_Master := Space_Master_Pkg.Get_Instance;
      else
        Bus_Travel_Ref.Space_Master := Space_Master;
      end if;

      return Bus_Travel_Ref;
   end Create;

   procedure Advance (This : in out Bus_Travel.Object)
   is
      Bus_Id        : Agent.Agent_Id := This.Traveller_Id;
      S_Type        : Stretch_Type
         := This.Traveller_Utils.Get_Stretch_Type (Bus_Id);
      Stops         : Infra_Id_List.List;
      Partial_Route : Infra_Id_List.List;
      Source_C      : Infra_Id_List.Cursor;
      Destination_C : Infra_Id_List.Cursor;
      Source        : Infra_Id;
      Destination   : Infra_Id;
   begin
      if not (This.Travel_State.all in Planning_Pkg.Object'Class) then
         Travel.Advance (Travel.Object (This));
         return;
      end if;
      Stops := This.Bus_Utils.Get_Route_Stops (Bus_Id);
      Source_C := Stops.First;
      Destination_C := Stops.First;
   -- Destination_C is "one step ahead" w.r.t. Source_C
      Infra_Id_List.Next (Destination_C);
      while Infra_Id_List.Has_Element (Destination_C) loop
         Source        := Infra_Id_List.Element (Source_C);
         Destination   := Infra_Id_List.Element (Destination_C);
         Partial_Route :=
            Planning_Pkg.Get_Route_From_A_To_B (
               Source, Destination, This,
               Stretch_Type'Image (S_Type),
               SU.To_String (Bus_Id));
         for Step of Partial_Route loop
            if Step /= Destination then
               This.Route.Append (Step);
            end if;
         end loop;
         Infra_Id_List.Next (Source_C);
         Infra_Id_List.Next (Destination_C);
      end loop;
      Source        := Infra_Id_List.Element (Source_C);
      Destination   := Stops.First_Element;
      Partial_Route :=
         Planning_Pkg.Get_Route_From_A_To_B (
            Source, Destination, This,
            Stretch_Type'Image (S_Type),
            SU.To_String (Bus_Id));
      for Step of Partial_Route loop
         if Step /= Destination then
            This.Route.Append (Step);
         end if;
      end loop;

      This.Traveller_Utils.Set_Current_Speed (
         Bus_Id,
         This.Traveller_Utils.Get_Maximum_Speed (Bus_Id));
      This.Space_Master.Defer (Bus_Id, False);

      This.Change_Travel_State (Progress_Pkg.Get_Instance);
   end Advance;

   function Get_Current_Step_Id (This : Bus_Travel.Object) return Infra_Id
   is (This.Route.First_Element);

   function Get_Previous_Step_Id (This : Bus_Travel.Object) return Infra_Id
   is (This.Route.Last_Element);

   function Get_Next_Step_Id (This : Bus_Travel.Object) return Infra_Id
   is
      Cursor : Infra_Id_List.Cursor := This.Route.First;
   begin
      Infra_Id_List.Next (Cursor);
      return Infra_Id_List.Element (Cursor);
   end Get_Next_Step_Id;

   function Has_Next_Step (This : in Bus_Travel.Object) return Boolean
   is (True);

   procedure Consume_Step (This : in out Bus_Travel.Object)
   is
      Previous_Head : Infra_Id := This.Route.First_Element;
   begin
      This.Route.Delete_First;
      This.Route.Append (Previous_Head);
   end Consume_Step;

end Active.Travel.Bus_Travel;
