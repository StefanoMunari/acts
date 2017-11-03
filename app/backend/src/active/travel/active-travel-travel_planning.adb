with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with AI;
with Active.Build;
with Active.Travel.Travel_Completed;
with Active.Travel.Travel_Planning.Exceptions;
with Active.Travel.Travel_Progress;

with Reactive.District;
with Reactive.Infrastructure.Intersection.Utils;
with Reactive.Infrastructure.Building.Host;
with Reactive.Infrastructure.Stretch.Utils;

with Shared.Infra_Id_List;
with Shared.Natural_List;
with Shared.Slice;

use Active.Travel.Travel_Planning.Exceptions;


package body Active.Travel.Travel_Planning is

   package Build renames Active.Build;
   package Travel_Progress renames Active.Travel.Travel_Progress;
   package District       renames Reactive.District;
   package Infrastructure renames Reactive.Infrastructure;
   package Intersection   renames Reactive.Infrastructure.Intersection;
   package Street         renames Reactive.Infrastructure.Street;
   package Stretch        renames Reactive.Infrastructure.Stretch;
   package Natural_List   renames Shared.Natural_List;
   package Slice          renames Shared.Slice;
   use Active.Space_Master.Next_Action_Type;
   use Slice;

   function Get_Instance (
      Travel_Progress      :
         access Active.Travel.Travel_Progress.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Street_Utils :
         access Reactive.Infrastructure.Street.Utils.Object'Class := null;
      Stretch_Utils        :
         access Reactive.Infrastructure.Stretch.Utils.Object'Class := null;
      Intersection_Utils   :
         access Reactive.Infrastructure.Intersection.Utils.Object'Class
            := null;
      Host_Utils           :
         access Reactive.Infrastructure.Building.Host.Utils.Object'Class
            := null;
      Space_Master         :
         access Active.Space_Master.Object'Class := null;
      Traveller_Utils      :
         access Active.Traveller.Utils.Object'Class := null)
   return Travel_Planning.Reference is
   begin
      if Instance = null then
         Instance := new Travel_Planning.Object;
      end if;

      if (Travel_Progress = null) then
         Instance.Travel_Progress
           := Active.Travel.Travel_Progress.Get_Instance;
      else
         Instance.Travel_Progress := Travel_Progress;
      end if;

      if (Infrastructure_Utils = null) then
         Instance.Infrastructure_Utils
           := Reactive.Infrastructure.Utils.Get_Instance;
      else
         Instance.Infrastructure_Utils := Infrastructure_Utils;
      end if;

      if (Street_Utils = null) then
         Instance.Street_Utils
           := Reactive.Infrastructure.Street.Utils.Get_Instance;
      else
         Instance.Street_Utils := Street_Utils;
      end if;

      if (Stretch_Utils = null) then
         Instance.Stretch_Utils
           := Reactive.Infrastructure.Stretch.Utils.Get_Instance;
      else
         Instance.Stretch_Utils := Stretch_Utils;
      end if;

      if (Intersection_Utils = null) then
         Instance.Intersection_Utils
           := Reactive.Infrastructure.Intersection.Utils.Get_Instance;
      else
         Instance.Intersection_Utils := Intersection_Utils;
      end if;

      if (Host_Utils = null) then
         Instance.Host_Utils :=
            Reactive.Infrastructure.Building.Host.Utils.Get_Instance;
      else
         Instance.Host_Utils := Host_Utils;
      end if;

      if (Space_Master = null) then
         Instance.Space_Master := Active.Space_Master.Get_Instance;
      else
         Instance.Space_Master := Space_Master;
      end if;

      if (Traveller_Utils = null) then
         Instance.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      else
         Instance.Traveller_Utils := Traveller_Utils;
      end if;

      return Instance;
   end Get_Instance;

   procedure Print_Route (Travel : in Active.Travel.Object'Class)
   is
      Route : Infra_Id_List.List;
   begin
      Route := Travel.Get_Residual_Route;
      for Step of Route
      loop
         null;
      end loop;
   end Print_Route;


   procedure Plan (This   : in out Travel_Planning.Object;
                   Travel : in out Active.Travel.Object'Class)
   is
      Traveller_Id      : Agent.Agent_Id;
      --- := Travel.Get_Traveller_Id;
      S_Type            : Reactive.Stretch_Type_Package.Stretch_Type;
      -- := This.Traveller_Utils.Get_Stretch_Type (Traveller_Id);
      Source_Slice      : Infra_Id_List.List;
      -- := Travel.Get_Route_Source.Element (S_Type);
      Destination_Slice : Infra_Id_List.List;
      Source            : Infra_Id;
      Destination       : Infra_Id;
      Path              : Infra_Id_List.List;
   begin
      Traveller_Id := Travel.Get_Traveller_Id;
      S_Type := This.Traveller_Utils.Get_Stretch_Type (Traveller_Id);
      Source_Slice := Travel.Get_Route_Source.Element (S_Type);
      Destination_Slice :=
         Travel.Get_Route_Destination.Element (
            This.Traveller_Utils.Get_Stretch_Type (
               Travel.Get_Traveller_Id));

      if Source_Slice.Is_Empty then
         Raise_Empty_SubSlice ("Source");
      end if;
      if Destination_Slice.Is_Empty then
         Raise_Empty_SubSlice ("Destination");
      end if;

      Source := Infra_Id_List.First_Element (Source_Slice);
      Destination := Infra_Id_List.Last_Element (Destination_Slice);

      This.Traveller_Utils.Set_Position (Traveller_Id, Destination);

      Path := Get_Route_From_A_To_B (
         Source, Destination, Travel,
         Stretch_Type'Image (S_Type),
         SU.To_String (Travel.Get_Traveller_Id));
      for Step of Path loop
         Travel.Route.Append (Step);
      end loop;

      --Print_Route (Travel);
   end Plan;

   function Get_Route_From_A_To_B (
      A          : in     Infra_Id;
      B          : in     Infra_Id;
      Travel_Obj : in out Active.Travel.Object'Class;
      S_Type     : in     String;
      Agent_Id   : in     String)
   return Infra_Id_List.List
   is
      Route        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Source       : String := Trim (Integer'Image (Integer (A)), Both);
      Destination  : String := Trim (Integer'Image (Integer (B)), Both);
      AI_Interface : access AI.Object'Class:= Travel_Obj.Get_AI;
      Path         : AI.Step_List.List;
      Step         : Integer;
      Index        : Natural := 0;
      ---------------------------
      -- O : Uniform Cost Search [Optimal]
      -- 1 : Greedy Search [Incomplete]
      -- 2 : A* Search [Optimal]
      ---------------------------
      Algorithm    : Natural := 0;
   begin
      Path := AI_Interface.Find_Path
         (Source, Destination, Algorithm, S_Type, Agent_Id);
      for E of Path loop    -- Note "of" instead of "in" here
        Step := Integer'Value (E);
        Route.Append (Infra_Id (Step));
        --                      Integer'Image (Step));
        Index := Index + 1;
      end loop;
      return Route;
   end Get_Route_From_A_To_B;

   procedure Advance (This   : in out Travel_Planning.Object;
                      Travel : in out Active.Travel.Object'Class)
   is
      Host_Slice           : Slice.Map := Travel.Get_Route_Source;
      Current_Traveller_Id : Agent.Agent_Id := Travel.Get_Traveller_Id;
      Exiting_Traveller_Id : Agent.Agent_Id := Travel.Get_Traveller_Id;
      Defer_Or_Not         : Next_Action;
      Stretch_Id           : Infra_Id;
      Host_Id              : Infra_Id;
   begin
      This.Plan (Travel);
      Stretch_Id :=
         This.Traveller_Utils.Get_List_From_Slice (
            Current_Traveller_Id, Host_Slice)
            .First_Element;
      Host_Id := This.Stretch_Utils.Get_Host (Stretch_Id);
      Defer_Or_Not :=
         This.Host_Utils.Exit_Building (
            Host_Id, Current_Traveller_Id, Exiting_Traveller_Id);
      if Defer_Or_Not = DEFER then
         This.Traveller_Utils.Set_Current_Speed (
            Exiting_Traveller_Id,
            This.Traveller_Utils.Get_Maximum_Speed (Exiting_Traveller_Id));
         This.Space_Master.Defer (Exiting_Traveller_Id, False);
         Travel.Change_Travel_State (This.Travel_Progress);
         return;
      end if;
      if Defer_Or_Not = RETRY then
      -- schedule for next tick: check if it will be able to exit host
         This.Space_Master.Defer (Exiting_Traveller_Id, True);
         return;
      end if;
      -- Defer_Or_Not = DO_NOT_DEFER : do nothing since *this* traveller
      --+ will not exit the host
   end Advance;

   function Has_Next_Step (This   : in Travel_Planning.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean is
   begin
      return FALSE;
   end Has_Next_Step;

   function Is_Progressing (This   : in Travel_Planning.Object;
                            Travel : in Active.Travel.Object'Class)
                            return Boolean is
   begin
      return FALSE;
   end Is_Progressing;

   function Dump (This : Travel_Planning.Object)
   return SU.Unbounded_String
   is (SU.To_Unbounded_String (Build.Planning_State));

end Active.Travel.Travel_Planning;
