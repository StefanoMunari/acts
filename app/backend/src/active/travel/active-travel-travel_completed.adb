with Active.Build;
with Active.Travel.Travel_Planning;
with Active.Traveller.Utils;

with Reactive.District;
with Reactive.Infrastructure.Stretch.Utils;

with Shared.Agent_Id_List;
with Shared.Slice;

package body Active.Travel.Travel_Completed is

   package Build renames Active.Build;
   package District      renames Reactive.District;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Slice         renames Shared.Slice;

   function Get_Instance(
      Host_Utils :
         access Reactive.Infrastructure.Building.Host.Utils.Object'Class
            := null;
      Travel_Planning :
         access Active.Travel.Travel_Planning.Object'Class := null;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class := null;
      Space_Master :
         access Active.Space_Master.Object'Class := null;
      PC_Utils :
         access Active.People_Carrier.Utils.Object'Class := null;
      Stretch_Utils :
         access Reactive.Infrastructure.Stretch.Utils.Object'Class := null)
   return Travel_Completed.Reference is
   begin
      if Instance = null then
         Instance := new Travel_Completed.Object;
      end if;

      if (Host_Utils = null) then
         Instance.Host_Utils :=
            Reactive.Infrastructure.Building.Host.Utils.Get_Instance;
      else
         Instance.Host_Utils := Host_Utils;
      end if;

      if (Travel_Planning /= null) then
         Instance.Travel_Planning := Travel_Planning;
      end if;

      if (Traveller_Utils = null) then
         Instance.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      else
         Instance.Traveller_Utils := Traveller_Utils;
      end if;

      if (Space_Master = null) then
         Instance.Space_Master := Active.Space_Master.Get_Instance;
      else
         Instance.Space_Master := Space_Master;
      end if;

      if (PC_Utils = null) then
         Instance.PC_Utils := Active.People_Carrier.Utils.Get_Instance;
      else
         Instance.PC_Utils := PC_Utils;
      end if;

      if (Stretch_Utils = null) then
         Instance.Stretch_Utils :=
            Reactive.Infrastructure.Stretch.Utils.Get_Instance;
      else
         Instance.Stretch_Utils := Stretch_Utils;
      end if;

      return Instance;
   end Get_Instance;

   procedure Advance (This   : in out Travel_Completed.Object;
                      Travel : in out Active.Travel.Object'Class)
   is
      Host_Slice           : Slice.Map := Travel.Get_Route_Destination;
      Stretch_Id           : Infra_Id;
      Host_Id              : Infra_Id;
      Current_Traveller_Id : Agent.Agent_Id := Travel.Get_Traveller_Id;
      Travellers           : Agent_Id_List.List;
      New_Travel           : access Active.Travel.Object'Class;
      New_Destination      : Slice.Map;
      New_Source           : Slice.Map;
      Left                 : Boolean;
   begin
      Stretch_Id := This.Traveller_Utils.Get_List_From_Slice (
                     Current_Traveller_Id, Host_Slice)
                     .First_Element;
      Host_Id := This.Stretch_Utils.Get_Host (Stretch_Id);
   -- Leave current stretch
      This.Stretch_Utils.Leave (
         Stretch_Id, Current_Traveller_Id, Left);
      Travellers := This.Host_Utils.Stop_Over (Host_Id, Current_Traveller_Id);
      for Traveller of Travellers loop
         if not This.PC_Utils.Is_A_People_Carrier (Traveller) then
         -- reset each traveller travel, reversing its src and dst
            New_Destination :=
               This.Traveller_Utils.Get_Travel_Source (Traveller);
            New_Source :=
               This.Traveller_Utils.Get_Travel_Destination (Traveller);
            New_Travel := Active.Travel.Create (
               New_Source, New_Destination, Traveller,
               This.Travel_Planning);
            This.Traveller_Utils.Set_Travel (Traveller, New_Travel);
         --  schedule each travel for a distant-enough number of ticks
            This.Space_Master.Defer (Traveller, Float (TIME_SPENT_IN_HOST));
         end if;
      end loop;
   end;

   function Has_Next_Step (This   : in Travel_Completed.Object;
                           Travel : in Active.Travel.Object'Class)
                           return Boolean is
   begin
      return FALSE;
   end Has_Next_Step;

   function Is_Progressing (This   : in Travel_Completed.Object;
                            Travel : in Active.Travel.Object'Class)
                            return Boolean is
   begin
      return FALSE;
   end Is_Progressing;

   function Dump (This : Travel_Completed.Object)
   return SU.Unbounded_String
   is (SU.To_Unbounded_String (Build.Completed_State));

end Active.Travel.Travel_Completed;
