with Ada.Strings.Unbounded;

with Shared.Agent_Id_To_Infra_Id_List_Map;

package body Active.Traveller.Strategy.Simple is

   package SU renames Ada.Strings.Unbounded;
   package Agent_Id_To_Infra_Id_List_Map
      renames Shared.Agent_Id_To_Infra_Id_List_Map;

   function Get_Instance (
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
   return Strategy.Simple.Reference
   is
      Instance : Strategy.Simple.Reference := new Strategy.Simple.Object;
   begin

      Instance.Traveller_Utils := Traveller_Utils;
      if Traveller_Utils = null then
         Instance.Traveller_Utils := Traveller.Utils.Get_Instance;
      end if;

      return Instance;
   end Get_Instance;

   function Wait_For_Bus_Or_Not (
      This            : Strategy.Simple.Object;
      Pedestrian_Id   : Agent.Agent_Id;
      Current_Stretch : Infra_Id;
      Bus_Stop_Ref    : Road_Sign.Bus_Stop.Reference)
   return Boolean
   is
      Buses_Map  : Agent_Id_To_Infra_Id_List_Map.Map;
      Bus_Id     : Agent.Agent_Id;
      Stops_List : Infra_Id_List.List;
   begin
   -- Get `Stops map
      Buses_Map := Bus_Stop_Ref.Get_All_Bus_Stops;

      for Bus_Map in Buses_Map.Iterate loop
         Bus_Id := Agent_Id_To_Infra_Id_List_Map.Key (Bus_Map);
         Stops_List := Agent_Id_To_Infra_Id_List_Map.Element (Bus_Map);
      -- If some stop is in pedestrian's travel, return true
         for Stop of Stops_List loop

            if     Stop /= Current_Stretch
               and This.Traveller_Utils.Does_Travel_Contain_Step (
                     Pedestrian_Id, Stop)
            then
            -- Register to bus stop ref
               Bus_Stop_Ref.Register_For_Bus_Waiting (
                  Waiting_Id => Pedestrian_Id,
                  Bus_Id     => Bus_Id);
               while This.Traveller_Utils.Get_Next_Step (Pedestrian_Id) /= Stop
               loop
                  This.Traveller_Utils.Consume_Step (Pedestrian_Id);
               end loop;
               return True;
            end if;

         end loop;
      end loop;

      return False;
   end Wait_For_Bus_Or_Not;


end Active.Traveller.Strategy.Simple;
