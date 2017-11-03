with Ada.Strings.Unbounded;

package body Passive.Road_Sign.Bus_Stop is

   package SU renames Ada.Strings.Unbounded;

   not overriding
   function Create (
      Buses             : in     Agent_Id_List.List;
      Waiting_Ids       : in     Agent_Id_To_Agent_Id_List_Map.Map;
      Stops             : in     Agent_Id_To_Infra_Id_List_Map.Map;
      Bus_Service_Utils : access Bus_Service.Utils.Object'Class := null)
   return Passive.Road_Sign.Bus_Stop.Reference is
      New_Sign : Bus_Stop.Reference := new Bus_Stop.Object;
   begin
      New_Sign.Buses             := Buses;
      New_Sign.Waiting_Ids       := Waiting_Ids;
      New_Sign.Stops             := Stops;

      New_Sign.Bus_Service_Utils :=
         Bus_Service.Utils.Reference (Bus_Service_Utils);
      if Bus_Service_Utils = null then
         New_Sign.Bus_Service_Utils := Bus_Service.Utils.Get_Instance;
      end if;

      return New_Sign;
   end Create;

   procedure Apply (This       : in out Bus_Stop.Object;
                    Traveller  : in     Agent.Agent_Id) is
   begin
      if This.Bus_Service_Utils.Is_A_Bus_Service (Traveller) then
         This.Bus_Service_Utils.On_Bus_Stop (Traveller);
      end if;
   end Apply;

   function Get_All_Bus_Stops (This : in Bus_Stop.Object)
   return Agent_Id_To_Infra_Id_List_Map.Map
   is (This.Stops);

   procedure Register_For_Bus_Waiting (
      This       : in out Bus_Stop.Object;
      Waiting_Id : in     Agent.Agent_Id;
      Bus_Id     : in     Agent.Agent_Id)
   is
      Waiting_List : Agent_Id_List.List := Agent_Id_List.Empty_List;
   begin
      if This.Waiting_Ids.Contains (Key => Bus_Id) then
         Waiting_List := This.Waiting_Ids.Element (Bus_Id);
      end if;
      Waiting_List.Append (Waiting_Id);
      This.Waiting_Ids.Include (Key => Bus_Id, New_Item => Waiting_List);
   end Register_For_Bus_Waiting;

   function Get_Waiting_For_Bus (
      This   : in out Bus_Stop.Object;
      Bus_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List
   is
   begin
      if This.Waiting_Ids.Contains (Key => Bus_Id) then
         return This.Waiting_Ids.Element (Bus_Id);
      else
         return Agent_Id_List.Empty_List;
      end if;
   end Get_Waiting_For_Bus;

   procedure Remove_From_Waiting_List (
      This                   : in out Bus_Stop.Object;
      Bus_Id                 : in     Agent.Agent_Id;
      Not_Waiting_Anymore_Id : in     Agent.Agent_Id)
   is
      Waiting_List_For_Bus : Agent_Id_List.List;
      Delete_Cursor        : Agent_Id_List.Cursor;
   begin
      Waiting_List_For_Bus := This.Waiting_Ids.Element (Bus_Id);
      Delete_Cursor := Waiting_List_For_Bus.Find (Not_Waiting_Anymore_Id);
      Waiting_List_For_Bus.Delete (Delete_Cursor);
      This.Waiting_Ids.Include (
         Key      => Bus_Id,
         New_Item => Waiting_List_For_Bus);
   end Remove_From_Waiting_List;

   function Is_A_Stop_For_Bus (This : in out Bus_Stop.Object;
                               Bus  : in     Agent.Agent_Id)
   return Boolean
   is (This.Buses.Contains (Bus));

   function Dump (This : Bus_Stop.Object) return G_JSON.JSON_Value is
      Bus_JSON          : G_JSON.JSON_Value;
      Buses_JSON        : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Waiting_For_Bus   : Agent_Id_List.List;
      Waiting_JSON      : G_JSON.JSON_Value;
      Waiting_List_JSON : G_JSON.JSON_Array;
      Stops             : Infra_Id_List.List;
      Stop_JSON         : G_JSON.JSON_Value;
      Stops_JSON        : G_JSON.JSON_Array;
   begin
      for Bus of This.Buses loop
         Bus_JSON := G_JSON.Create_Object;
         Bus_JSON.Set_Field (Id_Field, Bus);

         Waiting_For_Bus := This.Waiting_Ids.Element (Bus);
         Waiting_List_JSON := G_JSON.Empty_Array;
         for Waiting of Waiting_For_Bus loop
            Waiting_JSON := G_JSON.Create (SU.Unbounded_String (Waiting));
            G_JSON.Append (Waiting_List_JSON, Waiting_JSON);
         end loop;
         Bus_JSON.Set_Field (Waiting_List_Field, Waiting_JSON);

         Stops := This.Stops.Element (Bus);
         Stops_JSON := G_JSON.Empty_Array;
         for Stop of Stops loop
            Stop_JSON := G_JSON.Create (Integer (Stop));
            G_JSON.Append (Stops_JSON, Stop_JSON);
         end loop;
         Bus_JSON.Set_Field (Stops_Field, Stops_JSON);

         G_JSON.Append (Buses_JSON, Bus_JSON);
      end loop;

      return G_JSON.Create (Buses_JSON);
   end Dump;

end Passive.Road_Sign.Bus_Stop;
