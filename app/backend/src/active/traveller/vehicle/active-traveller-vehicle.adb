with Ada.Strings.Unbounded;

with Reactive.Infrastructure.Street.Utils;

package body Active.Traveller.Vehicle is

   package SU renames Ada.Strings.Unbounded;
   use Reactive.Stretch_Type_Package;

   procedure Init (
      Vehicle        : in out Traveller.Vehicle.Object'Class;
      Id             : in Agent.Agent_Id;
      Maximum_Speed  : in Natural;
      Max_Passengers : in Natural;
      Travel_Ref     :
         access Active.Travel.Object'Class;
      Infrastructure_Utils :
         access Infrastructure.Utils.Object'Class := null;
      Host_Utils           :
         access Host.Utils.Object'Class := null;
      Street_Utils         :
         access Street.Utils.Object'Class := null;
      Stretch_Utils        :
         access Stretch.Utils.Object'Class := null;
      Intersection_Utils   :
         access Intersection.Utils.Object'Class := null;
      Traveller_Utils      :
         access Traveller.Utils.Object'Class := null)
   is
   begin
      Vehicle.Max_Passengers := Max_Passengers;
      Traveller.Init (Traveller            => Vehicle,
                      Id                   => Id,
                      Maximum_Speed        => Maximum_Speed,
                      Travel_Ref           => Travel_Ref,
                      Infrastructure_Utils => Infrastructure_Utils,
                      Intersection_Utils   => Intersection_Utils);

      if Host_Utils = null then
         Vehicle.Host_Utils := Host.Utils.Get_Instance;
      else
         Vehicle.Host_Utils := Host_Utils;
      end if;

      if Street_Utils = null then
         Vehicle.Street_Utils := Street.Utils.Get_Instance;
      else
         Vehicle.Street_Utils := Street_Utils;
      end if;

      Vehicle.Stretch_Utils := Stretch_Utils;
      if Vehicle.Stretch_Utils = null then
         Vehicle.Stretch_Utils := Stretch.Utils.Get_Instance;
      end if;

      Vehicle.Traveller_Utils := Traveller_Utils;
      if Vehicle.Traveller_Utils = null then
         Vehicle.Traveller_Utils := Traveller.Utils.Get_Instance;
      end if;

   end Init;

   function Get_Stretch_Type (This : in Vehicle.Object)
   return Stretch_Type is
   begin
      return ROAD;
   end Get_Stretch_Type;

   procedure Travel (This : in out Vehicle.Object)
   is
      Current_Position   : Infra_Id := This.Get_Position;
      Host_Id            : Infra_Id;
      Passenger_Slice    : Slice.Map;
      S_Type             : Stretch_Type;
      Stretches_List     : Infra_Id_List.List;
      Dropped_Passengers : Agent_Id_List.List := Agent_Id_List.Empty_List;
      R1                 : Agent_Id_List.List; -- ignored result of function
      R2                 : Boolean; -- ignored result of function
   begin
   -- If the vehicle is at a slice of one of its passengers and travel is not
   --+ ended, check if vehicle has to drop some passengers down
      if     This.Stretch_Utils.Has_Host (Current_Position)
         and This.Travel_Ref.Has_Next_Step
      then

         for Passenger of This.Passengers loop
            Passenger_Slice :=
               This.Traveller_Utils.Get_Travel_Destination (Passenger);
            for T in Stretch_Type'Range loop
               S_Type         := Stretch_Type (T);
               Stretches_List := Passenger_Slice.Element (S_Type);

               if Stretches_List.Contains (Current_Position) then
               -- Add `Passenger to dropped passengers' list
                  Dropped_Passengers.Append (Passenger);
               -- Make `Passenger enter host
                  Host_Id := This.Stretch_Utils.Get_Host (Current_Position);
                  R1 := This.Host_Utils.Stop_Over (Host_Id, Passenger);
               end if;

            end loop;
         end loop;

      end if;

   -- Drop `Passenger from vehicle
      for Dropped_Passenger of Dropped_Passengers loop
         This.Free (Dropped_Passenger, R2);
      end loop;

   -- Call superclass operation (static dispatching)
      Traveller.Travel (Traveller.Object (This));
   end Travel;

   procedure Board (This    : in out Vehicle.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean) is
   begin
      if This.Count_Passengers < This.Max_Passengers then
         if not This.Passengers.Contains (Incomer) then
            This.Passengers.Append (Incomer);
            This.Traveller_Utils.Erase_Route (Incomer);
            Boarded := TRUE;
         else
            Boarded := FALSE;
         end if;
      end if;
   end Board;

   procedure Free (This      : in out Vehicle.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean)
   is
      Passenger_Cursor : Agent_Id_List.Cursor;
   begin
      Passenger_Cursor := This.Passengers.Find (Passenger);
      if Agent_Id_List.Has_Element (Passenger_Cursor) then
         This.Passengers.Delete (Passenger_Cursor);
         Freed := not This.Passengers.Contains (Passenger);
      else
         Freed := FALSE;
      end if;
   end Free;

   function Get_Passengers (This : in Vehicle.Object)
   return Agent_Id_List.List is (This.Passengers);

   function Count_Passengers (This : in Vehicle.Object) return Natural is
   begin
      return Natural (This.Passengers.Length);
   end Count_Passengers;

   function Get_Max_Number_Of_Passengers (This : in Vehicle.Object)
   return Natural is (This.Max_Passengers);

   function Dump (This : in Vehicle.Object)
   return G_JSON.JSON_Value
   is
      JSON            : G_JSON.JSON_Value := G_JSON.Create_Object;
      Passenger_JSON  : G_JSON.JSON_Value;
      Passengers_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
   begin
      JSON := Traveller.Dump (Traveller.Object'Class (This));

      JSON.Set_Field (Max_Passengers_Field, This.Max_Passengers);

      for Passenger of This.Passengers loop
         Passenger_JSON := G_JSON.Create (SU.Unbounded_String (Passenger));
         G_JSON.Append (Passengers_JSON, Passenger_JSON);
      end loop;
      JSON.Set_Field (Passengers_Field, Passengers_JSON);

      return JSON;
   end Dump;

end Active.Traveller.Vehicle;
