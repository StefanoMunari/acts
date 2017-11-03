with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Active.Build;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Passive.Road_Sign;

with Reactive;

with Shared.Direction;

package body Active.Traveller.Vehicle.Bus is

   package SU              renames Ada.Strings.Unbounded;
   package Types           renames Interface_Layer.Utils.Types;
   package Build           renames Active.Build;
   use Types.Recipient_Type_Pkg;
   use Reactive.Stretch_Type_Package;

   function Create (
      Id             : in     Agent.Agent_Id;
      Maximum_Speed  : in     Natural;
      Max_Passengers : in     Natural;
      Travel_Ref     : access Active.Travel.Object'Class;
      Bus_Stops      : in     Infra_Id_List.List;
      Route_Stops    : in     Infra_Id_List.List;
      Space_Master_Ref     : access Space_Master.Object'Class := null;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Lane_Utils           : access Lane.Utils.Object'Class := null;
      Way_Utils            : access Way.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null;
      Pedestrian_Utils     : access Pedestrian.Utils.Object'Class := null;
      SSD_Utils            : -- Stretch_Sign_Decorator
         access Stretch_Sign_Decorator.Utils.Object'Class := null;
      Stub                 :
         access Stub_Pkg.Object'Class := null;
      App_Wrapper_Factory  :
         access App_Wrapper_Pkg.Abstract_Factory.Object'Class:= null)
   return Bus.Reference
   is
      Bus : Traveller.Vehicle.Bus.Reference :=
         new Traveller.Vehicle.Bus.Object;
   begin
      Vehicle.Init (Vehicle              => Bus.all,
                    Id                   => Id,
                    Maximum_Speed        => Maximum_Speed,
                    Max_Passengers       => Max_Passengers,
                    Travel_Ref           => Travel_Ref,
                    Infrastructure_Utils => Infrastructure_Utils,
                    Stretch_Utils        => Stretch_Utils,
                    Street_Utils         => Street_Utils,
                    Host_Utils           => Host_Utils,
                    Traveller_Utils      => Traveller_Utils,
                    Intersection_Utils   => Intersection_Utils);

      for Stop of Bus_Stops loop
         if not Bus.Bus_Stops.Contains (Stop) then
            Bus.Bus_Stops.Append (Stop);
         end if;
      end loop;

      for Stop of Route_Stops loop
         if not Bus.Route_Stops.Contains (Stop) then
            Bus.Route_Stops.Append (Stop);
         end if;
      end loop;

      Bus.Space_Master_Ref := Space_Master_Ref;
      if Space_Master_Ref = null then
         Bus.Space_Master_Ref := Space_Master.Get_Instance;
      end if;

      Bus.Lane_Utils := Lane_Utils;
      if Lane_Utils = null then
         Bus.Lane_Utils := Lane.Utils.Get_Instance;
      end if;

      Bus.Way_Utils := Way_Utils;
      if Way_Utils = null then
         Bus.Way_Utils := Way.Utils.Get_Instance;
      end if;

      Bus.Pedestrian_Utils := Pedestrian_Utils;
      if Pedestrian_Utils = null then
         Bus.Pedestrian_Utils := Pedestrian.Utils.Get_Instance;
      end if;

      Bus.SSD_Utils := SSD_Utils;
      if SSD_Utils = null then
         Bus.SSD_Utils := Stretch_Sign_Decorator.Utils.Get_Instance;
      end if;

      Bus.Stub := Stub;
      if Stub = null then
         Bus.Stub := Stub_Pkg.Create;
      end if;

      if App_Wrapper_Factory = null then
         Bus.App_Wrapper_Factory := new
            Interface_Layer.Wrappers.Application.Concrete_Factory.Object;
      else
         Bus.App_Wrapper_Factory := App_Wrapper_Factory;
      end if;

      return Bus;
   end Create;

   procedure Travel (This : in out Bus.Object) is
   begin
   -- Important: call *Traveller*'s implementation of travel since Bus does not
   --+ behave in the same exact way of a Vehicle
      Traveller.Travel (Traveller.Object'Class (This));
   end Travel;

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Bus.Object)
   return Boolean is (TRUE);

   function Get_Size (This : in Bus.Object) return Natural
   is (5);

   procedure On_Bus_Stop (This : in out Bus.Object)
   is
      Current_Route_Stop  : Infra_Id := This.Route_Stops.First_Element;
      Landing_Stretch     : Infra_Id;
      Landing_Passengers  : Agent_Id_List.List;
      Boarding_Passengers : Agent_Id_List.List;
      Not_Yet_Landed      : Agent_Id_List.List;
      Not_Yet_Boarded     : Agent_Id_List.List;
      No_One_Boarded      : Boolean := True;
      No_One_Landed       : Boolean := True;
      Bus_Stop_Ref        : Road_Sign.Bus_Stop.Reference;
   begin
      if This.Get_Position /= Current_Route_Stop then
         return;
      end if;

   -- Retrieve landing stretch from the head of This.Bus_Stops
      Landing_Stretch := This.Bus_Stops.First_Element;
      Bus_Stop_Ref :=
         Road_Sign.Bus_Stop.Reference (
            This.SSD_Utils.Get_Sign (Landing_Stretch));
   -- Then put head of This.Bus_Stops at its end
      This.Bus_Stops.Delete_First;
      This.Bus_Stops.Append (Landing_Stretch);
      This.Route_Stops.Delete_First;
      This.Route_Stops.Append (Current_Route_Stop);

   -- Get list of landing passengers
      Landing_Passengers := This.Get_Landing_Passengers (Landing_Stretch);
      Boarding_Passengers := This.Get_Boarding_Passengers (Bus_Stop_Ref);

      loop
      -- Perform landing operation
         Not_Yet_Landed :=
            This.Land_Passengers (Landing_Stretch, Landing_Passengers);
         No_One_Landed :=
            Agent_Id_List."=" (Landing_Passengers, Not_Yet_Landed);
         Landing_Passengers := Not_Yet_Landed;

      -- Perform boarding operation
         Not_Yet_Boarded :=
            This.Board_Passengers (
               Landing_Stretch, Boarding_Passengers, Bus_Stop_Ref);
         No_One_Boarded :=
            Agent_Id_List."=" (Boarding_Passengers, Not_Yet_Boarded);
         Boarding_Passengers := Not_Yet_Boarded;
      exit when (No_One_Boarded and No_One_Landed);
      end loop;

   end On_Bus_Stop;

   function Get_Route_Stops (This : in Bus.Object) return Infra_Id_List.List
   is
      Stops : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      for Stop of This.Route_Stops loop
         Stops.Append (Stop);
      end loop;
      return Stops;
   end Get_Route_Stops;

   function Dump (This : in Bus.Object)
   return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := Vehicle.Dump (Vehicle.Object'Class (This));

      JSON.Set_Field (Type_Field, Build.Bus_Type);

      return JSON;
   end Dump;

   function Get_Landing_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id)
   return Agent_Id_List.List
   is
      Landing_Passengers : Agent_Id_List.List := Agent_Id_List.Empty_List;
   begin
      for Passenger of This.Passengers loop
         if This.Traveller_Utils.Get_Next_Step (Passenger)
            = Landing_Stretch
         then
            Landing_Passengers.Append (Passenger);
         end if;
      end loop;
      return Landing_Passengers;
   end Get_Landing_Passengers;

   function Get_Boarding_Passengers (
      This         : in out Bus.Object;
      Bus_Stop_Ref : in     Road_Sign.Bus_Stop.Reference)
   return Agent_Id_List.List
   is
   begin
      return Bus_Stop_Ref.Get_Waiting_For_Bus (This.Id);
   end Get_Boarding_Passengers;

   function Land_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id;
      Passengers      : in     Agent_Id_List.List)
   return Agent_Id_List.List
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
      Landed                : Boolean := False;
      Freed                 : Boolean := False;
      Not_Landed_Passengers : Agent_Id_List.List := Agent_Id_List.Empty_List;
      T_Wrapper              : App_Wrapper_Pkg.Reference; -- Traveller wrapper
      Recipient              : Recipient_Type;
   begin
      for Passenger of Passengers loop
         This.Stretch_Utils.Tread (Landing_Stretch, Passenger, Landed);
         if Landed then
         -- Free passenger space in bus
            This.Free (Passenger, Freed);
         -- set position,
            This.Traveller_Utils.Set_Position (Passenger, Landing_Stretch);
         -- recompute landing traveller's travel
            This.Pedestrian_Utils.Recompute_Travel (Passenger);
         -- reschedule passenger for next tick
            This.Space_Master_Ref.Defer (Passenger, True);
         -- Notify that a passenger went off the bus
            T_Wrapper :=
               This.App_Wrapper_Factory.Create_Wrapper (
                  SU.Unbounded_String (Passenger));
            Recipient.Id   := Landing_Stretch;
            Recipient.Sort := Types.TREADABLE;
            This.Stub.Async_Request (
               T_Wrapper, Types.EXIT_BUS, Recipient, This.Id);
            Free (T_Wrapper);
         else
            Not_Landed_Passengers.Append (Passenger);
         end if;
      end loop;
      return Not_Landed_Passengers;
   end Land_Passengers;

   function Board_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id;
      Passengers      : in     Agent_Id_List.List;
      Bus_Stop_Ref    :        Road_Sign.Bus_Stop.Reference)
   return Agent_Id_List.List
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
      Boarded                : Boolean;
      Not_Boarded_Passengers : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Left                   : Boolean;
      T_Wrapper              : App_Wrapper_Pkg.Reference; -- Traveller wrapper
      Recipient              : Recipient_Type;
   begin
      for Passenger of Passengers loop
      -- Vehicle.Board makes someone board only if there is room
         This.Board (Passenger, Boarded);
         if Boarded then
         -- Make the passenger stop waiting
            This.Pedestrian_Utils.Stop_Waiting (Passenger);
         -- Remove pedestrian from bus stop's waiting list
            Bus_Stop_Ref.Remove_From_Waiting_List (This.Id, Passenger);
         -- Make passenger leave stretch
            This.Stretch_Utils.Leave (Landing_Stretch, Passenger, Left);
         -- Notify that a passenger boarded the bus
            T_Wrapper :=
               This.App_Wrapper_Factory.Create_Wrapper (
                  SU.Unbounded_String (Passenger));
            Recipient.Id   := Landing_Stretch;
            Recipient.Sort := Types.TREADABLE;
            This.Stub.Async_Request (
               T_Wrapper, Types.ENTER_BUS, Recipient, This.Id);
            Free (T_Wrapper);
         else
            Not_Boarded_Passengers.Append (Passenger);
         end if;
      end loop;

      return Not_Boarded_Passengers;
   end Board_Passengers;

end Active.Traveller.Vehicle.Bus;
