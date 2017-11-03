with Active.Build;

with Reactive;

package body Active.Traveller.Vehicle.Private_Motor_Vehicle is

   package Build renames Active.Build;
   use Reactive.Stretch_Type_Package;

   function Create (
      Id             : in Agent.Agent_Id;
      Maximum_Speed  : in Natural;
      Max_Passengers : in Natural;
      Vehicle_Type   : in Private_Motor_Vehicle_Type;
      Travel_Ref     : access Active.Travel.Object'Class;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Lane_Utils           : access Lane.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null)
   return Private_Motor_Vehicle.Reference
   is
      Private_Motor_Vehicle : Traveller.Vehicle.Private_Motor_Vehicle.Reference
        := new Traveller.Vehicle.Private_Motor_Vehicle.Object;
   begin
      Vehicle.Init (Vehicle              => Private_Motor_Vehicle.all,
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

      Private_Motor_Vehicle.Vehicle_Type := Vehicle_Type;

      Private_Motor_Vehicle.Lane_Utils := Lane_Utils;
      if Lane_Utils = null then
         Private_Motor_Vehicle.Lane_Utils := Lane.Utils.Get_Instance;
      end if;

      return Private_Motor_Vehicle;
   end Create;

   procedure Travel (This : in out Private_Motor_Vehicle.Object)
   is
   -- Get position before action
      Current_Position      : Infra_Id := This.Get_Position;
      Current_Lane          : Infra_Id;
      Residual_Route_Length : Natural
         := Natural (This.Travel_Ref.Get_Residual_Route.Length);
      New_Position          : Infra_Id;
   begin
         if This.Will_Attempt_Overtake and Residual_Route_Length > 2
         then
            Current_Lane := This.Stretch_Utils.Find_Lane (Current_Position);
            This.Lane_Utils.Attempt_Overtake (
               Current_Lane, Current_Position, Residual_Route_Length, This.Id);
            This.Will_Attempt_Overtake := False;
         end if;

      -- Travel using superclass' implementation
         Vehicle.Travel (Vehicle.Object (This));

      -- Get position after action
         New_Position := This.Get_Position;

      -- If position is still the same, plan to attempt overtake
         if Current_Position = New_Position then
            This.Will_Attempt_Overtake := True;
         end if;
   end Travel;

   function Is_Affected_By_Traffic_Lights (
      This : in Private_Motor_Vehicle.Object)
   return Boolean is (TRUE);

   function Get_Size (This : in Private_Motor_Vehicle.Object) return Natural
   is (5);

   function Dump (This : in Private_Motor_Vehicle.Object)
   return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := Vehicle.Dump (Vehicle.Object'Class (This));

      JSON.Set_Field (Type_Field, Build.Private_Motor_Type);

      if This.Vehicle_Type = CAR then
         JSON.Set_Field (PVT_Type_Field, Build.Car_Pvt_Type);
      elsif This.Vehicle_Type = MOTORCYCLE then
         JSON.Set_Field (PVT_Type_Field, Build.Motorcycle_Pvt_Type);
      else
         JSON.Set_Field (PVT_Type_Field, Build.Sidecar_Pvt_Type);
      end if;

      return JSON;
   end Dump;

end Active.Traveller.Vehicle.Private_Motor_Vehicle;
