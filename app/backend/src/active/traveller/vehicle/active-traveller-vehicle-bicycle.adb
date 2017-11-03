with Active.Build;

with Reactive;
with Reactive.Infrastructure.Street.Utils;

package body Active.Traveller.Vehicle.Bicycle is

   package Build renames Active.Build;
   use Reactive.Stretch_Type_Package;

   function Create (
      Id             : in Agent.Agent_Id;
      Maximum_Speed  : in Natural;
      Max_Passengers : in Natural;
      Travel_Ref     : access Active.Travel.Object'Class;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null)
   return Bicycle.Reference
   is
      Bicycle : Traveller.Vehicle.Bicycle.Reference
         := new Traveller.Vehicle.Bicycle.Object;
   begin
      Vehicle.Init (Vehicle              => Bicycle.all,
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
      return Bicycle;
   end Create;

   function Get_Stretch_Type (This : in Bicycle.Object)
   return Stretch_Type is
   begin
      return BIKE;
   end Get_Stretch_Type;

   function Is_Affected_By_Traffic_Lights (This : in Bicycle.Object)
   return Boolean is (FALSE);

   function Get_Size (This : in Bicycle.Object) return Natural
   is (2);

   function Dump (This : in Bicycle.Object)
   return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := Vehicle.Dump (Vehicle.Object'Class (This));

      JSON.Set_Field (Type_Field, Build.Bicycle_Type);

      return JSON;
   end Dump;

end Active.Traveller.Vehicle.Bicycle;
