with Ada.Strings.Unbounded;

with Active.Build;
with Active.Travel.Travel_Planning;
with Active.Traveller.Strategy.Simple;

with Passive.Road_Sign;
with Passive.Road_Sign.Bus_Stop;

with Reactive;

package body Active.Traveller.Pedestrian is

   package SU        renames Ada.Strings.Unbounded;
   package Build     renames Active.Build;
   package Road_Sign renames Passive.Road_Sign;
   use Reactive.Stretch_Type_Package;

   function Create (
      Id            : in     Agent.Agent_Id;
      Maximum_Speed : in     Natural;
      Travel_Ref    : access Active.Travel.Object'Class;
      Is_Waiting    : in     Boolean := False;
      Infrastructure_Utils : access Infrastructure.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Strategy_Ref         : access Strategy.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null;
      SSD_Utils            : -- Stretch_Sign_Decorator
         access Stretch_Sign_Decorator.Utils.Object'Class := null)
   return Pedestrian.Reference
   is
      Pedestrian : Traveller.Pedestrian.Reference
        := new Traveller.Pedestrian.Object;
   begin
      Traveller.Init (Traveller            => Pedestrian.all,
                      Id                   => Id,
                      Maximum_Speed        => Maximum_Speed,
                      Travel_Ref           => Travel_Ref,
                      Infrastructure_Utils => Infrastructure_Utils,
                      Intersection_Utils   => Intersection_Utils);

      if Street_Utils = null then
         Pedestrian.Street_Utils := Street.Utils.Get_Instance;
      else
         Pedestrian.Street_Utils := Street_Utils;
      end if;

      if Strategy_Ref = null then
         Pedestrian.Strategy_Ref := Strategy.Simple.Get_Instance;
      else
         Pedestrian.Strategy_Ref := Strategy_Ref;
      end if;

      if SSD_Utils = null then
         Pedestrian.SSD_Utils := Stretch_Sign_Decorator.Utils.Get_Instance;
      else
         Pedestrian.SSD_Utils := SSD_Utils;
      end if;

      Pedestrian.Is_Waiting := Is_Waiting;

      return Pedestrian;
   end Create;

   function Get_Stretch_Type (This : in Pedestrian.Object)
   return Stretch_Type is
   begin
      return FOOT;
   end Get_Stretch_Type;

   procedure Travel (This : in out Pedestrian.Object) is
   begin
   -- If pedestrian is waiting for the bus, do nothing
      if This.Is_Waiting then
         return;
      end if;
      Traveller.Travel (Traveller.Object'Class (This));
   end Travel;

   function Is_Affected_By_Traffic_Lights (This : in Pedestrian.Object)
   return Boolean is (FALSE);

   function Get_Size (This : in Pedestrian.Object) return Natural
   is (1);

   procedure On_Bus_Stop (This : in out Pedestrian.Object)
   is
      Current_Stretch : Infra_Id;
      Sign_Ref        : Road_Sign.Reference;
      Bus_Stop_Ref    : Road_Sign.Bus_Stop.Reference;
   begin
   -- Get bus stop signal from current step
      Current_Stretch := This.Travel_Ref.Get_Current_Step_Id;

      -- Get stretch decorated with a street signal
      while not This.SSD_Utils.Is_A_Stretch_Sign_Decorator (Current_Stretch)
      loop
         Current_Stretch := This.SSD_Utils.Get_Stretch_Ref_Id (
            Current_Stretch);
      end loop;

      Sign_Ref        := This.SSD_Utils.Get_Sign (Current_Stretch);
      Bus_Stop_Ref    := Road_Sign.Bus_Stop.Reference (Sign_Ref);
   -- Invoke a strategy
      This.Is_Waiting :=
         This.Strategy_Ref.Wait_For_Bus_Or_Not (
            This.Id, Current_Stretch, Bus_Stop_Ref);
   end On_Bus_Stop;

   procedure Stop_Waiting (This : in out Pedestrian.Object)
   is
   begin
      This.Is_Waiting := False;
   end Stop_Waiting;

   procedure Recompute_Travel (This : Pedestrian.Object)
   is
      Destinations : Slice.Map;
      Destination  : Infra_Id;
      New_Route    : Infra_Id_List.List;
   begin
      Destinations := This.Get_Travel_Destination;
      Destination := Infra_Id_List.First_Element (Destinations.Element (FOOT));

      New_Route := Active.Travel.Travel_Planning.Get_Route_From_A_To_B (
         This.Get_Position, Destination, This.Travel_Ref.all,
         Stretch_Type'Image (FOOT), SU.To_String (This.Id));

      This.Travel_Ref.Set_Residual_Route (New_Route);
   end Recompute_Travel;

   function Dump (This : in Pedestrian.Object) return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON := Traveller.Dump (Traveller.Object'Class (This));

      JSON.Set_Field (Type_Field, Build.Pedestrian_Type);

      JSON.Set_Field (Is_Waiting_Field, This.Is_Waiting);

      return JSON;
   end Dump;

end Active.Traveller.Pedestrian;
