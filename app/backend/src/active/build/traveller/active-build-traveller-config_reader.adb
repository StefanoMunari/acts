with Reactive;

with Active.Build.Exceptions;
use Active.Build.Exceptions;

package body Active.Build.Traveller.Config_Reader is
-- use
   use Reactive.Infra_Id_Type;

   procedure Set_Builder (
      This        :    out Traveller.Config_Reader.Object;
      Builder_Ref : not null Builder.Reference) is
   begin
      This.Builder_Ref := Builder_Ref;
   end Set_Builder;

   function Read (
      This           : in out Traveller.Config_Reader.Object;
      Traveller_Json : in     G_JSON.JSON_Value)
   return Agent.Agent_Id
   is
      --- Traveller            : aliased Traveller_Pkg.Reference;
      Traveller_Id         : Agent.Agent_Id;
      Traveller_Type       : SU.Unbounded_String;
      Source               : G_JSON.JSON_Value;
      Destination          : G_JSON.JSON_Value;
      Residual_Travel      : G_JSON.JSON_Array;
      Travel_State         : SU.Unbounded_String;
      Current_Speed        : Natural;
      Max_Speed            : Natural;
      Current_Position_Int : Integer;
      Current_Position     : Infra_Id;
      Passengers           : G_JSON.JSON_Array;
      Max_Passengers       : Integer := -1;
      PVT_Type             : SU.Unbounded_String;
      Bus_Stops            : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Route_Stops          : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Is_Waiting           : Boolean;
   begin
      if not Traveller_Json.Has_Field (Id_Field) then
         Raise_Missing_Field_For_Traveller (Id_Field);
      end if;

      Traveller_Id :=
         Agent.Create_Id_From_Natural (Traveller_Json.Get (Id_Field));

      if not Traveller_Json.Has_Field (Type_Field) then
         Raise_Missing_Field_For_Traveller (Type_Field);
      end if;

      Traveller_Type := Traveller_Json.Get (Type_Field);

      if not Traveller_Json.Has_Field (Source_Field) then
         Raise_Missing_Field_For_Traveller (Source_Field);
      end if;
      Source := Traveller_Json.Get (Source_Field);

      if not Traveller_Json.Has_Field (Destination_Field) then
         Raise_Missing_Field_For_Traveller (Destination_Field);
      end if;
      Destination := Traveller_Json.Get (Destination_Field);

      if not Traveller_Json.Has_Field (Residual_Travel_Field) then
         Raise_Missing_Field_For_Traveller (Residual_Travel_Field);
      end if;
      Residual_Travel := Traveller_Json.Get (Residual_Travel_Field);

      if not Traveller_Json.Has_Field (Travel_State_Field) then
         Raise_Missing_Field_For_Traveller (Travel_State_Field);
      end if;
      Travel_State := Traveller_Json.Get (Travel_State_Field);

      if not Traveller_Json.Has_Field (Current_Speed_Field) then
         Raise_Missing_Field_For_Traveller (Current_Speed_Field);
      end if;
      Current_Speed := Traveller_Json.Get (Current_Speed_Field);

      if not Traveller_Json.Has_Field (Max_Speed_Field) then
         Raise_Missing_Field_For_Traveller (Max_Speed_Field);
      end if;
      Max_Speed := Traveller_Json.Get (Max_Speed_Field);

      if not Traveller_Json.Has_Field (Current_Position_Field) then
         Raise_Missing_Field_For_Traveller (Current_Position_Field);
      end if;
      Current_Position_Int := Traveller_Json.Get (Current_Position_Field);
      Current_Position := Infra_Id (Current_Position_Int);

      if not Traveller_Json.Has_Field (Passengers_Field) then
         Raise_Missing_Field_For_Traveller (Passengers_Field);
      end if;
      Passengers := Traveller_Json.Get (Passengers_Field);

      if Traveller_Type = SU.To_Unbounded_String (Private_Motor_Type) and
         not Traveller_Json.Has_Field (PVT_Type_Field) then
         Raise_Missing_Field_For_Traveller (PVT_Type_Field);
      end if;
      PVT_Type := Traveller_Json.Get (PVT_Type_Field);

      if Traveller_Json.Has_Field (Max_Passengers_Field) then
         Max_Passengers := Traveller_Json.Get (Max_Passengers_Field);
      end if;

      if Traveller_Json.Has_Field (Bus_Stops_Field) then
         Bus_Stops := Traveller_Json.Get (Bus_Stops_Field);
      end if;

      if Traveller_Json.Has_Field (Route_Stops_Field) then
         Route_Stops := Traveller_Json.Get (Route_Stops_Field);
      end if;

      if Traveller_Json.Has_Field (Route_Stops_Field) then
         Route_Stops := Traveller_Json.Get (Route_Stops_Field);
      end if;

      if Traveller_Json.Has_Field (Is_Waiting_Field) then
         Is_Waiting := Traveller_Json.Get (Is_Waiting_Field);
      end if;

      return This.Builder_Ref
                     .With_Id (Traveller_Id)
                     .With_Src (Source)
                     .With_Dst (Destination)
                     .With_Residual_Travel (Residual_Travel)
                     .With_Travel_State (Travel_State)
                     .With_Current_Speed (Current_Speed)
                     .With_Max_Speed (Max_Speed)
                     .With_Current_Position (Current_Position)
                     .With_Passengers (Passengers)
                     .With_Max_Passengers (Max_Passengers)
                     .With_Pvt_Type (PVT_Type)
                     .With_Bus_Stops (Bus_Stops)
                     .With_Route_Stops (Route_Stops)
                     .With_Is_Waiting (Is_Waiting)
                     .Get_Result (Traveller_Type);
   end Read;

end Active.Build.Traveller.Config_Reader;
