-- core
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
-- lib
with GNATCOLL.JSON;

with Active.Traveller.Pedestrian;
with Active.Traveller.Pedestrian.Extractor;

with Interface_Layer.Presentation.Converter.JSON;
with Interface_Layer.Presentation.JSON_Format;
with Interface_Layer.Utils.Explorer.Pedestrian;
with Interface_Layer.Utils.Types;

with Shared.String_Splitter;
with Shared.String_List;

package body Interface_Layer.Utils.Explorer.Vehicle is

   package JSON_Converter_Pkg
      renames Interface_Layer.Presentation.Converter.JSON;
   package JSON_Format_Pkg renames Interface_Layer.Presentation.JSON_Format;
   package G_JSON          renames GNATCOLL.JSON;
   package SSplitter       renames Shared.String_Splitter;
   package String_List     renames Shared.String_List;
   package SU              renames Ada.Strings.Unbounded;

   procedure Init (
      This           : in out Vehicle.Object'Class;
      Max_Passengers :        Natural;
      Passengers     :        Agent_Id_List.List;
      District       : access Reactive.District.Object'Class := null)
   is
   begin
      This.Max_Passengers := Max_Passengers;
      This.Passengers := Agent_Id_List.Copy (Passengers);

      if District = null then
         This.District := Reactive.District.Get_Instance;
      else
         This.District := District;
      end if;
   end Init;

   procedure Set_District (
      This     : in out Vehicle.Object'Class;
      District : access Reactive.District.Object'Class := null) is
   begin
      if District = null then
         This.District := Reactive.District.Get_Instance;
      else
         This.District := District;
      end if;
   end Set_District;

   procedure Marshalling (This       : in     Explorer.Vehicle.Object;
                          Stream_Map :    out String_Map.Data.Map)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Explorer.Pedestrian.Object'Class, Explorer.Pedestrian.Reference);
      Pass_Extractor : Active.Traveller.Pedestrian.Extractor.Object;
      Pass_Reference : Active.Traveller.Pedestrian.Reference;
      Pass_Explorer  : Explorer.Pedestrian.Reference;
      JSON_Converter : JSON_Converter_Pkg.Object;
      JSON_Obj       : JSON_Format_Pkg.Object;
      Pass_Data      : String_Map.Data.Map;
      Passenger_Ind  : Natural := 1; -- index
      Iterator       : Agent_Id_List.Cursor;
      Counter        : Natural := 0;
      Passengers_Key : SU.Unbounded_String;
      Passenger_SU   : SU.Unbounded_String := SU.To_Unbounded_String ("");
   begin
      Explorer.Object (This).Marshalling (Stream_Map);

   -- Serialize Vehicle's Max_Passengers
      Stream_Map.Insert ("Max_Passengers",
                         Natural'Image (This.Max_Passengers));
   -- Meta information: number of passengers currently in
      Stream_Map.Insert ("Passengers_Number",
                         Natural'Image (Natural (This.Passengers.Length)));

   -- Serialize Traveller's Passengers, a list of IDs
      for Passenger_Id of This.Passengers loop
         Pass_Explorer  := new Explorer.Pedestrian.Object;
         Pass_Reference := Active.Traveller.Pedestrian.Reference(
            This.District.Find_Traveller_By_Id (Passenger_Id));
         Pass_Extractor.Extract (Pass_Reference.all, Pass_Explorer.all);
         Pass_Explorer.Marshalling (Pass_Data);
         JSON_Obj :=
            JSON_Format_Pkg.Object (JSON_Converter.Encode (Pass_Data));

         Passengers_Key := SU.To_Unbounded_String (
            "Passenger" & Natural'Image (Passenger_Ind));
         Stream_Map.Insert (SU.To_String (Passengers_Key),
                            JSON_Obj.Write);
         Agent_Id_List.Next (Iterator);
         Free (Pass_Explorer);
      end loop;
   end Marshalling;

   procedure Unmarshalling (This       : in out Explorer.Vehicle.Object;
                            Stream_Map : in     String_Map.Data.Map)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Explorer.Pedestrian.Object'Class, Explorer.Pedestrian.Reference);
      Passengers_JSON : JSON_Format_Pkg.Object;
      JSON_Converter  : JSON_Converter_Pkg.Object;
      Passenger_Data  : String_Map.Data.Map;
      Passenger_Id    : Agent.Agent_Id;
      Result          : Traveller.Reference;
      Pass_Extractor  : Active.Traveller.Pedestrian.Extractor.Object;
      Pass_Explorer   : Explorer.Pedestrian.Reference;
      Added           : Boolean;
      Escape          : Character := '\';
      Quote           : Character := '"';
      Aux_Passengers  : SU.Unbounded_String;
      Passengers_SU   : SU.Unbounded_String;
      Passengers_Arr  : G_JSON.Json_Array;
   begin
      Explorer.Object (This).Unmarshalling (Stream_Map);
      This.Max_Passengers :=
         Natural'Value (Stream_Map.Element ("Max_Passengers"));
      This.Passengers_Number :=
         Natural'Value (Stream_Map.Element ("Passengers_Number"));

      if Stream_Map.Contains ("Passenger") then
         Aux_Passengers :=
            SU.To_Unbounded_String (Stream_Map.Element ("Passenger"));
      else
         Aux_Passengers := SU.To_Unbounded_String ("[]");
      end if;

      Passengers_SU :=
         Shared.String_Splitter.Add_Character (Aux_Passengers, Escape, Quote);
      Passengers_JSON := JSON_Format_Pkg.Read (SU.To_String (Passengers_SU));
      Passengers_Arr  := Passengers_JSON.Get;
      for I in 1 .. This.Passengers_Number loop
         declare
            Passenger        : String := "";
            Passenger_G_JSON : G_JSON.JSON_Value
               := G_JSON.Get (Passengers_Arr, I);
            Passenger_JSON   : JSON_Format_Pkg.Object
               := JSON_Format_Pkg.Read (SU.To_String (Passenger_G_JSON.Write));
         begin
         -- TODO: FIX
         -- Append passenger to Passengers
            Passenger_Id := Agent.Create_Id_From_Natural (Integer'Value (Passenger_JSON.Get ("Id")));
            This.Passengers.Append (Passenger_Id);

            if not This.District.Contains_Traveller (Passenger_Id) then
               Pass_Explorer  := new Explorer.Pedestrian.Object;
               Passenger_JSON.Set_Field ("Route", "");
               Passenger_Data := JSON_Converter.Decode (Passenger_JSON);
               Pass_Explorer.Unmarshalling (Passenger_Data);
               Result := new Traveller.Pedestrian.Object;
               Pass_Extractor.Extract (Pass_Explorer.all, Result.all);
               This.District.Add_Traveller (Result, Added);
               Free (Pass_Explorer);
            end if;

         end;
      end loop;
   end Unmarshalling;

end Interface_Layer.Utils.Explorer.Vehicle;
