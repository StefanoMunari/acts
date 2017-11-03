with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Shared.String_Splitter;
with Shared.String_List;

package body Interface_Layer.Utils.Explorer is

   package SSplitter   renames Shared.String_Splitter;
   package String_List renames Shared.String_List;
   use Reactive.Stretch_Type_Package;

   procedure Init (
      This          : in out Explorer.Object'Class;
      Id            :        Agent.Agent_Id;
      Maximum_Speed : in     Natural;
      Current_Speed : in     Natural;
      Position      : in     Infra_Id;
      Route         :        Infra_Id_List.List;
      Source        : in     Slice.Map;
      Destination   : in     Slice.Map) is
   begin
      This.Id            := Id;
      This.Maximum_Speed := Maximum_Speed;
      This.Current_Speed := Current_Speed;
      This.Position      := Position;
      This.Route         := Route;
      This.Source        := Source;
      This.Destination   := Destination;
   end Init;

   procedure Marshalling (This       : in     Explorer.Object;
                          Stream_Map :    out String_Map.Data.Map)
   is
      Route_Id   : Infra_Id;
      Iterator   : Infra_Id_List.Cursor;
      S_Type     : Stretch_Type;
   begin

   -- Serialize Traveller's Id
      Stream_Map.Insert ("Id", SU.To_String (This.Id));
      Stream_Map.Insert ("Maximum_Speed", Natural'Image (This.Maximum_Speed));
      Stream_Map.Insert ("Current_Speed", Natural'Image (This.Current_Speed));
      Stream_Map.Insert ("Current_Position",
                         Trim (Integer'Image (Integer (This.Position)), Both));

   -- Serialize Traveller's Route, a list of IDs
      Route_Id := 0;
      Iterator := Infra_Id_List.First (This.Route);

      while Infra_Id_List.Has_Element (Iterator) loop
         declare
            Counter   : Natural;
            ID_List   : SU.Unbounded_String;
            Route_Key : SU.Unbounded_String;
         begin
            Counter := 0;
            Route_Key := SU.To_Unbounded_String ("Route"
               & Infra_Id'Image (Route_Id));
            ID_List := SU.To_Unbounded_String ("");
         -- Divide in chunks of 100 IDs
            while Infra_Id_List.Has_Element (Iterator) and Counter < 100 loop
               SU.Append (
                  ID_List, Infra_Id'Image (Infra_Id_List.Element (Iterator)));
               SU.Append (ID_List, Separator);
               Counter := Counter + 1;
               Infra_Id_List.Next (Iterator);
            end loop;
            Stream_Map.Insert (
               SU.To_String (Route_Key), SU.To_String (ID_List));
            Route_Id := Route_Id + 1;
         end;
      end loop;

      for I in Stretch_Type'Range loop
         declare
            Steps_List : Infra_Id_List.List;
            Steps_SU   : SU.Unbounded_String := SU.To_Unbounded_String ("");
            Key        : SU.Unbounded_String
               := SU.To_Unbounded_String (Source_Key);
         begin
            S_Type     := Stretch_Type (I);
            Steps_List := This.Source.Element (S_Type);
            Steps_SU   := Encode_List (Steps_List);
            SU.Append (Key, "_");
            SU.Append (Key,
                       SU.To_Unbounded_String (Stretch_Type'Image (S_Type)));
            Stream_Map.Insert (
               SU.To_String (Key), SU.To_String (Steps_SU));
         end;
      end loop;

      for I in Stretch_Type'Range loop
         declare
            Steps_List : Infra_Id_List.List;
            Steps_SU   : SU.Unbounded_String := SU.To_Unbounded_String ("");
            Key        : SU.Unbounded_String
               := SU.To_Unbounded_String (Destination_Key);
         begin
            S_Type     := Stretch_Type (I);
            Steps_List := This.Destination.Element (S_Type);
            Steps_SU   := Encode_List (Steps_List);
            SU.Append (Key, "_");
            SU.Append (Key,
                       SU.To_Unbounded_String (Stretch_Type'Image (S_Type)));
            Stream_Map.Insert (
               SU.To_String (Key), SU.To_String (Steps_SU));
         end;
      end loop;

   end Marshalling;

   procedure Unmarshalling (
      This       : in out Explorer.Object;
      Stream_Map : in     String_Map.Data.Map)
   is
      String_Route : String_List.List := SSplitter.Filter_Separator (
         Stream_Map.Element ("Route"), Separator);
   begin
   -- Unmarshalling Fields
      This.Id := SU.To_Unbounded_String (Stream_Map.Element ("Id"));
      This.Maximum_Speed :=
         Natural'Value (Stream_Map.Element ("Maximum_Speed"));
      This.Current_Speed :=
         Natural'Value (Stream_Map.Element ("Current_Speed"));
      This.Position :=
         Infra_Id (Integer'Value (Stream_Map.Element ("Current_Position")));

      for SElement of String_Route loop
         declare
            Step : String :=
               SSplitter.Filter_Delimiters (
                  SSplitter.Filter_Delimiters (
                     SSplitter.Filter_Delimiters(SElement, ","), "["), "]");
         begin
            if Step /= "" then
               This.Route.Append (Infra_Id'Value (Step));
            end if;
         end;
      end loop;

      declare
         Foot_List    : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Source_FOOT"), Separator);
         Bike_List    : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Source_BIKE"), Separator);
         Road_List    : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Source_ROAD"), Separator);
         Foot_List_Id : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Bike_List_Id : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Road_List_Id : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Source_Slice : Slice.Map := Slice.Empty_Map;
      begin
         for Foot_Id of Foot_List loop
            Foot_List_Id.Append (Infra_Id (Integer'Value (Foot_Id)));
         end loop;
         for Bike_Id of Bike_List loop
            Bike_List_Id.Append (Infra_Id (Integer'Value (Bike_Id)));
         end loop;
         for Road_Id of Road_List loop
            Road_List_Id.Append (Infra_Id (Integer'Value (Road_Id)));
         end loop;
         Source_Slice.Include (FOOT, Foot_List_Id);
         Source_Slice.Include (BIKE, Bike_List_Id);
         Source_Slice.Include (ROAD, Road_List_Id);
         This.Source := Source_Slice;
      end;

      declare
         Foot_List         : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Destination_FOOT"), Separator);
         Bike_List         : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Destination_BIKE"), Separator);
         Road_List         : String_List.List := SSplitter.Filter_Separator (
            Stream_Map.Element ("Destination_ROAD"), Separator);
         Foot_List_Id      : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Bike_List_Id      : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Road_List_Id      : Infra_Id_List.List := Infra_Id_List.Empty_List;
         Destination_Slice : Slice.Map := Slice.Empty_Map;
      begin
         for Foot_Id of Foot_List loop
            Foot_List_Id.Append (Infra_Id (Integer'Value (Foot_Id)));
         end loop;
         for Bike_Id of Bike_List loop
            Bike_List_Id.Append (Infra_Id (Integer'Value (Bike_Id)));
         end loop;
         for Road_Id of Road_List loop
            Road_List_Id.Append (Infra_Id (Integer'Value (Road_Id)));
         end loop;
         Destination_Slice.Include (FOOT, Foot_List_Id);
         Destination_Slice.Include (BIKE, Bike_List_Id);
         Destination_Slice.Include (ROAD, Road_List_Id);
         This.Destination := Destination_Slice;
      end;
   end Unmarshalling;

   --------------
   --- PRIVATE
   --------------

   function Encode_List (Id_List : Infra_Id_List.List)
   return SU.Unbounded_String
   is
      Id         : Infra_Id;
      Id_Cursor  : Infra_Id_List.Cursor := Id_List.First;
      Look_Ahead : Infra_Id_List.Cursor := Id_List.First;
      Result     : SU.Unbounded_String := SU.To_Unbounded_String ("");
   begin
      if Id_List.Is_Empty then
         return Result;
      end if;
      Infra_Id_List.Next (Look_Ahead);

      while Infra_Id_List.Has_Element (Id_Cursor) loop
         Id     := Infra_Id_List.Element (Id_Cursor);
         declare
            Id_Str : String := Trim (Integer'Image (Integer (Id)), Both);
         begin
            SU.Append (Result, SU.To_Unbounded_String (Id_Str));
            if Infra_Id_List.Has_Element (Look_Ahead) then
               SU.Append (Result, Separator);
               Infra_Id_List.Next (Look_Ahead);
            end if;
            Infra_Id_List.Next (Id_Cursor);
         end;
      end loop;

      return Result;
   end Encode_List;

end Interface_Layer.Utils.Explorer;
