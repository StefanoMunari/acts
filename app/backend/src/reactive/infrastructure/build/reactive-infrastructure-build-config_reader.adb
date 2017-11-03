with Reactive.Infrastructure.Build.Host_Builder;
with Reactive.Infrastructure.Build.Street_Builder;
with Reactive.Infrastructure.Intersection.Intersection_Builder;
with Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder;
with Reactive.Infrastructure.Intersection.Intersection_Builder.Crossroads_Builder;

with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

with Shared.Reader.JSON;

package body Reactive.Infrastructure.Build.Config_Reader is

   package Intersection_Builder
      renames Reactive.Infrastructure.Intersection.Intersection_Builder;
   package Crossroads_Builder renames Intersection_Builder.Crossroads_Builder;
   package T_Junction_Builder renames Intersection_Builder.T_Junction_Builder;
   use Infra_Id_Type;

   function Get_Instance (
      File_Reader         : access Shared.Reader.Object'Class := null;
      Host_Reader         : access Host_Config_Reader.Object'Class := null;
      Street_Reader       : access Street_Config_Reader.Object'Class := null;
      Intersection_Reader : access Intersection_Config_Reader.Object'Class
        := null)
   return Config_Reader.Reference is
   begin
      if Instance = null then
         Instance := new Config_Reader.Object;
      end if;

      Instance.File_Reader := File_Reader;
      if Instance.File_Reader = null then
         Instance.File_Reader := new Shared.Reader.JSON.Object;
      end if;

      Instance.Host_Reader := Host_Reader;
      if Instance.Host_Reader = null then
         Instance.Host_Reader := new Host_Config_Reader.Object;
      end if;

      Instance.Street_Reader := Street_Reader;
      if Instance.Street_Reader = null then
         Instance.Street_Reader := new Street_Config_Reader.Object;
      end if;

      Instance.Intersection_Reader := Intersection_Reader;
      if Instance.Intersection_Reader = null then
         Instance.Intersection_Reader := new Intersection_Config_Reader.Object;
      end if;

      return Instance;
   end Get_Instance;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String)
   return Boolean is
      Node          : G_JSON.JSON_Value;
      Facilities    : Infra_Id_List.List;
      Streets       : Infra_Id_List.List;
      Intersections : Infra_Id_List.List;
   -- json fields
      Hosts_Field         : constant String := "facilities";
      Streets_Field       : constant String := "streets";
      Intersections_Field : constant String := "intersections";
   begin
      Node := This.File_Reader.Parse (File_Path);

      if not Node.Has_Field (Hosts_Field) then
         Raise_Missing_Field_For_District (Hosts_Field);
      end if;
      Facilities := This.Read_Hosts (Node.Get (Hosts_Field).Get);

      if not Node.Has_Field (Streets_Field) then
         Raise_Missing_Field_For_District (Streets_Field);
      end if;
      Streets := This.Read_Streets (Node.Get (Streets_Field).Get);

      if not Node.Has_Field (Intersections_Field) then
         Raise_Missing_Field_For_District (Intersections_Field);
      end if;
      Intersections := This.Read_Intersections (
         Node.Get (Intersections_Field).Get);

      return True;
   end Read_Config;

-----------
--- private
-----------

   function Read_Hosts (This  : in out Config_Reader.Object;
                        Hosts :        G_JSON.JSON_Array)
   return Infra_Id_List.List
   is
      Builder         :
         Reactive.Infrastructure.Build.Host_Builder.Reference
            := Reactive.Infrastructure.Build.Host_Builder.Create;
      Number_Of_Hosts : Natural;
      Host_Ids        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Host            : G_JSON.JSON_Value;
   begin
      This.Host_Reader.Set_Builder (Builder);

      Number_Of_Hosts := G_JSON.Length (Hosts);
      for I in 1 .. Number_Of_Hosts loop
         Host := G_JSON.Get (Hosts, I);
         Host_Ids.Append (This.Host_Reader.Read (Host));
      end loop;
      return Host_Ids;
   end Read_Hosts;

   function Read_Streets (This    : in out Config_Reader.Object;
                          Streets :        G_JSON.JSON_Array)
   return Infra_Id_List.List
   is
      Builder           :
         Reactive.Infrastructure.Build.Street_Builder.Reference
            := Reactive.Infrastructure.Build.Street_Builder.Create;
      Number_Of_Streets : Natural;
      Street_Ids        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Street            : G_JSON.JSON_Value;
   begin
      This.Street_Reader.Set_Builder (Builder);

      Number_Of_Streets := G_JSON.Length (Streets);

      for I in 1 .. Number_Of_Streets loop
         Street := G_JSON.Get (Streets, I);
         Street_Ids.Append (This.Street_Reader.Read (Street));
      end loop;

      return Street_Ids;
   end Read_Streets;

   function Read_Intersections (This          : in out Config_Reader.Object;
                                Intersections :        G_JSON.JSON_Array)
   return Infra_Id_List.List
   is
      Builder                 : Intersection_Builder.Reference;
      T_Builder               : T_Junction_Builder.Reference;
      X_Builder               : Crossroads_Builder.Reference;
      Number_Of_Intersections : Natural;
      Intersection            : G_JSON.JSON_Value;
      Exits                   : G_JSON.JSON_Array;
      Intersection_Id         : Infra_Id;
      Intersection_Id_Int     : Integer;
      Intersection_Ids        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Id_Field                : constant String := "id";
      Exits_Field             : constant String := "exits";
   begin
      Number_Of_Intersections := G_JSON.Length (Intersections);

      for I in 1 .. Number_Of_Intersections loop
         Intersection := G_JSON.Get (Intersections, I);

         if not Intersection.Has_Field (Id_Field) then
            Raise_Missing_Field_For_Intersection (Id_Field);
         end if;
         Intersection_Id_Int := Intersection.Get (Id_Field);
         Intersection_Id     := Infra_Id (Natural (Intersection_Id_Int));

         if not Intersection.Has_Field (Exits_Field) then
            Raise_Missing_Field_For_Intersection (Exits_Field);
         end if;
         Exits := Intersection.Get (Exits_Field);
      -- Raise exception if entries \not \in {3, 4}
         if G_JSON.Length (Exits) /= 3 and G_JSON.Length (Exits) /= 4 then
            null;
         else
            if G_JSON.Length (Exits) = 3 then
            -- T-junction builder
               T_Builder := T_Junction_Builder.Create (Intersection_Id);
               Builder   := Intersection_Builder.Reference (T_Builder);
            else
            -- X-junction builder
               X_Builder := Crossroads_Builder.Create (Intersection_Id);
               Builder   := Intersection_Builder.Reference (X_Builder);
            end if;

            This.Intersection_Reader.Set_Builder (Builder);
            Intersection_Id := This.Intersection_Reader.Read (Intersection);

            Intersection_Ids.Append (Intersection_Id);
         end if;
      end loop;

      return Intersection_Ids;
   end Read_Intersections;

end Reactive.Infrastructure.Build.Config_Reader;
