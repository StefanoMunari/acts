with Ada.Unchecked_Deallocation;

with Active.Build.Traveller.Builder;
with Active.Space_Master;

with Reactive.Infrastructure.Stretch;
with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

with Shared.Reader.JSON;

package body Active.Build.Config_Reader is

   package Stretch_Pkg renames Reactive.Infrastructure.Stretch;
   package T_Builder_Pkg renames Active.Build.Traveller.Builder;

   function Get_Instance (
      File_Reader          :
         access Shared.Reader.Object'Class := null;
      Traffic_Light_Reader :
        access Traffic_Light_Config_Reader.Object'Class := null;
      Traveller_Reader   :
        access Traveller.Config_Reader.Object'Class := null)
   return Config_Reader.Reference
   is
   begin
      if Instance = null then
         Instance := new Config_Reader.Object;
      end if;

      Instance.File_Reader := File_Reader;
      if Instance.File_Reader = null then
         Instance.File_Reader :=
            new Shared.Reader.JSON.Object;
      end if;

      Instance.Traffic_Light_Reader := Traffic_Light_Reader;
      if Instance.Traffic_Light_Reader = null then
         Instance.Traffic_Light_Reader :=
            new Traffic_Light_Config_Reader.Object;
      end if;

      Instance.Traveller_Reader := Traveller_Reader;
      if Instance.Traveller_Reader = null then
         Instance.Traveller_Reader :=
            new Traveller.Config_Reader.Object;
      end if;

      return Instance;
   end Get_Instance;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String)
   return Boolean is
      Node          : G_JSON.JSON_Value;
      Max_Speed      : Integer;
      Travellers     : Agent_Id_List.List;
      Traffic_Lights : Agent_Id_List.List;
   -- json fields
      Max_Speed_Field      : constant String := "maxSpeed";
      Travellers_Field     : constant String := "travellers";
      Traffic_Lights_Field : constant String := "trafficLights";
   begin
      Node := This.File_Reader.Parse (File_Path);

      if not Node.Has_Field (Max_Speed_Field) then
         Raise_Missing_Field_For_District (Max_Speed_Field);
      end if;

      Max_Speed := Node.Get (Max_Speed_Field);

      Space_Master.Get_Instance.Set_Max_Speed (Max_Speed);

      if not Node.Has_Field (Stretch_Pkg.Travellers_Field) then
         Raise_Missing_Field_For_District (Stretch_Pkg.Travellers_Field);
      end if;
      Travellers :=
         This.Read_Travellers (Node.Get (Stretch_Pkg.Travellers_Field));

      if not Node.Has_Field (Traffic_Lights_Field) then
         Raise_Missing_Field_For_District (Traffic_Lights_Field);
      end if;
      Traffic_Lights :=
         This.Read_Traffic_Lights (Node.Get (Traffic_Lights_Field).Get);

      return True;
   end Read_Config;

   function Read_Travellers (This       : in out Config_Reader.Object;
                             Travellers :        G_JSON.JSON_Array)
   return Agent_Id_List.List
   is
      Number_Of_Travellers : Natural;
      Traveller            : G_JSON.JSON_Value;
      Traveller_Ids        : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Builder_Ref          : T_Builder_Pkg.Reference;
      procedure Free is new
         Ada.Unchecked_Deallocation (
            T_Builder_Pkg.Object'Class,
            T_Builder_Pkg.Reference);
   begin
      Number_Of_Travellers := G_JSON.Length (Travellers);
      for I in 1 .. Number_Of_Travellers loop
         Builder_Ref := new T_Builder_Pkg.Object;
         This.Traveller_Reader.Set_Builder (Builder_Ref);
         Traveller := G_JSON.Get (Travellers, I);

         Traveller_Ids.Append (This.Traveller_Reader.Read (Traveller));
         Free (Builder_Ref);
      end loop;

      return Traveller_Ids;
   end Read_Travellers;

   function Read_Traffic_Lights (This           : in out Config_Reader.Object;
                                 Traffic_Lights :        G_JSON.JSON_Array)
   return Agent_Id_List.List
   is
      Number_Of_Traffic_Lights : Natural;
      Traffic_Light            : G_JSON.JSON_Value;
      Traffic_Light_Ids        : Agent_Id_List.List
         := Agent_Id_List.Empty_List;
   begin
      Number_Of_Traffic_Lights := G_JSON.Length (Traffic_Lights);
      for I in 1 .. Number_Of_Traffic_Lights loop
         Traffic_Light := G_JSON.Get (Traffic_Lights, I);
         Traffic_Light_Ids.Append (
            This.Traffic_Light_Reader.Read (Traffic_Light));
      end loop;

      return Traffic_Light_Ids;
   end Read_Traffic_Lights;

end Active.Build.Config_Reader;
