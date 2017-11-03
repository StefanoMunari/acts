-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Host_Config_Reader;
with Reactive.Infrastructure.Build.Intersection_Config_Reader;
with Reactive.Infrastructure.Build.Street_Config_Reader;

with Shared.Infra_Id_List;
with Shared.Reader;

package Reactive.Infrastructure.Build.Config_Reader is

   package G_JSON        renames GNATCOLL.JSON;
   package Infra_Id_List renames Shared.Infra_Id_List;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   function Get_Instance (
      File_Reader         : access Shared.Reader.Object'Class := null;
      Host_Reader         : access Host_Config_Reader.Object'Class := null;
      Street_Reader       : access Street_Config_Reader.Object'Class := null;
      Intersection_Reader : access Intersection_Config_Reader.Object'Class
        := null)
   return Config_Reader.Reference;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String) return Boolean;

private
   type Object is new Ada.Finalization.Controlled with record
      File_Reader         : access Shared.Reader.Object'Class := null;
      Host_Reader         : access Host_Config_Reader.Object'Class := null;
      Street_Reader       : access Street_Config_Reader.Object'Class := null;
      Intersection_Reader :
        access Intersection_Config_Reader.Object'Class := null;
   end record;

   function Read_Hosts (This  : in out Config_Reader.Object;
                        Hosts :        G_JSON.JSON_Array)
   return Infra_Id_List.List;

   function Read_Streets (This    : in out Config_Reader.Object;
                          Streets :        G_JSON.JSON_Array)
   return Infra_Id_List.List;

   function Read_Intersections (This          : in out Config_Reader.Object;
                                Intersections :        G_JSON.JSON_Array)
   return Infra_Id_List.List;

   Instance : Config_Reader.Reference;

end Reactive.Infrastructure.Build.Config_Reader;
