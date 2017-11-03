-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Intersection;
with Reactive.Infrastructure.Intersection.Intersection_Builder;

package Reactive.Infrastructure.Build.Intersection_Config_Reader is

   package G_JSON           renames GNATCOLL.JSON;
   package Intersection_Pkg renames Reactive.Infrastructure.Intersection;
   package Intersection_Builder_Pkg
      renames Intersection_Pkg.Intersection_Builder;
   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Set_Builder (
      This    :    out Intersection_Config_Reader.Object;
      Builder : access Intersection_Builder_Pkg.Object'Class := null);

   not overriding
   function Read (This              : in out Intersection_Config_Reader.Object;
                  Intersection_Json : in     G_JSON.JSON_Value)
   return Infra_Id;

-- JSON Fields
   Exits_Field         : constant String := Intersection_Pkg.Exits_Field;
   Street_Id_Field     : constant String := Intersection_Pkg.Street_Id_Field;
   Stretch_Id_Field    : constant String := Intersection_Pkg.Stretch_Id_Field;
   Direction_Field     : constant String := Intersection_Pkg.Direction_Field;
   Traffic_Light_Field : constant String
      := Intersection_Pkg.Traffic_Light_Field;

private
   type Object is new Ada.Finalization.Controlled with record
      Builder : access Intersection_Builder_Pkg.Object'Class := null;
   end record;

end Reactive.Infrastructure.Build.Intersection_Config_Reader;
