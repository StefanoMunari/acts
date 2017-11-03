-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Host_Builder;
with Reactive.Infrastructure.Building.Host;

package Reactive.Infrastructure.Build.Host_Config_Reader is

-- libs
   package G_JSON renames GNATCOLL.JSON;
-- reactive
   package Host_Builder renames Reactive.Infrastructure.Build.Host_Builder;
   package Host_Pkg
      renames Reactive.Infrastructure.Building.Host;
-- use
   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Set_Builder (
      This    :    out Host_Config_Reader.Object;
      Builder : access Host_Builder.Object'Class := null);

   not overriding
   function Read (This      : in out Host_Config_Reader.Object;
                  Host_Json : in     G_JSON.JSON_Value) return Infra_Id;

private
   type Object is new Ada.Finalization.Controlled with record
      Builder : access Host_Builder.Object'Class := null;
   end record;

   Garage_Field : constant String := Host_Pkg.Garage_Field;

end Reactive.Infrastructure.Build.Host_Config_Reader;
