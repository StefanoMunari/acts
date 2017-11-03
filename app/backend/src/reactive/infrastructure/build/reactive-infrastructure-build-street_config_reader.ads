-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Street_Builder;

package Reactive.Infrastructure.Build.Street_Config_Reader is

   package G_JSON renames GNATCOLL.JSON;
   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Set_Builder (
      This    :    out Street_Config_Reader.Object;
      Builder : access Street_Builder.Object'Class := null);

   not overriding
   function Read (This        : in out Street_Config_Reader.Object;
                  Street_Json : in     G_JSON.JSON_Value) return Infra_Id;

private
   type Object is new Ada.Finalization.Controlled with record
      Builder : access Street_Builder.Object'Class := null;
   end record;

end Reactive.Infrastructure.Build.Street_Config_Reader;
