-- core
with Ada.Strings.Unbounded;
with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

package body Reactive.Infrastructure.Build.Host_Config_Reader is

   procedure Set_Builder (
      This    :    out Host_Config_Reader.Object;
      Builder : access Host_Builder.Object'Class := null) is
   begin
      This.Builder := Builder;
   end Set_Builder;

   function Read (This      : in out Host_Config_Reader.Object;
                  Host_Json : in     G_JSON.JSON_Value)
   return Infra_Id
   is
   begin
      -- GARAGE
      if not Host_Json.Has_Field (Garage_Field) then
         Raise_Missing_Field_For_Host (Garage_Field);
      end if;

      This.Builder.all.With_Parking_Manager (Host_Json.Get (Garage_Field));

      return This.Builder.all.Get_Host (Host_Json);
   end Read;

end Reactive.Infrastructure.Build.Host_Config_Reader;
