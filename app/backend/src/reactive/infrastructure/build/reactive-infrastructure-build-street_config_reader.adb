-- core
with Ada.Finalization;

-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Street_Builder;
with Reactive.Infrastructure.Street;

with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;


package body Reactive.Infrastructure.Build.Street_Config_Reader is

   package Street_Pkg renames Reactive.Infrastructure.Street;
   use Street_Pkg;

   procedure Set_Builder (
      This    :    out Street_Config_Reader.Object;
      Builder : access Street_Builder.Object'Class := null) is
   begin
      This.Builder := Builder;
   end Set_Builder;

   function Read (This        : in out Street_Config_Reader.Object;
                  Street_Json : in     G_JSON.JSON_Value) return Infra_Id is
      Bikeways           : G_JSON.JSON_Array;
      Bikeway            : G_JSON.JSON_Value;
      Number_Of_Bikeways : Natural;
      Footways           : G_JSON.JSON_Array;
      Footway            : G_JSON.JSON_Value;
      Number_Of_Footways : Natural;
      Roadways           : G_JSON.JSON_Array;
      Roadway            : G_JSON.JSON_Value;
      Number_Of_Roadways : Natural;
      Bikeway_Id         : Infra_Id;
      Footway_Id         : Infra_Id;
      Roadway_Id         : Infra_Id;
      Street_Id_Int      : Integer;
      Street_Id          : Infra_Id;
   begin
      This.Builder.Reset;

      if not Street_Json.Has_Field (Street_Pkg.Id_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Id_Field);
      end if;
      Street_Id_Int := Street_Json.Get (Street_Pkg.Id_Field);
      Street_Id := Infra_Id (Street_Id_Int);

      if not Street_Json.Has_Field (Street_Pkg.Bikeways_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Bikeways_Field);
      end if;
      Bikeways := Street_Json.Get (Street_Pkg.Bikeways_Field);
      Number_Of_Bikeways := G_JSON.Length (Bikeways);
      for I in 1 .. Number_Of_Bikeways loop
         Bikeway := G_JSON.Get (Bikeways, I);
         Bikeway_Id := This.Builder.With_Bikeway (Bikeway);
      end loop;

      if not Street_Json.Has_Field (Street_Pkg.Footways_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Footways_Field);
      end if;
      Footways := Street_Json.Get (Street_Pkg.Footways_Field);
      Number_Of_Footways := G_JSON.Length (Footways);
      for I in 1 .. Number_Of_Footways loop
         Footway := G_JSON.Get (Footways, I);
         Footway_Id := This.Builder.With_Footway (Footway);
      end loop;

      if not Street_Json.Has_Field (Street_Pkg.Roadways_Field) then
         Raise_Missing_Field_For_Street (Street_Pkg.Roadways_Field);
      end if;
      Roadways := Street_Json.Get (Street_Pkg.Roadways_Field);
      Number_Of_Roadways := G_JSON.Length (Roadways);
      for I in 1 .. Number_Of_Roadways loop
         Roadway := G_JSON.Get (Roadways, I);
         Roadway_Id := This.Builder.With_Roadway (Roadway);
      end loop;

      Street_Id := This.Builder.Get_Street (Street_Json);

      return Street_Id;
   end Read;

end Reactive.Infrastructure.Build.Street_Config_Reader;
