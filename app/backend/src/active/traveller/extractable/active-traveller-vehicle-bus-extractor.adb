with Ada.Unchecked_Deallocation;

with Active.Travel.Bus_Travel;
with Active.Travel.Travel_Progress;

with Interface_Layer.Utils.Explorer.Vehicle.Bus;

package body Active.Traveller.Vehicle.Bus.Extractor is

   package Bus_Travel_Pkg      renames Active.Travel.Bus_Travel;
   package Travel_Progress_Pkg renames Active.Travel.Travel_Progress;
   package Bus_Explorer_Pkg
      renames Interface_Layer.Utils.Explorer.Vehicle.Bus;

   ----------------------
   --- BUS -> EXPLORER
   ----------------------

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Traveller_Pkg.Object'Class;
      Result   : in out Explorer.Object'Class)
   is
      Bus_Obj : Bus.Object'Class := Bus.Object'Class (Instance);
   begin
   -- UPCAST
   --   This : Bus.Extractor => This : Vehicle.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Explorer.Vehicle.Bus)
   --   Result inherits the record from Explorer.Vehicle
      Interface_Layer.Utils.Explorer.Vehicle.Bus.Init (
         Bus_Explorer_Pkg.Object (Result),
         Route_Stops => Bus_Obj.Route_Stops,
         Bus_Stops => Bus_Obj.Bus_Stops);
      Vehicle_Extractor.Extract (
         Vehicle_Extractor.Object (This), Instance, Result);
   end Extract;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Explorer.Object'Class;
      Result   : in out Traveller_Pkg.Object'Class)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Bus.Object'Class, Bus.Reference);
      Bus_Explorer : Bus_Explorer_Pkg.Object'Class :=
         Bus_Explorer_Pkg.Object'Class (Instance);
      Bus_Ref     : Bus.Reference;
      Result_Id   : Agent.Agent_Id := Instance.Id;
      Source      : Slice.Map      := Instance.Source;
      Destination : Slice.Map      := Instance.Destination;
   begin
   -- UPCAST
   --   This : Bus.Extractor => This : Vehicle.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Traveller.Vehicle.Bus)
   --   Result inherits the record from Traveller.Vehicle
      Bus_Ref := Bus.Create (
         Id             => Result_Id,
         Maximum_Speed  => Instance.Maximum_Speed,
         Max_Passengers => Bus_Explorer.Max_Passengers,
         Bus_Stops      => Bus_Explorer.Bus_Stops,
         Route_Stops    => Bus_Explorer.Route_Stops,
         Travel_Ref     => Bus_Travel_Pkg.Create (
            Source, Destination, Result_Id, Travel_Progress_Pkg.Get_Instance)
      );

      Result := Bus_Ref.all;
      Vehicle_Extractor.Object (This).Extract (Instance, Result);
      Result.Travel_Ref.Init_Route (Instance.Route);

      Free (Bus_Ref);
   end Extract;

end Active.Traveller.Vehicle.Bus.Extractor;
