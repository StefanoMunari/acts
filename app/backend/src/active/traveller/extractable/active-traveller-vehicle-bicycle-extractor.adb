with Ada.Unchecked_Deallocation;

with Active.Travel.Travel_Progress;

with Interface_Layer.Utils.Explorer.Vehicle;

package body Active.Traveller.Vehicle.Bicycle.Extractor is

   package Travel_Progress_Pkg renames Active.Travel.Travel_Progress;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Traveller_Pkg.Object'Class;
      Result   : in out Explorer.Object'Class)
   is
   begin
   -- UPCAST
   --   This : Bicycle.Extractor => This : Vehicle.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Explorer.Vehicle.Bicycle)
   --   Result inherits the record from Explorer.Vehicle
      Vehicle_Extractor.Extract (
         Vehicle_Extractor.Object (This), Instance, Result);
   end Extract;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Explorer.Object'Class;
      Result   : in out Traveller_Pkg.Object'Class)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Bicycle.Object'Class, Bicycle.Reference);
      Bicycle_Ref : Bicycle.Reference;
      Result_Id   : Agent.Agent_Id := Instance.Id;
      Source      : Slice.Map      := Instance.Source;
      Destination : Slice.Map      := Instance.Destination;
   begin
   -- UPCAST
   --   This : Bicycle.Extractor => This : Vehicle.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Traveller.Vehicle.Bicycle)
   --   Result inherits the record from Traveller.Vehicle
      Bicycle_Ref := Bicycle.Create (
         Id             => Result_Id,
         Maximum_Speed  => Instance.Maximum_Speed,
         Max_Passengers => Explorer.Vehicle.Object (Instance).Max_Passengers,
         Travel_Ref     => Active.Travel.Create (
            Source, Destination, Result_Id, Travel_Progress_Pkg.Get_Instance)
      );

      Result := Bicycle_Ref.all;
      Vehicle_Extractor.Object (This).Extract (Instance, Result);
      Result.Travel_Ref.Init_Route (Instance.Route);

      Free (Bicycle_Ref);
   end Extract;

end Active.Traveller.Vehicle.Bicycle.Extractor;
