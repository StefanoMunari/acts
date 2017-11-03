with Ada.Unchecked_Deallocation;

with Active.Travel.Travel_Progress;

with Interface_Layer.Utils.Explorer.Pedestrian;

package body Active.Traveller.Pedestrian.Extractor is

   package Travel_Progress_Pkg renames Active.Travel.Travel_Progress;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Traveller_Pkg.Object'Class;
      Result   : in out Explorer.Object'Class)
   is
   begin
   -- UPCAST
   --   This : Pedestrian.Extractor => This : Traveller.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Explorer.Pedestrian)
   --   Result inherits the record from Explorer
      Traveller_Extractor.Extract (
         Traveller_Extractor.Object (This), Instance, Result);
   end Extract;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Explorer.Object'Class;
      Result   : in out Traveller_Pkg.Object'Class)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Pedestrian.Object'Class, Pedestrian.Reference);
      Pedestrian_Ref : Pedestrian.Reference;
      Result_Id      : Agent.Agent_Id := Instance.Id;
      Source         : Slice.Map      := Instance.Source;
      Destination    : Slice.Map      := Instance.Destination;
   begin
   -- UPCAST
   --   This : Pedestrian.Extractor => This : Traveller.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Traveller.Pedestrian)
   --   Result inherits the record from Traveller
      Pedestrian_Ref := Pedestrian.Create (
         Id             => Result_Id,
         Maximum_Speed  => Instance.Maximum_Speed,
         Travel_Ref     => Active.Travel.Create (
            Source, Destination, Result_Id, Travel_Progress_Pkg.Get_Instance)
      );

      Result := Pedestrian_Ref.all;
      Traveller_Extractor.Object (This).Extract (Instance, Result);
      Result.Travel_Ref.Init_Route (Instance.Route);

      Free (Pedestrian_Ref);
   end Extract;

end Active.Traveller.Pedestrian.Extractor;
