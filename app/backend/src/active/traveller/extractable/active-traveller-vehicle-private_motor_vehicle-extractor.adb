with Ada.Unchecked_Deallocation;

with Active.Travel.Travel_Progress;

package body Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor is

   package Travel_Progress_Pkg renames Active.Travel.Travel_Progress;
   package PMV_Pkg
      renames Active.Traveller.Vehicle.Private_Motor_Vehicle;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Traveller_Pkg.Object'Class;
      Result   : in out Explorer.Object'Class)
   is
      Veh_Type : PMV_Pkg.Private_Motor_Vehicle_Type;
   begin
   -- UPCAST
   --   This : PMV.Extractor => This : Vehicle.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Explorer.Vehicle.PMV)
   --   Result inherits the record from Explorer.Vehicle
      Vehicle_Extractor.Extract (
         Vehicle_Extractor.Object (This), Instance, Result);
      Veh_Type := PMV_Pkg.Object (Instance).Vehicle_Type;
      Explorer.Vehicle.Private_Motor_Vehicle.Init (
         Explorer.Vehicle.Private_Motor_Vehicle.Object (Result),
         Veh_Type);
   end Extract;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Explorer.Object'Class;
      Result   : in out Traveller_Pkg.Object'Class)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         PMV_Pkg.Object'Class, PMV_Pkg.Reference);
      PVM         : PMV_Pkg.Reference;
      Result_Id   : Agent.Agent_Id := Instance.Id;
      Source      : Slice.Map      := Instance.Source;
      Destination : Slice.Map      := Instance.Destination;
   begin
   -- UPCAST
   --   This : PVM.Extractor => This : Vehicle.Extractor
   --   To obtain an Traveller (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Traveller.Vehicle.PVM)
   --   Result inherits the record from Traveller.Vehicle
      PVM := PMV_Pkg.Create (
         Id             => Result_Id,
         Maximum_Speed  => Instance.Maximum_Speed,
         Max_Passengers => Explorer.Vehicle.Object (Instance).Max_Passengers,
         Vehicle_Type   =>
            Explorer.Vehicle.Private_Motor_Vehicle.Object (Instance)
            .Vehicle_Type,
         Travel_Ref     => Active.Travel.Create (
            Source, Destination, Result_Id, Travel_Progress_Pkg.Get_Instance)
      );

      Result := PVM.all;
      Vehicle_Extractor.Object (This).Extract (Instance, Result);
      Result.Travel_Ref.Init_Route (Instance.Route);

      Free (PVM);
   end Extract;

end Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor;
