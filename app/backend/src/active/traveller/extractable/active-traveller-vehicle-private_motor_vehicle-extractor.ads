with Active.Traveller.Vehicle.Extractor;

with Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle;

with Shared.Extractable;

package Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor is

   package Vehicle_Extractor renames Active.Traveller.Vehicle.Extractor;
   package Traveller_Pkg     renames Active.Traveller;
   package Explorer          renames Interface_Layer.Utils.Explorer;

   type Object is
      new Vehicle_Extractor.Object
   with null record;
   type Reference is access all Extractor.Object'Class;

   overriding
   procedure Extract (This     :        Extractor.Object;
                      Instance : in     Traveller_Pkg.Object'Class;
                      Result   : in out Explorer.Object'Class);

   overriding
   procedure Extract (This     : in     Extractor.Object;
                      Instance : in     Explorer.Object'Class;
                      Result   : in out Traveller_Pkg.Object'Class);

end Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor;
