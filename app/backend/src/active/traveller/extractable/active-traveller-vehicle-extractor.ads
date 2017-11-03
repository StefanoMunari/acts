with Active.Traveller.Extractor;
with Active.Traveller.Vehicle;

with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Explorer.Vehicle;

package Active.Traveller.Vehicle.Extractor is

   package Traveller_Pkg    renames Active.Traveller;
   package Vehicle_Pkg      renames Active.Traveller.Vehicle;
   package Explorer         renames Interface_Layer.Utils.Explorer;
   package Explorer_Vehicle renames Interface_Layer.Utils.Explorer.Vehicle;

   type Object is abstract
      new Traveller_Pkg.Extractor.Object
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

end Active.Traveller.Vehicle.Extractor;
