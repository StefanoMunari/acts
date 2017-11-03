with Active.Traveller;
with Active.Traveller.Pedestrian;
with Active.Traveller.Extractor;

with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Explorer.Pedestrian;

package Active.Traveller.Pedestrian.Extractor is

   package Traveller_Extractor renames Active.Traveller.Extractor;
   package Traveller_Pkg       renames Active.Traveller;
   package Explorer            renames Interface_Layer.Utils.Explorer;

   type Object is
      new Traveller_Extractor.Object
   with null record;
   type Reference is access all Extractor.Object'Class;

   overriding
   procedure Extract (This     : in     Extractor.Object;
                      Instance : in     Traveller_Pkg.Object'Class;
                      Result   : in out Explorer.Object'Class);

   overriding
   procedure Extract (This     : in     Extractor.Object;
                      Instance : in     Explorer.Object'Class;
                      Result   : in out Traveller_Pkg.Object'Class);

end Active.Traveller.Pedestrian.Extractor;
