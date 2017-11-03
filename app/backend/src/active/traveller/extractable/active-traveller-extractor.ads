with Active.Traveller;

with Interface_Layer.Utils.Explorer;

with Shared.Extractable;
with Shared.Natural_List;

package Active.Traveller.Extractor is

   package Traveller_Pkg       renames Active.Traveller;
   package Explorer            renames Interface_Layer.Utils.Explorer;
   package Natural_List        renames Shared.Natural_List;

   type Object is abstract
      new Shared.Extractable.Object
   with null record;
   type Reference is access all Extractor.Object'Class;

   overriding
   procedure Extract (This      : in Extractor.Object;
                      Traveller : in Traveller_Pkg.Object'Class;
                      Result    : in out Explorer.Object'Class);
   overriding
   procedure Extract (This     : in     Extractor.Object;
                      Instance : in     Explorer.Object'Class;
                      Result   : in out Traveller_Pkg.Object'Class);

end Active.Traveller.Extractor;
