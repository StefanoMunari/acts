with Active.Traveller;

with Interface_Layer.Utils.Explorer;

package Shared.Extractable is

   type Object is interface;
   type Reference is access all Extractable.Object'Class;

   procedure Extract (
      This     : in     Extractable.Object;
      Instance : in     Active.Traveller.Object'Class;
      Result   : in out Interface_Layer.Utils.Explorer.Object'Class)
   is abstract;

   procedure Extract (
      This     : in     Extractable.Object;
      Instance : in     Interface_Layer.Utils.Explorer.Object'Class;
      Result   : in out Active.Traveller.Object'Class)
   is abstract;

end Shared.Extractable;
