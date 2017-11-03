with Reactive;
use Reactive.Infra_Id_Type;

package Shared.Containable is

   type Object is interface;
   type Reference is access all Containable.Object'Class;

   not overriding
   function Is_Contained_By (This         : in Containable.Object;
                             Container_Id : in Infra_Id)
   return Boolean is abstract;

end Shared.Containable;
