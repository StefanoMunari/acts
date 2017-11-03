with Reactive.Treadable;
with Reactive.Infrastructure;
limited with Reactive.Infrastructure.Street;

package Reactive.Infrastructure.Street_Related_Infrastructure is

   use Reactive.Infra_Id_Type;

   type Object is interface and Infrastructure.Object;
   type Reference is access all Street_Related_Infrastructure.Object'Class;

   not overriding
   function Find_Street (This : in Street_Related_Infrastructure.Object)
   return Infra_Id is abstract;

end Reactive.Infrastructure.Street_Related_Infrastructure;
