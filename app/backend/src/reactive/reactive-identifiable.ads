package Reactive.Identifiable is

   use Reactive.Infra_Id_Type;

   type Object is interface;
   type Reference is access all Identifiable.Object'Class;

   not overriding
   function Get_Id (This : in Identifiable.Object)
   return Infra_Id is abstract;

end Reactive.Identifiable;
