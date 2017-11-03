package Shared.Identifiable is

   type Object is interface;
   type Reference is access all Identifiable.Object'Class;

   not overriding
   function Get_Id (This : in Shared.Identifiable.Object)
                    return Natural is abstract;

end Shared.Identifiable;
