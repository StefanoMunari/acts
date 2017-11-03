-- gnatcoll libs
with GNATCOLL.JSON;

package Shared.Dumpable is

   package G_JSON renames GNATCOLL.JSON;

   type Object is interface;
   type Reference is access all Dumpable.Object'Class;

   function Dump (This : in Shared.Dumpable.Object) return G_JSON.JSON_Value
   is abstract;

end Shared.Dumpable;
