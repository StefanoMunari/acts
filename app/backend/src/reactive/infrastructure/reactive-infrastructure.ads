-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Identifiable;

with Shared.Containable;
with Shared.Dumpable;

package Reactive.Infrastructure is

   package G_JSON       renames GNATCOLL.JSON;
   package Identifiable renames Reactive.Identifiable;
   package Containable  renames Shared.Containable;
   package Dumpable     renames Shared.Dumpable;
   use Reactive.Infra_Id_Type;

   type Object is interface
     and Containable.Object
     and Identifiable.Object
     and Dumpable.Object;
   type Reference is access all Infrastructure.Object'Class;

end Reactive.Infrastructure;
