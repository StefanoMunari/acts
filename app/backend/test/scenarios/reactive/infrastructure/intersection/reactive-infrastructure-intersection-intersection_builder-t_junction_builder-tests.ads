with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Intersection.Intersection_Builder.Tests;

package Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type T_Junction_Builder_Test is new Intersection_Builder.Tests.Intersection_Builder_Test with null record;

   overriding procedure Set_Up (T: in out T_Junction_Builder_Test);

   procedure Register_Tests (T: in out T_Junction_Builder_Test);
   overriding function Name (T: in T_Junction_Builder_Test) return AU.Message_String;

end Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder.Tests;
