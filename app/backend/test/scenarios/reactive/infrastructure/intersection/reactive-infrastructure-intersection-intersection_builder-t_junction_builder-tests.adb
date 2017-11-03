with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.District.Mock;
with Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;
with Reactive.Infrastructure.Lane.Utils.Mock;
with Reactive.Infrastructure.Intersection.Mock;
with Reactive.Infrastructure.Intersection.Crossing.Mock;
with Active.Traveller.Utils.Mock;

package body Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder.Tests is
   package Ass renames AUnit.Assertions;
   package District_Mock renames Reactive.District.Mock;
   package Street_Utils renames Reactive.Infrastructure.Street.Utils.Mock;
   package Intersection_Utils renames Reactive.Infrastructure.Intersection.Utils.Mock;
   package Lane_Utils renames Reactive.Infrastructure.Lane.Utils.Mock;
   package Intersection renames Reactive.Infrastructure.Intersection.Mock;
   package Traveller_Utils renames Active.Traveller.Utils.Mock;
   package Crossing_Strategy renames Reactive.Infrastructure.Intersection.Crossing.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out T_Junction_Builder_Test) is
   begin
      T.Intersection_Id := 235;
      T.Intersection := Intersection.Create;
      T.District           :=
        Reactive.District.Reference (District_Mock.Create);
      T.Street_Utils := Street_Utils.Create;
      T.Intersection_Utils := Intersection_Utils.Create;
      T.Lane_Utils := Lane_Utils.Create;
      T.Traveller_Utils := Traveller_Utils.Create;
      T.Crossing_Strategy := Crossing_Strategy.Create;

      T.Intersection_Builder
        := Intersection_Builder.Reference
          (T_Junction_Builder
           .Create (Intersection_Id    => T.Intersection_Id,
                    District           => T.District,
                    Street_Utils       => T.Street_Utils,
                    Intersection_Utils => T.Intersection_Utils,
                    Lane_Utils         => T.Lane_Utils,
                    Intersection       => T.Intersection,
                    Traveller_Utils    => T.Traveller_Utils,
                    Crossing_Strategy  => T.Crossing_Strategy));

      Intersection_Builder.Tests.Set_Up
        (Intersection_Builder.Tests.Intersection_Builder_Test (T));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out T_Junction_Builder_Test) is
   begin
      Intersection_Builder
        .Tests.Register_Tests
          (Intersection_Builder.Tests.Intersection_Builder_Test (T));
   end Register_Tests;

   function Name(T: T_Junction_Builder_Test) return AU.Message_String is
   begin
      return AU.Format ("T_Junction_Builder");
   end Name;
end Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder.Tests;
