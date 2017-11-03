with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Utils.Mock;

with Reactive.District.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Lane.Utils.Mock;
with Reactive.Infrastructure.Intersection.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;
with Active.Traveller.Utils.Mock;
with Reactive.Infrastructure.Intersection.Crossing.Mock;

package Reactive.Infrastructure.Intersection.Intersection_Builder.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Intersection_Builder_Test is abstract new TC.Test_Case with record
      Intersection_Id      : Infra_Id;
      Intersection_Builder : Intersection.Intersection_Builder.Reference;
      District             : Reactive.District.Reference;
      Street_Utils         :
         Reactive.Infrastructure.Street.Utils.Mock.Reference;
      Intersection_Utils   :
         Reactive.Infrastructure.Intersection.Utils.Mock.Reference;
      Lane_Utils           : Reactive.Infrastructure.Lane.Utils.Mock.Reference;
      Intersection         : Infrastructure.Intersection.Mock.Reference;
      Traveller_Utils      : Active.Traveller.Utils.Mock.Reference;
      Crossing_Strategy    :
         Reactive.Infrastructure.Intersection.Crossing.Mock.Reference;
   end record;

   -- Test Routines:
   procedure Test_Result_Getter (
      T : in out Intersection_Builder_Test);
   procedure Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection (
      T : in out Intersection_Builder_Test);
   procedure Test_Connection_To_A_Street_Without_Intersections (
      T : in out Intersection_Builder_Test);

   procedure Register_Tests (T : in out Intersection_Builder_Test);

end Reactive.Infrastructure.Intersection.Intersection_Builder.Tests;
