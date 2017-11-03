with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;
with Active.Travel.Mock;
with Active.Traveller;

package Active.Traveller.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Traveller_Test is abstract new TC.Test_Case with record
      Traveller            : Active.Traveller.Reference;
      Infrastructure_Utils : Reactive.Infrastructure.Utils.Mock.Reference;
      Intersection_Utils   :
         Reactive.Infrastructure.Intersection.Utils.Mock.Reference;
      Travel_Ref           : Active.Travel.Mock.Reference;
      Current_Position     : Infra_Id;
      Scheduled_For        : Float;
   end record;

   -- Test Routines:
   procedure Test_Traveller_Id_Getter (T : in out Traveller_Test);
   procedure Test_Position_Getter (T : in out Traveller_Test);
   procedure Test_Position_Setter (T : in out Traveller_Test);
   procedure Test_Maximum_Speed_Getter (T : in out Traveller_Test);
   procedure Test_Current_Speed_Getter (T : in out Traveller_Test);
   procedure Test_Current_Speed_Setter (T : in out Traveller_Test);
   procedure Test_Scheduled_For_Getter (T : in out Traveller_Test);
   procedure Test_Scheduled_For_Setter (T : in out Traveller_Test);
   procedure Test_Source_Getter (T : in out Traveller_Test);
   procedure Test_Destination_Getter (T : in out Traveller_Test);
   procedure Test_Contains_Step_Infra_Id (T : in out Traveller_Test);
   procedure Test_Contains_Step_Slice (T : in out Traveller_Test);
   procedure Test_Has_Steps_Ahead (T : in out Traveller_Test);
   procedure Test_Has_No_Step_Ahead (T : in out Traveller_Test);
   procedure Test_Is_Travelling (T : in out Traveller_Test);
   procedure Test_Is_Not_Travelling (T : in out Traveller_Test);
   procedure Test_Equality_Of_Two_Travellers (T : in out Traveller_Test);
   --procedure Test_Inequality_Of_Two_Travellers (T : in out Traveller_Test);

   procedure Register_Tests (T : in out Traveller_Test);

end Active.Traveller.Tests;
