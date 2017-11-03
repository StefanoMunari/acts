with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.District.Mock;
with Reactive.Infrastructure.Intersection.Mock;

package body Reactive.Infrastructure.Intersection.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package District_Mock renames Reactive.District.Mock;
   package Intersection_Mock renames Reactive.Infrastructure.Intersection.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Intersection_Utils : Reactive.Infrastructure.Intersection.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Intersection_Utils_Test) is
   begin
      District := District_Mock.Create;

      Intersection_Utils
        := Reactive.Infrastructure.Intersection.Utils
          .Get_Instance (District => District);
   end Set_Up;

   procedure Test_Street_Direction_Finder (T: in out TC.Test_Case'Class)
   is
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Intersection_Id : Infra_Id := 55;
      Street_Id : Infra_Id := 56;
      Found_Direction : Direction.Cardinal;
      Expected_Direction : Direction.Cardinal := Direction.WEST;
      Added, Direction_Found : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      District.Add_Intersection (Infrastructure => Intersection,
                                 Added          => Added);

      Ass.Assert (Added,
                  "The Intersection is not added to district");

      Intersection
        .Set_Return_Value_For_Find_Street_Direction
          (Direction => Expected_Direction,
           Found => TRUE);

      Intersection_Utils.Find_Street_Direction
        (Intersection_Id  => Intersection_Id,
         Street_Id        => Street_Id,
         Street_Direction => Found_Direction,
         Found            => Direction_Found);

      Ass.Assert (Direction_Found,
                  "The direction is not found");

      Ass.Assert (Direction."=" (Found_Direction, Expected_Direction),
                  "The found direction is like expected");
   end Test_Street_Direction_Finder;

   procedure Test_Streets_Connected_With_Intersection_Finder (T: in out TC.Test_Case'Class)
   is
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Intersection_Id : Infra_Id := 345;
      Expected_Street_Ids : Infra_Id_Set.Set;
      Expected_Direction : Direction.Cardinal := Direction.WEST;
      Added, Direction_Found : Boolean := FALSE;
   begin
      Expected_Street_Ids.Insert (723);
      Expected_Street_Ids.Insert (335);
      Expected_Street_Ids.Insert (635);

      Intersection.Set_Id (Intersection_Id);
      District.Add_Intersection (Infrastructure => Intersection,
                                 Added          => Added);

      Ass.Assert (Added,
                  "The Intersection is not added to district");

      Intersection
        .Set_Return_Value_For_Find_Streets_Connected_With_Intersection
          (Expected_Street_Ids);

      Ass.Assert (Intersection_Utils.Find_Streets_Connected_With_Intersection
                  (Intersection_Id)."=" (Expected_Street_Ids),
                  "The street connected with interersection found is wrong");
   end Test_Streets_Connected_With_Intersection_Finder;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Intersection_Utils_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Street_Direction_Finder'Access,
                        Name    => "Test street direction finder");

      Register_Routine (Test    => T,
                        Routine => Test_Streets_Connected_With_Intersection_Finder'Access,
                        Name    => "Test streets connected with intersection finder");
   end Register_Tests;

   function Name(T: Intersection_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Intersection_Utils");
   end Name;
end Reactive.Infrastructure.Intersection.Utils.Tests;
