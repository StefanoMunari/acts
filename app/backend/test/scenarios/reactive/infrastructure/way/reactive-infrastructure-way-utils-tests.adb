with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.District.Mock;
with Reactive.Infrastructure.Way.Mock;
with Reactive.Infrastructure.Way.Roadway.Mock;

-- TODO: Look for commented Add_Infrastructure
-- because it does not exist anymore and you have to add a
-- concrete class mock
package body Reactive.Infrastructure.Way.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package District_Mock renames Reactive.District.Mock;
   package Way_Mock renames Reactive.Infrastructure.Way.Mock;
   package Roadway_Mock renames Reactive.Infrastructure.Way.Roadway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Way_Utils : Reactive.Infrastructure.Way.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Way_Utils_Test) is
   begin
      District := District_Mock.Create;

      Way_Utils
        := Reactive.Infrastructure.Way.Utils.Get_Instance (District => District);
   end Set_Up;

   procedure Test_Find_Street (T: in out TC.Test_Case'Class)
   is
      Way_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Way : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Way_Mock_Ref);
      Way_Id : Infra_Id := 345;
      Expected_Street_Id : Infra_Id := 435;
      Added : Boolean := FALSE;
   begin
      Way_Mock_Ref.Set_Id (Way_Id);

      District.Add_Roadway (Infrastructure => Way,
                            Added          => Added);

      Way_Mock_Ref.Set_Return_Value_For_Find_Street (Expected_Street_Id);

      Ass.Assert (Way_Utils.Find_Street (Way_Id => Way_Id) = Expected_Street_Id,
                  "The found street id is not the expected one");
   end Test_Find_Street;

   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class)
   is
      Way_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Way : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Way_Mock_Ref);
      Way_Id : Infra_Id := 346;
      Container_Id : Infra_Id := 1346;
      Added : Boolean := FALSE;
   begin
      Way_Mock_Ref.Set_Id (Way_Id);
      District.Add_Roadway (Infrastructure => Way,
                            Added          => Added);

      Way_Mock_Ref.Set_Return_Value_For_Is_Contained_By (Container_Id, TRUE);

      Ass.Assert (Way_Utils.Is_Contained_By (Way_Id       => Way_Id,
                                             Container_Id => Container_Id),
                  "The way is not contained by the containter");
   end Test_Is_Contained_By;

   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class)
   is
      Way_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Way : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Way_Mock_Ref);
      Way_Id : Infra_Id := 345;
      Container_Id : Infra_Id := 1345;
      Added : Boolean := FALSE;
   begin
      Way_Mock_Ref.Set_Id (Way_Id);
      District.Add_Roadway (Infrastructure => Way,
                            Added          => Added);

      Way_Mock_Ref.Set_Return_Value_For_Is_Contained_By (Container_Id, FALSE);

      Ass.Assert (not Way_Utils.Is_Contained_By (Way_Id       => Way_Id,
                                                 Container_Id => Container_Id),
                  "The way is contained by the containter");
   end Test_Is_Not_Contained_By;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Way_Utils_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Find_Street'Access,
                        Name => "Test find street");

      Register_Routine (Test => T,
                        Routine => Test_Is_Contained_By'Access,
                        Name => "Test way is contained by a container");

      Register_Routine (Test => T,
                        Routine => Test_Is_Not_Contained_By'Access,
                        Name => "Test way is not contained by a container");
   end Register_Tests;

   function Name(T: Way_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Way_Utils");
   end Name;
end Reactive.Infrastructure.Way.Utils.Tests;
