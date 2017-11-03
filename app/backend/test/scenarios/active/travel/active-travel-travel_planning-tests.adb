with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Space_Master.Mock;
with Active.Travel.Mock;
with Active.Travel.Travel_Progress.Mock;
with Active.Traveller.Utils.Mock;

with AI;
with AI.Mock;

with Reactive;
with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;
with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;

with Shared.Slice;

package body Active.Travel.Travel_Planning.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Space_Master_Mock renames Active.Space_Master.Mock;
   package Travel_Mock renames Active.Travel.Mock;
   package Travel_Progress_Mock renames Active.Travel.Travel_Progress.Mock;
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package AI_Mock_Pkg renames AI.Mock;
   package Infrastructure_Utils_Mock
      renames Reactive.Infrastructure.Utils.Mock;
   package Street_Utils_Mock renames Reactive.Infrastructure.Street.Utils.Mock;
   package Stretch_Utils_Mock
      renames Reactive.Infrastructure.Stretch.Utils.Mock;
   package Host_Utils_Mock
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Intersection_Utils_Mock
      renames Reactive.Infrastructure.Intersection.Utils.Mock;
   package Slice renames Shared.Slice;
   use Reactive.Stretch_Type_Package;
   use Slice;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Travel               : Travel_Mock.Reference;
   Travel_Progress      : Travel_Progress_Mock.Reference;
   Infrastructure_Utils : Infrastructure_Utils_Mock.Reference;
   Street_Utils         : Street_Utils_Mock.Reference;
   Stretch_Utils        : Stretch_Utils_Mock.Reference;
   Traveller_Utils      : Traveller_Utils_Mock.Reference;
   Intersection_Utils   : Intersection_Utils_Mock.Reference;
   Travel_Planning      : Active.Travel.Travel_Planning.Reference;
   Host_Utils           : access Host_Utils_Mock.Object;
   Space_Master         : access Space_Master_Mock.Object;
   AI_Mock_Ref          : access AI_Mock_Pkg.Object;

   procedure Set_Street_Of_Infrastructure (
      Infrastructure_Id, Street_Id : in Infra_Id)
   renames Street_Utils.Set_Return_Value_For_Get_Id;

   procedure Set_Stretch_Of_Infrastructure (
      Infrastructure_Id, Stretch_Id : in Infra_Id)
   renames Stretch_Utils.Set_Return_Value_For_Get_Id;

   procedure Set_Up (T: in out Travel_Planning_Test) is
   begin
      AI_Mock_Ref := new AI.Mock.Object;
      Active.Travel.Set_AI (AI_Mock_Ref);
      Travel := Travel_Mock.Create;
      Travel_Progress := Travel_Progress_Mock.Create;
      Infrastructure_Utils := Infrastructure_Utils_Mock.Create;
      Street_Utils := Street_Utils_Mock.Create;
      Stretch_Utils := Stretch_Utils_Mock.Create;
      Intersection_Utils := Intersection_Utils_Mock.Create;
      Traveller_Utils := Traveller_Utils_Mock.Create;
      Host_Utils := Host_Utils_Mock.Create;
      Space_Master := Space_Master_Mock.Create;
      Travel_Planning := Active.Travel.Travel_Planning.Get_Instance (
         Travel_Progress      => Travel_Progress,
         Infrastructure_Utils => Infrastructure_Utils,
         Street_Utils         => Street_Utils,
         Stretch_Utils        => Stretch_Utils,
         Intersection_Utils   => Intersection_Utils,
         Traveller_Utils      => Traveller_Utils,
         Host_Utils           => Host_Utils,
         Space_Master         => Space_Master);
   end Set_Up;

   procedure Test_Has_No_Next_Step (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (not Travel_Planning.Has_Next_Step (Travel.all),
                  "The travel has next step");
   end Test_Has_No_Next_Step;

   procedure Test_Is_Not_Progressing (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (not Travel_Planning.Is_Progressing (Travel.all),
                  "The travel is progressing");
   end Test_Is_Not_Progressing;

   procedure Test_Plan_The_Same_Stretch (T : in out TC.Test_Case'Class)
   is
      Traveller_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (3456);
      The_Slice     : Slice.Map := Slice.Empty_Map;
      Stretch_Id    : Infra_Id := 86;
      Step          : String := Integer'Image (Integer (Stretch_Id));
      Steps         : AI.Step_List.List := AI.Step_List.Empty_List;
      The_Stretches : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Actual_Route  : Infra_Id_List.List;
   begin
      The_Stretches.Append (Stretch_Id);
      The_Slice.Include (FOOT, The_Stretches);
      The_Slice.Include (ROAD, The_Stretches);
      The_Slice.Include (BIKE, The_Stretches);
      Travel.Set_Return_Value_For_Get_Traveller_Id (Traveller_Id);
      Travel.Set_Return_Value_For_Get_Route_Source (The_Slice);
      Travel.Set_Return_Value_For_Get_Route_Destination (The_Slice);
      Traveller_Utils.Set_Return_Value_For_Get_List_From_Slice (The_Stretches);
      Traveller_Utils.Set_Return_Value_For_Get_Stretch_Type (FOOT);
      Infrastructure_Utils.Set_Return_Value_For_Exists (TRUE);
      Steps.Append (Step);
      AI_Mock_Ref.Set_Return_Value_For_Find_Path (Steps);

      Travel_Planning.Plan (Travel.all);

      Actual_Route := Travel.Get_Residual_Route;

      Ass.Assert (Natural (Actual_Route.Length) = 1,
                  "The route has not length equals to 1");

      Ass.Assert (Actual_Route.Contains (Stretch_Id),
                  "The route contains the stretch id");
   end Test_Plan_The_Same_Stretch;


   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Travel_Planning_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Has_No_Next_Step'Access,
                        Name => "Test has no next step");

      Register_Routine (
         Test => T,
         Routine => Test_Is_Not_Progressing'Access,
         Name => "Test is not progressing");

      Register_Routine (
         Test => T,
         Routine => Test_Plan_The_Same_Stretch'Access,
         Name    => "Test plan the same stretch");
   end Register_Tests;

   function Name(T: Travel_Planning_Test) return AU.Message_String is
   begin
      return AU.Format ("Travel_Planning");
   end Name;
end Active.Travel.Travel_Planning.Tests;
