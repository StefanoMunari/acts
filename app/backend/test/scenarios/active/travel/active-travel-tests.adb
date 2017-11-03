with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Travel.Travel_State.Mock;
with Active.Traveller.Utils.Mock;

with AI;
with AI.Mock;

with Reactive;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;

with Shared.Slice;

package body Active.Travel.Tests is

   use Active.Agent; -- for Agent_Id comparison
   use Reactive.Infra_Id_Type;
   use Reactive.Stretch_Type_Package;

   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Travel_State_Mock renames Active.Travel.Travel_State.Mock;
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package AI_Mock_Pkg renames AI.Mock;
   package Street_Utils_Mock renames Reactive.Infrastructure.Street.Utils.Mock;
   package Stretch_Utils_Mock
      renames Reactive.Infrastructure.Stretch.Utils.Mock;
   package Infrastructure_Utils_Mock
      renames Reactive.Infrastructure.Utils.Mock;
   package Intersection_Utils_Mock
      renames Reactive.Infrastructure.Intersection.Utils.Mock;
   package Slice renames Shared.Slice;
   use Slice;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Route_Source         : Slice.Map := Slice.Empty_Map;
   Route_Destination    : Slice.Map := Slice.Empty_Map;
   Dummy_List           : Infra_Id_List.List := Infra_Id_List.Empty_List;
   Traveller_Id         : Agent.Agent_Id;
   Travel               : Active.Travel.Reference;
   Travel_State         : access Travel_State_Mock.Object;
   Traveller_Utils      : access Traveller_Utils_Mock.Object;
   Street_Utils         : access Street_Utils_Mock.Object;
   Infrastructure_Utils : access Infrastructure_Utils_Mock.Object;
   Stretch_Utils        : access Stretch_Utils_Mock.Object;
   Intersection_Utils   : access Intersection_Utils_Mock.Object;
   AI_Mock_Ref          : access AI_Mock_Pkg.Object;

   procedure Set_Up (T: in out Travel_Test) is
   begin
      AI_Mock_Ref := new AI.Mock.Object;
      Active.Travel.Set_AI (AI_Mock_Ref);
      Traveller_Id := Agent.Create_Id_From_Natural (12);
      Route_Source.Include (FOOT, Dummy_List);
      Route_Source.Include (BIKE, Dummy_List);
      Route_Source.Include (ROAD, Dummy_List);
      Route_Destination.Include (FOOT, Dummy_List);
      Route_Destination.Include (BIKE, Dummy_List);
      Route_Destination.Include (ROAD, Dummy_List);
      Travel_State := Travel_State_Mock.Create;
      Traveller_Utils := Traveller_Utils_Mock.Create;
      Infrastructure_Utils := Infrastructure_Utils_Mock.Create;
      Street_Utils := Street_Utils_Mock.Create;
      Intersection_Utils := Intersection_Utils_Mock.Create;
      Stretch_Utils := Stretch_Utils_Mock.Create;

      Travel
         := Active.Travel.Create (Route_Source      => Route_Source,
                                  Route_Destination => Route_Destination,
                                  Traveller_Id      => Traveller_Id,
                                  Travel_State      => Travel_State,
                                  Traveller_Utils   => Traveller_Utils);
   end Set_Up;

   procedure Test_Has_Next_Step (T: in out TC.Test_Case'Class)
   is
      Steps           : Infra_Id_List.List;
      Current_Step_Id : Infra_Id := 800;
      Next_Step_Id    : Infra_Id := 8657;
   begin
      Steps.Append (Current_Step_Id);
      Steps.Append (Next_Step_Id);
      Travel.Route := Steps;

      Ass.Assert (Travel.Has_Next_Step,
                  "The travel has no next step");
      Travel.Route := Infra_Id_List.Empty_List;
   end Test_Has_Next_Step;

   procedure Test_Has_No_Next_Step (T: in out TC.Test_Case'Class) is
   begin
      Travel_State.Set_Return_Value_For_Has_Next_Step (FALSE);

      Ass.Assert (not Travel.Has_Next_Step,
                  "The travel has next step");
   end Test_Has_No_Next_Step;

   procedure Test_Is_Progressing (T: in out TC.Test_Case'Class) is
   begin
      Travel_State.Set_Return_Value_For_Is_Progressing (TRUE);

      Ass.Assert (Travel.Is_Progressing,
                  "The travel is not progressing");
   end Test_Is_Progressing;

   procedure Test_Is_Not_Progressing (T: in out TC.Test_Case'Class) is
   begin
      Travel_State.Set_Return_Value_For_Is_Progressing (FALSE);

      Ass.Assert (not Travel.Is_Progressing,
                  "The travel is progressing");
   end Test_Is_Not_Progressing;

   procedure Test_Route_Source_Id_Getter (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (Travel.Get_Route_Source = Travel.Route_Source,
                  "The route source returned by Travel's getter is wrong");
   end Test_Route_Source_Id_Getter;

   procedure Test_Route_Destination_Id_Getter (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (
         Travel.Get_Route_Destination = Travel.Route_Destination,
        "The route destination returned by Travel's getter is wrong");
   end Test_Route_Destination_Id_Getter;

   procedure Test_Traveller_Id_Getter (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (Travel.Get_Traveller_Id = Travel.Traveller_Id,
                  "The traveller id returned by Travel's getter is wrong");
   end Test_Traveller_Id_Getter;

   procedure Test_Current_Step_Getter (T: in out TC.Test_Case'Class)
   is
      Current_Step_Id : Infra_Id := 34353;
      Steps           : Infra_Id_List.List;
   begin
      Steps.Append (Current_Step_Id);
      Travel.Route := Steps;

      Ass.Assert (Travel.Get_Current_Step_Id = Current_Step_Id,
                  "The current step id returned by Travel's getter is wrong");
   end Test_Current_Step_Getter;

   procedure Test_Next_Step_Getter (T: in out TC.Test_Case'Class)
   is
      First_Step_Id : Infra_Id := 323;
      Next_Step_Id  : Infra_Id := 34353;
   begin
      Travel.Route.Append (First_Step_Id);
      Travel.Route.Append (Next_Step_Id);

      Ass.Assert (Travel.Get_Next_Step_Id = Next_Step_Id,
                  "The next step id returned by Travel's getter is wrong");
   end Test_Next_Step_Getter;

   procedure Test_Previous_Step_Getter (T: in out TC.Test_Case'Class)
   is
      First_Step_Id : Infra_Id := 323;
      Next_Step_Id  : Infra_Id := 34353;
   begin
      Travel_State.Set_Return_Value_For_Has_Next_Step (True);
      Travel.Route.Append (First_Step_Id);
      Travel.Route.Append (Next_Step_Id);
      Travel.Consume_Step;

      Ass.Assert (Travel.Get_Previous_Step_Id = First_Step_Id,
                  "The previous step id returned by Travel's getter is wrong");
   end Test_Previous_Step_Getter;

   procedure Test_Step_Prepend (
      T: in out TC.Test_Case'Class)
   is
      Steps             : Infra_Id_List.List;
      Prepended_Step_Id : Infra_Id := 435;
   begin
      Steps.Append (123);
      Steps.Append (51);
      Steps.Append (112);
      Steps.Append (214);

      Travel.Route := Steps;

      Steps.Prepend (Prepended_Step_Id);

      Travel.Prepend_Step (Prepended_Step_Id);

      Ass.Assert (Travel.Route."=" (Steps),
                  "The step has not been prepended");
   end Test_Step_Prepend;

   procedure Test_Travel_State_Change (
      T: in out TC.Test_Case'Class)
   is
      Travel_State1, Travel_State2 : Travel_State_Mock.Reference;
   begin
      Travel_State1 := Travel_State_Mock.Create;
      Travel_State2 := Travel_State_Mock.Create;

      Travel.Travel_State := Travel_State1;

      Ass.Assert (Travel.Travel_State /= Travel_State2,
                  "The travel is already in new state");

      Travel.Change_Travel_State (Travel_State2);

      Ass.Assert (Travel.Travel_State = Travel_State2,
                  "The travel is not already in new state");
   end Test_Travel_State_Change;

   procedure Test_First_Step_Id_Getter (T: in out TC.Test_Case'Class)
   is
      Steps         : Infra_Id_List.List;
      First_Step_Id : Infra_Id := 435;
   begin
      Steps.Append (First_Step_Id);
      Steps.Append (123);
      Steps.Append (51);
      Steps.Append (112);

      Travel.Route := Steps;

      Ass.Assert (Travel.Get_First_Step_Id = First_Step_Id,
                  "The first step is not the expected one");
   end Test_First_Step_Id_Getter;

   procedure Test_Last_Step_Id_Getter (T: in out TC.Test_Case'Class)
   is
      Steps        : Infra_Id_List.List;
      Last_Step_Id : Infra_Id := 435;
   begin
      Steps.Append (123);
      Steps.Append (51);
      Steps.Append (112);
      Steps.Append (Last_Step_Id);

      Travel.Route := Steps;

      Ass.Assert (Travel.Get_Last_Step_Id = Last_Step_Id,
                  "The last step is not the expected one");
   end Test_Last_Step_Id_Getter;

   procedure Test_Route_Getter (T: in out TC.Test_Case'Class)
   is
      Steps : Infra_Id_List.List;
   begin
      Steps.Append (123);
      Steps.Append (51);
      Steps.Append (112);
      Steps.Append (214);

      Travel.Route := Steps;

      Ass.Assert (Travel.Get_Route."=" (Steps),
                  "The route not contains the steps expected");
   end Test_Route_Getter;

   procedure Test_Route_Clear (
      T: in out TC.Test_Case'Class)
   is
      Clean : Boolean := FALSE;
   begin
      Travel.Route.Append (23);
      Ass.Assert (not Travel.Route.Is_Empty,
                  "Travel's route is empty");
      Travel.Clear_Route (Clean);
      Ass.Assert (Clean,
                  "Travel's route is clean");
      Ass.Assert (Travel.Route.Is_Empty,
                  "Travel's route is not empty");
   end Test_Route_Clear;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Travel_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Has_Next_Step'Access,
                        Name => "Test has next step");

      Register_Routine (Test => T,
                        Routine => Test_Has_No_Next_Step'Access,
                        Name => "Test has not next step");

      Register_Routine (
         Test => T,
         Routine => Test_Is_Progressing'Access,
         Name => "Test is progressing");

      Register_Routine (
         Test => T,
         Routine => Test_Is_Not_Progressing'Access,
         Name => "Test is not progressing");

      Register_Routine (
         Test => T,
         Routine => Test_Route_Source_Id_Getter'Access,
         Name => "Test route source id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Route_Destination_Id_Getter'Access,
         Name => "Test route destination id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Traveller_Id_Getter'Access,
         Name => "Test traveller id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Current_Step_Getter'Access,
         Name => "Test current step id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Next_Step_Getter'Access,
         Name => "Test next step id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Previous_Step_Getter'Access,
         Name => "Test previous step id getter");

     Register_Routine (
         Test => T,
         Routine => Test_Step_Prepend'Access,
         Name => "Test step prepend");

      Register_Routine (
         Test => T,
         Routine => Test_Travel_State_Change'Access,
         Name => "Test travel state change");

      Register_Routine (
         Test => T,
         Routine => Test_First_Step_Id_Getter'Access,
         Name => "Test first step id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Last_Step_Id_Getter'Access,
         Name => "Test last step id getter");

      Register_Routine (
         Test => T,
         Routine => Test_Route_Getter'Access,
         Name => "Test route getter");

      Register_Routine (
         Test => T,
         Routine => Test_Route_Clear'Access,
         Name => "Test route cleaning");
   end Register_Tests;

   function Name(T: Travel_Test) return AU.Message_String is
   begin
      return AU.Format ("Travel");
   end Name;
end Active.Travel.Tests;
