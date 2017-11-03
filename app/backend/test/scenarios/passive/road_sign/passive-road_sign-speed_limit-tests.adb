with AUnit.Assertions;

with Active.Agent;
with Active.Traveller.Mock;
with Active.Traveller.Utils.Mock;

with Passive.Road_Sign.Speed_Limit;

with Reactive.Infrastructure.Lane.Footway_Lane.Mock;
with Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;
with Reactive.Infrastructure.Way.Footway.Mock;

with Shared.Direction;

package body Passive.Road_Sign.Speed_Limit.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Traveller_Mock renames Active.Traveller.Mock;
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package Footway renames Reactive.Infrastructure.Way.Footway.Mock;
   package Footway_Lane renames Reactive.Infrastructure.Lane.Footway_Lane.Mock;
   package Footway_Stretch renames Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;
   package Speed_Limit renames Passive.Road_Sign.Speed_Limit;
   package Direction renames Shared.Direction;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Trav_Utils          : Traveller_Utils_Mock.Reference;
   Traveller_Ref       : Traveller_Mock.Reference;
   Speed_Limit_Ref     : Speed_Limit.Reference;
   Footway_Ref         : Footway.Reference;
   Footway_Lane_Ref    : Footway_Lane.Reference;
   Footway_Stretch_Ref : Footway_Stretch.Reference;
   Base_Vel            : Natural;
   Limit_Velocity      : Natural;

   procedure Set_Up_Case (T: in out Speed_Limit_Test) is
   begin
      null;
   end Set_Up_Case;

   procedure Set_Up (T: in out Speed_Limit_Test) is
   begin
      Base_Vel  := 42;
      Limit_Velocity := Base_Vel + 1;
      Traveller_Ref := Traveller_Mock.Create;
      Trav_Utils := Traveller_Utils_Mock.Create;
      Trav_Utils.Set_Traveller_For_Set_Current_Speed (Traveller_Ref);
      Speed_Limit_Ref := Road_Sign.Speed_Limit.Create(
         Limit           => Limit_Velocity,
         Traveller_Utils => Trav_Utils);
      Footway_Ref := Footway.Create;
      Footway_Lane_Ref := Footway_Lane.Create;
      Footway_Stretch_Ref := Footway_Stretch.Create;
   end Set_Up;

   procedure Tear_Down (T: in out Speed_Limit_Test) is
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Speed_Limit_Test) is
   begin
      null;
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Speed_Limit_Under_The_Limit (T: in out TC.Test_Case'Class) is
      Expected_Speed : Natural := Limit_Velocity - 1;
      Traveller_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (42);
   begin
      Traveller_Ref.Set_Current_Speed (Expected_Speed);

      Speed_Limit_Ref.Apply (Traveller_Id);

      Ass.Assert (
         (Traveller_Ref.Get_Current_Speed = Expected_Speed),
         "Traveller speed has changed");

      Ass.Assert (
         (Traveller_Ref.Get_Current_Speed <= Limit_Velocity),
         "Traveller speed is above the limit");
   end Speed_Limit_Under_The_Limit;

   procedure Speed_Limit_Equal_To_Limit (T: in out TC.Test_Case'Class)
   is
      Expected_Speed : Natural := Limit_Velocity;
      Traveller_Id   : Agent.Agent_Id := Agent.Create_Id_From_Natural (43);
   begin
      Traveller_Ref.Set_Current_Speed (Limit_Velocity);

      Speed_Limit_Ref.Apply (Traveller_Id);

      Ass.Assert ((Traveller_Ref.Get_Current_Speed = Expected_Speed),
                   "Traveller speed has changed");

      Ass.Assert (
         (Traveller_Ref.Get_Current_Speed <= Limit_Velocity),
         "Traveller speed is above the limit");
   end Speed_Limit_Equal_To_Limit;

   procedure Speed_Limit_Over_The_Limit (T: in out TC.Test_Case'Class)
   is
      Expected_Speed : Natural := Limit_Velocity;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (44);
   begin
      Traveller_Ref.Set_Current_Speed (Limit_Velocity + 1);

      Speed_Limit_Ref.Apply (Traveller_Id);

      Ass.Assert ((Traveller_Ref.Get_Current_Speed = Expected_Speed),
                   "Traveller speed has changed");

      Ass.Assert (
         (Traveller_Ref.Get_Current_Speed <= Limit_Velocity),
         "Traveller speed is above the limit");
   end Speed_Limit_Over_The_Limit;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Speed_Limit_Test) is
      use TC.Registration;
   begin
      Register_Routine
         (Test => T,
          Routine => Speed_Limit_Under_The_Limit'Access,
          Name => "Speed_Limit if starting speed is under the limit");

      Register_Routine
         (Test => T,
          Routine => Speed_Limit_Equal_To_Limit'Access,
          Name => "Speed_Limit if starting speed is equal to the limit");

      Register_Routine
         (Test => T,
          Routine => Speed_Limit_Over_The_Limit'Access,
          Name => "Speed_Limit if starting speed is above the limit");
   end Register_Tests;

   function Name(T: Speed_Limit_Test) return AU.Message_String is
   begin
      return AU.Format ("Speed_Limit");
   end Name;
end Passive.Road_Sign.Speed_Limit.Tests;
