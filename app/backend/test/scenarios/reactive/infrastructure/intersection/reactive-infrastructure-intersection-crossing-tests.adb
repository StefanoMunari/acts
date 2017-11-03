with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.Infrastructure.Intersection.Crossing;

with Shared.Direction;

package body Reactive.Infrastructure.Intersection.Crossing.Tests is
   package Ass renames AUnit.Assertions;
   package Direction renames Shared.Direction;

   task body TX is
      From_To : Shared.Direction.Any;
      Crossed : Boolean := False;
   begin
      accept Start_Crossing (D : Shared.Direction.Any) do
         From_To := D;
      end Start_Crossing;
      Intersection_Crossing_Ref.Cross
         (Traveller_Id => Agent.Create_Id_From_Natural (Traveller_Id),
          Direction    => From_To,
          Crossed      => Crossed);
      accept Wait_For_Crossing do
         null;
      end Wait_For_Crossing;
   end TX;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Intersection_Crossing_Ref : Intersection.Crossing.Reference;

   procedure Set_Up (T: in out Intersection_Crossing_Test) is
   begin
      Intersection_Crossing_Ref
        := new Intersection.Crossing.Object;
      Intersection_Crossing_Ref.Initialize;
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Safe_Cross (T : in out TC.Test_Case'Class) is
      Trav_1 : TX (42, Intersection_Crossing_Ref);
   begin
      null;
   end Test_Safe_Cross;

   procedure Test_Cross (T : in out TC.Test_Case'Class)
   is
      --T1 : TX (124, Direction.NORTH_EAST);
      --T2 : TX (125, Direction.SOUTH_WEST);
      --T3 : TX (126, Direction.NORTH_EAST);
      --T4 : TX (127, Direction.SOUTH_WEST);
      --T5 : TX (128, Direction.NORTH_EAST);
      --T6 : TX (129, Direction.SOUTH_WEST);
      --T7 : TX (130, Direction.NORTH_EAST);
      --T8 : TX (131, Direction.SOUTH_WEST);
      --T9 : TX (132, Direction.NORTH_EAST);
      --T10 : TX (133, Direction.SOUTH_WEST);
      --T11 : TX (134, Direction.NORTH_EAST);
      --T12 : TX (135, Direction.SOUTH_WEST);
   begin
      null;
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Shared.Direction.SOUTH_WEST);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Direction);
--
--        Intersection_Crossing_Ref.Cross (Traveller_Id => Traveller_Id,
--                                         Direction    => Shared.Direction.NORTH_WEST);
   end Test_Cross;

--     procedure Test_Intersection_Id_Getter (T: in out TC.Test_Case'Class)
--     is
--        Intersection_Id : Natural := 34;
--     begin
--        Intersection_Ref.Id := Intersection_Id;
--
--        Ass.Assert (Intersection_Ref.Get_Id = Intersection_Id,
--                    "The id returned by Intersection's getter is wrong");
--     end Test_Intersection_Id_Getter;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Intersection_Crossing_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Cross'Access,
                        Name => "Test intersection crossing");
   end Register_Tests;

   function Name(T: Intersection_Crossing_Test) return AU.Message_String is
   begin
      return AU.Format ("Intersection Crossing");
   end Name;
end Reactive.Infrastructure.Intersection.Crossing.Tests;
