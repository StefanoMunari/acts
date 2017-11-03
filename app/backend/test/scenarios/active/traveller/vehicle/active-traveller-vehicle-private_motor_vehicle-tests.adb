with AUnit.Assertions;

with Active.Agent;
with Active.Travel.Mock;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Utils.Mock;
with Reactive.Infrastructure.Intersection.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package body Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;
   package Host_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Infrastructure_Utils renames Reactive.Infrastructure.Utils.Mock;
   package Street_Utils renames Reactive.Infrastructure.Street.Utils.Mock;
   package Stretch_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Stretch.Utils.Mock;
   package Intersection_Utils renames Reactive.Infrastructure.Intersection.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Private_Motor_Vehicle_Id : Agent.Agent_Id;
   Maximum_Speed, Route_Source_Id, Route_Destination_Id,
   Max_Passengers : Natural;

   procedure Set_Up (T: in out Private_Motor_Vehicle_Test) is
   begin
      Private_Motor_Vehicle_Id := Agent.Create_Id_From_Natural (2);
      Maximum_Speed := 20;
      Max_Passengers := 2;
      Route_Source_Id := 2;
      Route_Destination_Id := 5;
      T.Infrastructure_Utils := Infrastructure_Utils.Create;
      T.Street_Utils := Street_Utils.Create;
      T.Stretch_Utils := Stretch_Utils_Mock_Pkg.Create;
      T.Intersection_Utils := Intersection_Utils.Create;
      T.Travel_Ref := Active.Travel.Mock.Create;
      T.Traveller_Utils := Traveller_Utils_Mock_Pkg.Create;
      T.Host_Utils := Host_Utils_Mock_Pkg.Create;

      T.Traveller := Traveller.Reference (
         Private_Motor_Vehicle.Create (
            Id  => Private_Motor_Vehicle_Id,
            Maximum_Speed        => Maximum_Speed,
            Max_Passengers       => Max_Passengers,
            Vehicle_Type         => Private_Motor_Vehicle.CAR,
            Travel_Ref           => T.Travel_Ref,
            Infrastructure_Utils => T.Infrastructure_Utils,
            Street_Utils         => T.Street_Utils,
            Stretch_Utils        => T.Stretch_Utils,
            Traveller_Utils      => T.Traveller_Utils,
            Host_Utils           => T.Host_Utils,
            Intersection_Utils   => T.Intersection_Utils));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Private_Motor_Vehicle_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Private_Motor_Vehicle_Test);
      use Register_Specific;
   begin
      Vehicle.Tests.Register_Tests (Vehicle.Tests.Vehicle_Test(T));
   end Register_Tests;

   function Name(T: Private_Motor_Vehicle_Test) return AU.Message_String is
   begin
      return AU.Format ("Private_Motor_Vehicle");
   end Name;
end Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests;
