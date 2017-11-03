with AUnit;
with AUnit.Test_Cases;

with Active.Space_Master.Mock;
with Active.Traveller.Pedestrian.Utils.Mock;
with Active.Traveller.Vehicle.Tests;

with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;

package Active.Traveller.Vehicle.Bus.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Space_Master_Mock_Pkg renames Active.Space_Master.Mock;
   package Pedestrian_Utils_Mock_Pkg
      renames Active.Traveller.Pedestrian.Utils.Mock;
   package SSD_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;

   type Bus_Test is new Vehicle.Tests.Vehicle_Test with record
      Bus_Stops        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Route_Stops      : Infra_Id_List.List := Infra_Id_List.Empty_List;
      SSD_Utils        : SSD_Utils_Mock_Pkg.Reference;
      Pedestrian_Utils : Pedestrian_Utils_Mock_Pkg.Reference;
      Space_Master     : Space_Master_Mock_Pkg.Reference;
   end record;

   overriding procedure Set_Up (T: in out Bus_Test);
   -- Bus-specific test routines:
   procedure Test_Is_Affected_By_Traffic_Lights (T : in out Bus_Test);
   procedure Test_On_Bus_Stop (T : in out Bus_Test);

   procedure Register_Tests (T: in out Bus_Test);
   overriding function Name (T: in Bus_Test) return AU.Message_String;

end Active.Traveller.Vehicle.Bus.Tests;
