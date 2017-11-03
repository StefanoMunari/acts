with Natural_Package;

package body Shared.Natural_Mock_Test_Fixture is

   procedure Initialize (TI : out Shared.Natural_Mock.T_Reference) is
   begin
      TI := new Natural_Package.Natural_Object;
      TI.all.Value := 2;
  end Initialize;

end Shared.Natural_Mock_Test_Fixture;
