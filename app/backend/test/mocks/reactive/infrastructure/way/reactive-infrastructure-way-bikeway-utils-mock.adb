with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Way.Bikeway.Utils.Mock is

   function Create return Bikeway.Utils.Mock.Reference
   is (new Way.Bikeway.Utils.Mock.Object);

   overriding
   procedure Set_Street (This       : in Bikeway.Utils.Mock.Object;
                         Bikeway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id) is null;

   procedure Find_Lane_By_Direction (
      This             : in     Bikeway.Utils.Mock.Object;
      Bikeway_Id       : in     Infra_Id;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean) is
   begin
      if not This.Return_Values.Find_Lane_By_Direction_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Procedure_Name => "Find_Lane_By_Direction",
            Package_Name   => "Bikeway.Utils.Mock");
      end if;

      Lane_Id := This.Return_Values.Find_Lane_By_Direction;
      Found   := This.Return_Values.Find_Lane_By_Direction_Found;
   end Find_Lane_By_Direction;

   procedure Validate (This       : in Bikeway.Utils.Mock.Object;
                       Bikeway_Id : in Infra_Id) is null;

   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Bikeway.Utils.Mock.Object;
      Lane_Id : in     Infra_Id;
      Found   : in     Boolean) is
   begin
      This.Return_Values.Find_Lane_By_Direction := Lane_Id;
      This.Return_Values.Find_Lane_By_Direction_Found := Found;
      This.Return_Values.Find_Lane_By_Direction_Existence := TRUE;
   end Set_Return_Value_For_Find_Lane_By_Direction;

end Reactive.Infrastructure.Way.Bikeway.Utils.Mock;
