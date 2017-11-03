with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Way.Utils.Mock is

   function Create return Way.Utils.Mock.Reference
   is (new Way.Utils.Mock.Object);

   function Find_Street (This : in Way.Utils.Mock.Object;
                         Way_Id : in Infra_Id)
                         return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name  => "Way.Utils.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Is_Contained_By (This : in Way.Utils.Mock.Object;
                             Way_Id, Container_Id : in Infra_Id)
                             return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name  => "Way.Utils.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

    procedure Set_Return_Value_For_Find_Street
     (This : in out Way.Utils.Mock.Object;
      Return_Value : in Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Is_Contained_By
     (This : in out Way.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

end Reactive.Infrastructure.Way.Utils.Mock;
