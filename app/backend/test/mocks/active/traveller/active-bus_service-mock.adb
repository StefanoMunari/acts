package body Active.Bus_Service.Mock is

   function Create return Bus_Service.Mock.Reference
   is (new Bus_Service.Mock.Object);

   overriding
   procedure On_Bus_Stop (This : in out Mock.Object) is
   begin
     This.Mock_Values.On_Bus_Stop_Called := TRUE;
   end;

   function Get_On_Bus_Stop_Called (This : in out Mock.Object)
   return Boolean is (This.Mock_Values.On_Bus_Stop_Called);

end Active.Bus_Service.Mock;
