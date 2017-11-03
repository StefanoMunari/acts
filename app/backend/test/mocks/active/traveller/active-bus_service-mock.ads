package Active.Bus_Service.Mock is

   type Object is
     new Bus_Service.Object
   with private;
   type Reference is access all Bus_Service.Mock.Object'Class;

   function Create return Bus_Service.Mock.Reference;

   overriding
   procedure On_Bus_Stop (This    : in out Mock.Object);

   not overriding
   function Get_On_Bus_Stop_Called (This    : in out Mock.Object)
   return Boolean;

private
   type Mock_Values_Collection is record
      On_Bus_Stop_Called : Boolean := FALSE;
   end record;

   type Object is
     new Bus_Service.Object
   with record
      Mock_Values : Mock_Values_Collection;
   end record;

end Active.Bus_Service.Mock;
