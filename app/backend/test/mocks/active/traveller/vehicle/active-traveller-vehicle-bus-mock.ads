package Active.Traveller.Vehicle.Bus.Mock is

   type Object is new Bus.Object with private;
   type Reference is access all Bus.Mock.Object'Class;

   function Create return Bus.Mock.Reference;

   overriding
   function Get_Id (This : in Bus.Mock.Object) return Agent.Agent_Id;

   overriding
   function Get_Maximum_Speed (This : in Bus.Mock.Object)
   return Natural;

   overriding
   function Get_Current_Speed (This : in Bus.Mock.Object)
   return Natural;

   overriding
   procedure Set_Current_Speed (This      : in out Bus.Mock.Object;
                                New_Speed : in     Natural);

   overriding
   procedure Act (This : in out Bus.Mock.Object) is null;

   overriding
   procedure Travel (This : in out Bus.Mock.Object) is null;

   overriding
   function Has_Next_Step (This : in Bus.Mock.Object) return Boolean;

   overriding
   function Is_Travelling (This : in Bus.Mock.Object) return Boolean;

   overriding
   function "=" (This, Other : Bus.Mock.Object) return Boolean;

   overriding
   procedure Board (This    : in out Bus.Mock.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean);

   overriding
   procedure Free (This      : in out Bus.Mock.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean);

   overriding
   function Count_Passengers (This : in Bus.Mock.Object) return Natural;

   not overriding
   procedure Set_Return_Value_For_Get_Id (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Agent.Agent_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Maximum_Speed (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Get_Current_Speed (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Travelling (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Boarded_Value_For_Board (
      This    : in out Bus.Mock.Object;
      Boarded : in     Boolean);

   not overriding
   procedure Set_Freed_Value_For_Free (
      This  : in out Bus.Mock.Object;
      Freed : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Count_Passengers (
      This         : in out Bus.Mock.Object;
      Return_Value : in     Natural);

private
   type Return_Values_Collection is record
      Get_Id : Agent.Agent_Id;
      Get_Maximum_Speed : Natural;
      Get_Current_Speed : Natural;
      Has_Next_Step : Boolean;
      Is_Travelling : Boolean;
      Equality_Operator : Boolean;
      Board : Boolean;
      Free : Boolean;
      Count_Passengers : Natural;
   end record;

   type Object is
     new Bus.Object
   with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traveller.Vehicle.Bus.Mock;
