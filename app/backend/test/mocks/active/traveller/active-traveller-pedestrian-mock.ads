with Active.Agent;

package Active.Traveller.Pedestrian.Mock is

   package Agent renames Active.Agent;

   type Object is
     new Pedestrian.Object
   with private;
   type Reference is access all Pedestrian.Mock.Object'Class;

   function Create return Pedestrian.Mock.Reference;

   overriding
   function Get_Id (This : in Pedestrian.Mock.Object) return Agent.Agent_Id;

   overriding
   function Get_Maximum_Speed (This : in Pedestrian.Mock.Object)
   return Natural;

   overriding
   function Get_Current_Speed (This : in Pedestrian.Mock.Object)
   return Natural;

   overriding
   procedure Set_Current_Speed (This      : in out Pedestrian.Mock.Object;
                                New_Speed : in     Natural);

   overriding
   procedure Act (This : in out Pedestrian.Mock.Object) is null;

   overriding
   procedure Travel (This : in out Pedestrian.Mock.Object) is null;

   overriding
   function Has_Next_Step (This : in Pedestrian.Mock.Object) return Boolean;

   overriding
   function Is_Travelling (This : in Pedestrian.Mock.Object) return Boolean;

   overriding
   function "=" (This, Other : Pedestrian.Mock.Object) return Boolean;

   overriding
   procedure On_Bus_Stop (This : in out Pedestrian.Mock.Object);

   not overriding
   procedure Set_Id (
      This  : in out Pedestrian.Mock.Object;
      Id    : in     Agent.Agent_Id);

   not overriding
   procedure Set_Maximum_Speed (
      This          : in out Pedestrian.Mock.Object;
      Maximum_Speed : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Travelling (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_On_Bus_Stop (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean);

private
   type Mock_Values_Collection is record
      Id : Agent.Agent_Id;
      Id_Existence : Boolean := FALSE;
      Maximum_Speed : Natural;
      Maximum_Speed_Existence : Boolean := FALSE;
      Current_Speed : Natural;
      Current_Speed_Existence : Boolean := FALSE;
      On_Bus_Stop : Boolean;
      On_Bus_Stop_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Has_Next_Step : Boolean;
      Is_Travelling : Boolean;
      Is_Travelling_Existence : Boolean := FALSE;
      Equality_Operator : Boolean;
   end record;

   type Object is
     new Pedestrian.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traveller.Pedestrian.Mock;
