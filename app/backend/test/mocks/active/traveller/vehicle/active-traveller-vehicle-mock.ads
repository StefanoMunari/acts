package Active.Traveller.Vehicle.Mock is

   type Object is new Vehicle.Object with private;
   type Reference is access all Vehicle.Mock.Object'Class;

   function Create return Vehicle.Mock.Reference;

   overriding
   function Get_Id (This : in Vehicle.Mock.Object) return Agent.Agent_Id;

   overriding
   function Get_Maximum_Speed (This : in Vehicle.Mock.Object)
   return Natural;

   overriding
   function Get_Current_Speed (This : in Vehicle.Mock.Object)
   return Natural;

   overriding
   procedure Set_Current_Speed (This      : in out Vehicle.Mock.Object;
                                New_Speed : in     Natural);

   overriding
   procedure Act (This : in out Vehicle.Mock.Object) is null;

   overriding
   procedure Travel (This : in out Vehicle.Mock.Object) is null;

   overriding
   function Has_Next_Step (This : in Vehicle.Mock.Object) return Boolean;

   overriding
   function Is_Travelling (This : in Vehicle.Mock.Object) return Boolean;

   overriding
   function "=" (This, Other : Vehicle.Mock.Object) return Boolean;

   overriding
   procedure Board (This    : in out Vehicle.Mock.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :        out Boolean);

   overriding
   procedure Free (This      : in out Vehicle.Mock.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean);

   overriding
   function Get_Passengers   (This : in Vehicle.Mock.Object)
   return Agent_Id_List.List;

   overriding
   function Count_Passengers (This : in Vehicle.Mock.Object) return Natural;

   overriding
   function Get_List_From_Slice (This      : in Vehicle.Mock.Object;
                                 The_Slice : in Slice.Map)
   return Infra_Id_List.List;

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Vehicle.Mock.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Vehicle.Mock.Object)
   return Natural;

   not overriding
   procedure Set_Return_Value_For_Get_Id (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Agent.Agent_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Maximum_Speed (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Get_Current_Speed (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Travelling (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_List_Value_For_Get_List_From_Slice (
      This     : in out Vehicle.Mock.Object;
      The_List : in     Infra_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Board (
      This    : in out Vehicle.Mock.Object;
      Boarded : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Free (
      This  : in out Vehicle.Mock.Object;
      Freed : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Passengers (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Agent_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Count_Passengers (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Is_Affected_By_Traffic_Lights (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Size (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   function Get_Board_Called (This : in out Mock.Object) return Boolean;

   not overriding
   function Get_Free_Called (This : in out Mock.Object) return Boolean;

private
   type Return_Values_Collection is record
      Get_Id                     : Agent.Agent_Id;
      Get_Maximum_Speed          : Natural;
      Get_Current_Speed          : Natural;
      Has_Next_Step              : Boolean;
      Is_Travelling              : Boolean;
      Equality_Operator          : Boolean;
      The_List                   : Infra_Id_List.List;
      Board                      : Boolean;
      Board_Called               : Boolean := False;
      Free                       : Boolean;
      Free_Called                : Boolean := False;
      Get_Passengers             : Agent_Id_List.List;
      Count_Passengers           : Natural;
      Affected_By_Traffic_Lights : Boolean;
      Get_Size                   : Natural;
   end record;

   type Object is
     new Vehicle.Object
   with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traveller.Vehicle.Mock;
