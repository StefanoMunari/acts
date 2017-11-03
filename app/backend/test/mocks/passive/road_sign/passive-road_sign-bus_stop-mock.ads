package Passive.Road_Sign.Bus_Stop.Mock is

   type Object is
      new Bus_Stop.Object
   with private;
   type Reference is access all Bus_Stop.Mock.Object'Class;

   not overriding
   function Create
   return Road_Sign.Bus_Stop.Mock.Reference;

   overriding
   procedure Apply (This       : in out Bus_Stop.Mock.Object;
                    Traveller  : in     Agent.Agent_Id)
   is null;

   overriding
   function Get_All_Bus_Stops (This : in Bus_Stop.Mock.Object)
   return Agent_Id_To_Infra_Id_List_Map.Map;

   overriding
   procedure Register_For_Bus_Waiting (
      This       : in out Bus_Stop.Mock.Object;
      Waiting_Id : in     Agent.Agent_Id;
      Bus_Id     : in     Agent.Agent_Id);

   overriding
   function Get_Waiting_For_Bus (
      This   : in out Bus_Stop.Mock.Object;
      Bus_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

   overriding
   procedure Remove_From_Waiting_List (
      This                   : in out Bus_Stop.Mock.Object;
      Bus_Id                 : in     Agent.Agent_Id;
      Not_Waiting_Anymore_Id : in     Agent.Agent_Id);

   overriding
   function Is_A_Stop_For_Bus (This : in out Bus_Stop.Mock.Object;
                               Bus  : in     Agent.Agent_Id)
   return Boolean;

   overriding
   function Dump (This : Bus_Stop.Mock.Object) return G_JSON.JSON_Value;

   not overriding
   procedure Set_Return_Value_For_Get_All_Bus_Stops (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     Agent_Id_To_Infra_Id_List_Map.Map);

   not overriding
   procedure Set_Return_Value_For_Get_Waiting_For_Bus (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     Agent_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

   not overriding
   function Get_Register_For_Bus_Waiting_Called (
      This : in out Bus_Stop.Mock.Object)
   return Boolean;

private
   type Return_Values_Collection is record
      Is_A_Stop_For_Bus : Boolean;
      Is_A_Stop_For_Bus_Existence : Boolean := FALSE;
      Get_All_Bus_Stops : Agent_Id_To_Infra_Id_List_Map.Map;
      Get_All_Bus_Stops_Existence : Boolean := FALSE;
      Get_Waiting_For_Bus : Agent_Id_List.List;
      Get_Waiting_For_Bus_Existence : Boolean := FALSE;
      Dump : G_JSON.JSON_Value;
      Dump_Existence : Boolean := FALSE;
   end record;

   type Mock_Values_Collection is record
      Register_For_Bus_Waiting_Called : Boolean := False;
   end record;

   type Object is
     new Road_Sign.Bus_Stop.Object
   with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

end Passive.Road_Sign.Bus_Stop.Mock;
