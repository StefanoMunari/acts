with Mock.Exceptions;
use Mock.Exceptions;

package body Passive.Road_Sign.Bus_Stop.Mock is

   function Create
   return Road_Sign.Bus_Stop.Mock.Reference
   is (new Bus_Stop.Mock.Object);

   function Get_All_Bus_Stops (This : in Bus_Stop.Mock.Object)
   return Agent_Id_To_Infra_Id_List_Map.Map is
   begin
      if not This.Return_Values.Get_All_Bus_Stops_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_All_Bus_Stops",
            Package_Name  => "Passive.Road_Sign.Bus_Stop.Mock");
      end if;

      return This.Return_Values.Get_All_Bus_Stops;
   end Get_All_Bus_Stops;

   procedure Register_For_Bus_Waiting (
      This       : in out Bus_Stop.Mock.Object;
      Waiting_Id : in     Agent.Agent_Id;
      Bus_Id     : in     Agent.Agent_Id) is
   begin
      This.Mock_Values.Register_For_Bus_Waiting_Called := True;
   end Register_For_Bus_Waiting;

   function Get_Waiting_For_Bus (
      This   : in out Bus_Stop.Mock.Object;
      Bus_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List is
   begin
      if not This.Return_Values.Get_Waiting_For_Bus_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Waiting_For_Bus",
            Package_Name  => "Passive.Road_Sign.Bus_Stop.Mock");
      end if;

      return This.Return_Values.Get_Waiting_For_Bus;
   end Get_Waiting_For_Bus;

   procedure Remove_From_Waiting_List (
      This                   : in out Bus_Stop.Mock.Object;
      Bus_Id                 : in     Agent.Agent_Id;
      Not_Waiting_Anymore_Id : in     Agent.Agent_Id) is
   begin
   -- TODO: Mock operation
      null;
   end Remove_From_Waiting_List;

   function Is_A_Stop_For_Bus (This : in out Bus_Stop.Mock.Object;
                               Bus  : in     Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Is_A_Stop_For_Bus_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Is_A_Stop_For_Bus",
            Package_Name  => "Passive.Road_Sign.Bus_Stop.Mock");
      end if;

      return This.Return_Values.Is_A_Stop_For_Bus;
   end Is_A_Stop_For_Bus;

   function Dump (This : Bus_Stop.Mock.Object) return G_JSON.JSON_Value is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Dump",
            Package_Name  => "Passive.Road_Sign.Bus_Stop.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   not overriding
   procedure Set_Return_Value_For_Get_All_Bus_Stops (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     Agent_Id_To_Infra_Id_List_Map.Map) is
   begin
      This.Return_Values.Get_All_Bus_Stops := Return_Value;
      This.Return_Values.Get_All_Bus_Stops_Existence := TRUE;
   end Set_Return_Value_For_Get_All_Bus_Stops;

   not overriding
   procedure Set_Return_Value_For_Get_Waiting_For_Bus (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     Agent_Id_List.List)
   is
   begin
      This.Return_Values.Get_Waiting_For_Bus := Return_Value;
      This.Return_Values.Get_Waiting_For_Bus_Existence := True;
   end Set_Return_Value_For_Get_Waiting_For_Bus;

   procedure Set_Return_Value_For_Dump (
      This         : in out Bus_Stop.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := TRUE;
   end Set_Return_Value_For_Dump;

   function Get_Register_For_Bus_Waiting_Called (
      This : in out Bus_Stop.Mock.Object)
   return Boolean is
   begin
      return This.Mock_Values.Register_For_Bus_Waiting_Called;
   end Get_Register_For_Bus_Waiting_Called;

end Passive.Road_Sign.Bus_Stop.Mock;
