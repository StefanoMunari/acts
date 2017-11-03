with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traveller.Pedestrian.Mock is

   function Create return Pedestrian.Mock.Reference
   is (new Pedestrian.Mock.Object);

   function Get_Id (This : in Pedestrian.Mock.Object) return Agent.Agent_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Id",
            Package_Name   => "Active.Traveller.Pedestrian.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Get_Maximum_Speed (This : in Pedestrian.Mock.Object)
   return Natural is
   begin
      if not This.Mock_Values.Maximum_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Maximum_Speed",
            Package_Name   => "Active.Traveller.Pedestrian.Mock");
      end if;

      return This.Mock_Values.Maximum_Speed;
   end Get_Maximum_Speed;

   function Get_Current_Speed (This : in Pedestrian.Mock.Object)
   return Natural is
   begin
      if not This.Mock_Values.Current_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Current_Speed",
            Package_Name   => "Active.Traveller.Pedestrian.Mock");
      end if;

      return This.Mock_Values.Current_Speed;
   end Get_Current_Speed;

   function Has_Next_Step (This : in Pedestrian.Mock.Object) return Boolean
   is (This.Return_Values.Has_Next_Step);

   function Is_Travelling (This : in Pedestrian.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Is_Travelling_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Is_Travelling",
            Package_Name   => "Active.Traveller.Pedestrian.Mock");
      end if;

      return This.Return_Values.Is_Travelling;
   end Is_Travelling;

   function "=" (This, Other : Pedestrian.Mock.Object) return Boolean
   is (This.Return_Values.Equality_Operator);

   procedure On_Bus_Stop (This : in out Pedestrian.Mock.Object) is
   begin
   -- TODO: implement mocked operation
     null;
   end On_Bus_Stop;

   procedure Set_Id (
      This  : in out Pedestrian.Mock.Object;
      Id    : in     Agent.Agent_Id) is
   begin
      This.Mock_Values.Id := Id;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Current_Speed (This      : in out Pedestrian.Mock.Object;
                                New_Speed : in     Natural) is
   begin
      This.Mock_Values.Current_Speed           := New_Speed;
      This.Mock_Values.Current_Speed_Existence := TRUE;
   end Set_Current_Speed;

   procedure Set_Maximum_Speed (
      This          : in out Pedestrian.Mock.Object;
      Maximum_Speed : in     Natural) is
   begin
      This.Mock_Values.Maximum_Speed           := Maximum_Speed;
      This.Mock_Values.Maximum_Speed_Existence := TRUE;
   end Set_Maximum_Speed;

   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Travelling (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Travelling           := Return_Value;
      This.Return_Values.Is_Travelling_Existence := TRUE;
   end Set_Return_Value_For_Is_Travelling;

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_On_Bus_Stop (
      This         : in out Pedestrian.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Mock_Values.On_Bus_Stop           := Return_Value;
      This.Mock_Values.On_Bus_Stop_Existence := TRUE;
   end Set_Return_Value_For_On_Bus_Stop;

end Active.Traveller.Pedestrian.Mock;
