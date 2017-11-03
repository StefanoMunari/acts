with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traveller.Mock is

   function Create return Traveller.Mock.Reference
   is (new Traveller.Mock.Object);

   function Get_Id (This : in Traveller.Mock.Object) return Agent.Agent_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Id",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Get_Stretch_Type (This : in Traveller.Mock.Object)
   return Stretch_Type is
   begin
      if not This.Return_Values.Get_Stretch_Type_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Stretch_Type",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Return_Values.Get_Stretch_Type;
   end Get_Stretch_Type;

   function Get_Maximum_Speed (This : in Traveller.Mock.Object)
   return Natural is
   begin
      if not This.Mock_Values.Maximum_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Maximum_Speed",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Mock_Values.Maximum_Speed;
   end Get_Maximum_Speed;

   function Get_Current_Speed (This : in Traveller.Mock.Object)
   return Natural is
   begin
      if not This.Mock_Values.Current_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Current_Speed",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Mock_Values.Current_Speed;
   end Get_Current_Speed;

   function Has_Next_Step (This : in Traveller.Mock.Object) return Boolean
   is (This.Return_Values.Has_Next_Step);

   function Is_Travelling (This : in Traveller.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Is_Travelling_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Is_Travelling",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Return_Values.Is_Travelling;
   end Is_Travelling;

   function "=" (This, Other : Traveller.Mock.Object) return Boolean
   is (This.Return_Values.Equality_Operator);

   function Get_List_From_Slice (This      : in Traveller.Mock.Object;
                                 The_Slice : in Slice.Map)
   return Infra_Id_List.List is (This.Return_Values.The_List);

   function Wait_For_Bus (This : in Traveller.Mock.Object)
                          return Boolean
   is (This.Return_Values.Wait_For_Bus);

   function Is_Affected_By_Traffic_Lights (This : in Mock.Object)
   return Boolean is
   begin
      if not This.Return_Values.Affected_By_Traffic_Lights_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Is_Affected_By_Traffic_Lights",
            Function_Param => "Affected_By_Traffic_Lights has not been set",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Return_Values.Affected_By_Traffic_Lights;
   end Is_Affected_By_Traffic_Lights;

   function Get_Size (This : in Mock.Object)
   return Natural is
   begin
      if not This.Return_Values.Get_Size_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Size",
            Function_Param => "Get_Size has not been set",
            Package_Name   => "Active.Traveller.Mock");
      end if;

      return This.Return_Values.Get_Size;
   end Get_Size;

   procedure Set_Id (
      This  : in out Traveller.Mock.Object;
      Id : in Agent.Agent_Id) is
   begin
      This.Mock_Values.Id := Id;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Return_Value_For_Get_Stretch_Type (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Stretch_Type) is
   begin
      This.Return_Values.Get_Stretch_Type := Return_Value;
      This.Return_Values.Get_Stretch_Type_Existence := True;
   end Set_Return_Value_For_Get_Stretch_Type;

   procedure Set_Current_Speed (This      : in out Traveller.Mock.Object;
                                New_Speed : in     Natural) is
   begin
      This.Mock_Values.Current_Speed := New_Speed;
      This.Mock_Values.Current_Speed_Existence := TRUE;
   end Set_Current_Speed;

   procedure Set_Maximum_Speed (
      This          : in out Traveller.Mock.Object;
      Maximum_Speed : in     Natural) is
   begin
      This.Mock_Values.Maximum_Speed := Maximum_Speed;
      This.Mock_Values.Maximum_Speed_Existence := TRUE;
   end Set_Maximum_Speed;

   procedure Set_Return_Value_For_Has_Next_Step (
      This  : in out Traveller.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Travelling (
      This  : in out Traveller.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Travelling := Return_Value;
      This.Return_Values.Is_Travelling_Existence := TRUE;
   end Set_Return_Value_For_Is_Travelling;

   procedure Set_Return_Value_For_Equality_Operator (
      This  : in out Traveller.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Tread_Street (
      This         : in out Traveller.Mock.Object;
      Street_Id    : in     Infra_Id;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Tread_Street.Insert (Key      => Street_Id,
                                              New_Item => Return_Value);
   end Set_Return_Value_For_Tread_Street;

   procedure Set_List_Value_For_Get_List_From_Slice (
      This     : in out Traveller.Mock.Object;
      The_List : in     Infra_Id_List.List) is
   begin
      This. Return_Values.The_List := The_List;
   end Set_List_Value_For_Get_List_From_Slice;

   procedure Set_Return_Value_For_Wait_For_Bus (
      This  : in out Traveller.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Wait_For_Bus := Return_Value;
   end Set_Return_Value_For_Wait_For_Bus;

   procedure Set_Return_Value_For_Is_Affected_By_Traffic_Lights (
      This  : in out Traveller.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Affected_By_Traffic_Lights := Return_Value;
      This.Return_Values.Affected_By_Traffic_Lights_Existence := TRUE;
   end Set_Return_Value_For_Is_Affected_By_Traffic_Lights;

   procedure Set_Return_Value_For_Get_Size (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Get_Size := Return_Value;
      This.Return_Values.Get_Size_Existence := TRUE;
   end Set_Return_Value_For_Get_Size;

end Active.Traveller.Mock;
