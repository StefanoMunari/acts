with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Travel.Mock is

   function Create return Travel.Mock.Reference
   is (new Travel.Mock.Object);

   procedure Advance (This : in out Travel.Mock.Object) is
   begin
      This.Mock_Values.Advance_Called := True;
   end Advance;

   function Get_Route_Source (This : in Travel.Mock.Object)
   return Slice.Map is
   begin
      if not This.Return_Values.Get_Route_Source_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Route_Source",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Route_Source;
   end Get_Route_Source;

   function Get_Route_Destination (This : in Travel.Mock.Object)
   return Slice.Map is
   begin
      if not This.Return_Values.Get_Route_Destination_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Route_Destination",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Route_Destination;
   end Get_Route_Destination;

   function Get_Traveller_Id (
      This : in Travel.Mock.Object)
   return Agent.Agent_Id is
   begin
      if not This.Return_Values.Get_Traveller_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Traveller_Id",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Traveller_Id;
   end Get_Traveller_Id;

   function Is_Progressing (This : in Travel.Mock.Object) return boolean is
   begin
      if not This.Return_Values.Is_Progressing_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Progressing",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Is_Progressing;
   end Is_Progressing;

   function Has_Next_Step (This : in Travel.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Has_Next_Step_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Has_Next_Step",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Has_Next_Step;
   end Has_Next_Step;

   function Get_Current_Step_Id (This : in Travel.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Current_Step_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Current_Step",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Current_Step_Id;
   end Get_Current_Step_Id;

   function Get_Next_Step_Id (This : Travel.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Next_Step_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Next_Step",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Next_Step_Id;
   end Get_Next_Step_Id;

   function Get_Previous_Step_Id (This : Travel.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Previous_Step_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Previous_Step",
            package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Previous_Step_Id;
   end Get_Previous_Step_Id;

   function Get_First_Step_Id (This : Travel.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_First_Step_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_First_Step_Id",
            Package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_First_Step_Id;
   end Get_First_Step_Id;

   function Get_Last_Step_Id (This : Travel.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Last_Step_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Last_Step_Id",
            Package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Get_Last_Step_Id;
   end Get_Last_Step_Id;

   function Contains (This :    Travel.Mock.Object;
                      Step : in Infra_Id)
   return Boolean is
   begin
      if not This.Return_Values.Contains.Contains (Step) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Contains",
            Package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Contains.Element (Step);
   end Contains;

   function Contains (This        : Travel.Mock.Object;
                      Steps_Slice : in Slice.Map)
   return Boolean is
   begin
      if not This.Return_Values.Contains_Slice_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Contains",
            Package_Name  => "Active.Travel.Mock");
      end if;

      return This.Return_Values.Contains_Slice;
   end Contains;

   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
      This.Return_Values.Has_Next_Step_Existence := TRUE;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Progressing (
      This         : in out Travel.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Progressing := Return_Value;
      This.Return_Values.Is_Progressing_Existence := TRUE;
   end Set_Return_Value_For_Is_Progressing;

   procedure Set_Return_Value_For_Get_Route_Source (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Slice.Map) is
   begin
      This.Return_Values.Get_Route_Source := Return_Value;
      This.Return_Values.Get_Route_Source_Existence := TRUE;
   end Set_Return_Value_For_Get_Route_Source;

   procedure Set_Return_Value_For_Get_Route_Destination (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Slice.Map) is
   begin
      This.Return_Values.Get_Route_Destination := Return_Value;
      This.Return_Values.Get_Route_Destination_Existence := TRUE;
   end Set_Return_Value_For_Get_Route_Destination;

   procedure Set_Return_Value_For_Get_Traveller_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Agent.Agent_Id) is
   begin
      This.Return_Values.Get_Traveller_Id := Return_Value;
      This.Return_Values.Get_Traveller_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Traveller_Id;

   procedure Set_Return_Value_For_Get_Current_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in Infra_Id) is
   begin
      This.Return_Values.Get_Current_Step_Id := Return_Value;
      This.Return_Values.Get_Current_Step_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Current_Step_Id;

   procedure Set_Return_Value_For_Get_Next_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in Infra_Id) is
   begin
      This.Return_Values.Get_Next_Step_Id := Return_Value;
      This.Return_Values.Get_Next_Step_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Next_Step;

   procedure Set_Return_Value_For_Get_Previous_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in Infra_Id) is
   begin
      This.Return_Values.Get_Previous_Step_Id := Return_Value;
      This.Return_Values.Get_Previous_Step_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Previous_Step;

   procedure Set_Return_Value_For_Get_First_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_First_Step_Id := Return_Value;
      This.Return_Values.Get_First_Step_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_First_Step_Id;

   procedure Set_Return_Value_For_Get_Last_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Last_Step_Id := Return_Value;
      This.Return_Values.Get_Last_Step_Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Last_Step_Id;

   procedure Set_Return_Value_For_Contains (
      This         : in out Travel.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Boolean)
   is
   begin
   -- Usage of insert is on purpose: prevent multiple tests from colliding
      This.Return_Values.Contains.Insert (Stretch_Id, Return_Value);
   end Set_Return_Value_For_Contains;

   procedure Set_Return_Value_For_Contains_Slice (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Contains_Slice := Return_Value;
      This.Return_Values.Contains_Slice_Existence := True;
   end Set_Return_Value_For_Contains_Slice;

   function Get_Advance_Called (This : in out Travel.Mock.Object)
   return Boolean
   is (This.Mock_Values.Advance_Called);

   function Get_Travel_State (
      This : in Travel.Mock.Object)
      return access Travel.Travel_State.Object'Class is
   begin
      return This.Return_Values.Travel_State;
   end Get_Travel_State;

   procedure Change_Travel_State (
      This         : in out Travel.Mock.Object;
      Travel_State : access Travel.Travel_State.Object'Class) is
   begin
      This.Return_Values.Travel_State := Travel_State;
   end Change_Travel_State;

   procedure Clear_Route (
      This       : in out Travel.Mock.Object;
      Clean      : out Boolean) is
   begin
      This.Return_Values.Route.Clear;
      Clean := This.Return_Values.Route.Is_Empty;
   end Clear_Route;

   procedure Prepend_Step (
      This    : in out Travel.Mock.Object;
      Step_Id : in Infra_Id) is
   begin
      This.Return_Values.Route.Prepend (Step_Id);
   end Prepend_Step;

   function Get_Route (This : Travel.Mock.Object)
   return Infra_Id_List.List is
   begin
      return This.Return_Values.Route;
   end Get_Route;

end Active.Travel.Mock;
