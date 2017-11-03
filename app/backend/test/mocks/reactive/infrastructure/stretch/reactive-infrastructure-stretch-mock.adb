with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Stretch.Mock is

   function Create return Stretch.Mock.Reference
   is
      Mock_Ref : Stretch.Mock.Reference := new Stretch.Mock.Object;
   begin
      Mock_Ref.Protected_Travellers_Queue := new Protected_Travellers_Queue;
      Mock_Ref.Protected_Travellers_Queue.Set_Size (10);
      return Mock_Ref;
   end Create;

   procedure Tread (This         : in out Stretch.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean) is
   begin
      if not This.Return_Values.Tread_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Advanced",
            Procedure_Name => "Tread",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      Advanced := This.Return_Values.Tread;
   end Tread;

   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Waiting_To_Enter_Stretch_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Waiting_To_Enter_Stretch",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Is_Waiting_To_Enter_Stretch;
   end Is_Waiting_To_Enter_Stretch;

   procedure Leave (
      This         : in out Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Leaved       : out Boolean) is
   begin
      if not This.Return_Values.Leave_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Leaved",
            Procedure_Name => "Get_Id",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      Leaved := This.Return_Values.Leave;
   end Leave;

   function Get_Id (This : in Stretch.Mock.Object) return Infra_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   function Find_Street (This : in Stretch.Mock.Object)
      return Infra_Id is
   begin
      if not This.Return_Values.Find_Street_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Street",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Find_Street;
   end Find_Street;

   function Find_Lane (This : in Stretch.Mock.Object)
      return Infra_Id is
   begin
      if not This.Return_Values.Find_Lane_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Lane",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Find_Lane;
   end Find_Lane;

   function Calculate_Position (This : in Stretch.Mock.Object)
      return Natural is
   begin
      if not This.Return_Values.Calculate_Position_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Calculate_Position",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Calculate_Position;
   end Calculate_Position;

   function Is_Before (This, Other: in Stretch.Mock.Object)
                       return Boolean is
   begin
      if not This.Return_Values.Is_Before_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Before",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Is_Before;
   end Is_Before;

   function Find_Intersections (
      This : in Stretch.Mock.Object) return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Intersections_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Intersections",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Find_Intersections;
   end Find_Intersections;

   function Is_Contained_By (This         : in Stretch.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Contained_By_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Contained_By",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Is_Contained_By;
   end Is_Contained_By;

   procedure Set_Lane (This    : in out Stretch.Mock.Object;
                       Lane_Id : in     Infra_Id) is
   begin
      This.Mock_Values.Lane_Id := Lane_Id;
      This.Mock_Values.Lane_Id_Existence := TRUE;
   end Set_Lane;

   procedure Set_Host (
      This    : in out Stretch.Mock.Object;
      Host_Id : in     Infra_Id) is
   begin
      This.Mock_Values.Set_Host_Id := Host_Id;
      This.Mock_Values.Set_Host_Id_Called := TRUE;
   end Set_Host;

   function Get_Host (This : in Stretch.Mock.Object) return Infra_Id is
   begin
      if not This.Return_Values.Get_Host_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Host",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Get_Host;
   end Get_Host;

   function Has_Host (This : in Stretch.Mock.Object) return Boolean is
   begin
      if not This.Return_Values.Has_Host_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Has_Host",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Has_Host;
   end Has_Host;

   procedure Set_Return_Value_For_Tread (
      This     : in out Stretch.Mock.Object;
      Advanced : in     Boolean) is
   begin
      This.Return_Values.Tread := Advanced;
      This.Return_Values.Tread_Existence := TRUE;
   end Set_Return_Value_For_Tread;

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
      This.Return_Values.Find_Street_Existence := TRUE;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Id (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Id := Return_Value;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

   procedure Set_Return_Value_For_Find_Lane (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Lane := Return_Value;
      This.Return_Values.Find_Lane_Existence := TRUE;
   end Set_Return_Value_For_Find_Lane;

   procedure Set_Return_Value_For_Calculate_Position (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Calculate_Position := Return_Value;
      This.Return_Values.Calculate_Position_Existence := TRUE;
   end Set_Return_Value_For_Calculate_Position;

   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Waiting_To_Enter_Stretch := Return_Value;
      This.Return_Values.Is_Waiting_To_Enter_Stretch_Existence := TRUE;
   end Set_Return_Value_For_Is_Waiting_To_Enter_Stretch;

   procedure Set_Return_Value_For_Leave (
      This   : in out Stretch.Mock.Object;
      Leaved : in     Boolean) is
   begin
      This.Return_Values.Leave := Leaved;
      This.Return_Values.Leave_Existence := TRUE;
   end Set_Return_Value_For_Leave;

   procedure Set_Return_Value_For_Is_Before (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Before := Return_Value;
      This.Return_Values.Is_Before_Existence := TRUE;
   end Set_Return_Value_For_Is_Before;

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
      This.Return_Values.Find_Intersections_Existence := TRUE;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
      This.Return_Values.Is_Contained_By_Existence := TRUE;
   end Set_Return_Value_For_Is_Contained_By;

   not overriding
   procedure Set_Return_Value_For_Get_Host (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Host := Return_Value;
      This.Return_Values.Get_Host_Existence := TRUE;
   end Set_Return_Value_For_Get_Host;

   not overriding
   procedure Set_Return_Value_For_Has_Host (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Has_Host := Return_Value;
      This.Return_Values.Has_Host_Existence := TRUE;
   end Set_Return_Value_For_Has_Host;

   function Get_Value_For_Set_Host_Id (This : in out Stretch.Mock.Object)
   return Infra_Id is
   begin
      if not This.Mock_Values.Set_Host_Id_Called then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Value_For_Set_Host_Id",
            Package_Name  => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Mock_Values.Set_Host_Id;
   end Get_Value_For_Set_Host_Id;

   overriding
   function Get_Travellers_Queue (This : in Stretch.Mock.Object)
   return access Stretch.Protected_Travellers_Queue is
   begin
      if not This.Return_Values.Get_Travellers_Queue_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Travellers_Queue",
            Package_Name  => "Reactive.Infrastructure.Stretch.Mock");
      end if;

      return This.Return_Values.Get_Travellers_Queue;
   end Get_Travellers_Queue;

   not overriding
   procedure Set_Return_Value_For_Get_Travellers_Queue (
      This         : in out Stretch.Mock.Object;
      Return_Value : access Stretch.Protected_Travellers_Queue) is
   begin
      This.Return_Values.Get_Travellers_Queue := Return_Value;
      This.Return_Values.Get_Travellers_Queue_Existence := TRUE;
   end Set_Return_Value_For_Get_Travellers_Queue;

end Reactive.Infrastructure.Stretch.Mock;
