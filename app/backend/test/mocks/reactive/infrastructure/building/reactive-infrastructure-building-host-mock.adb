with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Building.Host.Mock is

   function Create return Building.Host.Mock.Reference
   is  (new Host.Mock.Object);

   function Get_Id (This : in Host.Mock.Object) return Infra_Id is
   begin
      if not This.Return_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Id",
            Procedure_Name => "Get_Id",
            Package_Name
            => "Reactive.Infrastructure.Building.Host.Mock");
      end if;

     return This.Return_Values.Id;
   end;

   overriding
   function Get_Parking (This       : in out Host.Mock.Object)
   return Parking_Manager.Reference is
   begin
      if not This.Return_Values.Get_Parking_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Return value",
            Procedure_Name => "Get_Parking",
            Package_Name
            => "Reactive.Infrastructure.Building.Host.Mock");
      end if;

     return This.Return_Values.Get_Parking;
   end Get_Parking;

   overriding
   procedure Stop_Over (This       : in out Host.Mock.Object;
                        Travellers : in     Agent_Id_List.List) is
   begin
      This.Mock_Values.Stop_Over_Called := True;
   end Stop_Over;

   overriding
   function Exit_Building (This         : in out Host.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Next_Action is
   begin
      if not This.Return_Values.Exit_Building_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Return value",
            Procedure_Name => "Exit_Building",
            Package_Name
            => "Reactive.Infrastructure.Building.Host.Mock");
      end if;

      Vehicle_Id := This.Return_Values.Exit_Building_Vehicle;
     return This.Return_Values.Exit_Building_Action;
   end Exit_Building;

   overriding
   procedure Put_Stopping_Traveller (This         : in out Host.Mock.Object;
                                     Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.Mock_Values.Put_Stopping_Traveller_Called := True;
   end Put_Stopping_Traveller;

   procedure Accessible_By (
      This       : in out Host.Mock.Object;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type) is
   begin
      This.Mock_Values.Accessible_By_Called.Append (Stretch_Id);
   end Accessible_By;

   overriding
   function Dump (This    : Host.Mock.Object)
   return G_JSON.JSON_Value is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Return value",
            Procedure_Name => "Dump",
            Package_Name
            => "Reactive.Infrastructure.Building.Host.Mock");
      end if;

     return This.Return_Values.Dump;
   end Dump;

   procedure Set_Return_Value_For_Get_Id (
      This         : in out Host.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Id := Return_Value;
      This.Return_Values.Id_Existence := TRUE;
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Get_Parking (
      This         : in out Host.Mock.Object;
      Return_Value : in     Parking_Manager.Reference) is
   begin
      This.Return_Values.Get_Parking := Return_Value;
      This.Return_Values.Get_Parking_Existence := True;
   end Set_Return_Value_For_Get_Parking;

   procedure Set_Return_Values_For_Exit_Building (
      This    : in out Host.Mock.Object;
      Vehicle : in     Agent.Agent_Id;
      Action  : in     Next_Action) is
   begin
      This.Return_Values.Exit_Building_Action := Action;
      This.Return_Values.Exit_Building_Vehicle := Vehicle;
      This.Return_Values.Exit_Building_Existence := TRUE;
   end Set_Return_Values_For_Exit_Building;

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Host.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := True;
   end Set_Return_Value_For_Dump;

   function Get_Stop_Over_Called (
      This         : in out Building.Host.Mock.Object) return Boolean
   is (This.Mock_Values.Stop_Over_Called);

   function Get_Exit_Building_Called (
      This         : in out Building.Host.Mock.Object) return Boolean
   is (This.Mock_Values.Exit_Building_Called);

   not overriding
   function Get_Put_Stopping_Traveller_Called (
      This : in out Building.Host.Mock.Object) return Boolean
   is (This.Mock_Values.Put_Stopping_Traveller_Called);

   function Get_Accessible_By_Called (
      This       : in out Building.Host.Mock.Object;
      Stretch_Id : in     Infra_Id) return Boolean is
   begin
      return This.Mock_Values.Accessible_By_Called.Contains (Stretch_Id);
   end Get_Accessible_By_Called;

end Reactive.Infrastructure.Building.Host.Mock;
