with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Traveller_Registry.Mock is

   function Create return Traveller_Registry.Mock.Reference
   is (new Traveller_Registry.Mock.Object);

   function Contains_Traveller (
      This         : in Traveller_Registry.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean
   is (This.Mock_Values.Population.Contains (Traveller_Id));

   function Find_Traveller_By_Id (
      This         : in Traveller_Registry.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference is
   begin
      if not This.Mock_Values.Population.Contains (Traveller_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Traveller_By_Id",
            Function_Param => "Traveller_Id = "
            --& Natural'Image (Traveller_Id),
            ,
            Package_Name   => "Reactive.Traveller_Registry.Mock");
      end if;

      return This.Mock_Values.Population.Element (Traveller_Id);
   end Find_Traveller_By_Id;

   procedure Add_Traveller (
      This      : in out Traveller_Registry.Mock.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean)
   is
      Traveller_Id : Agent.Agent_Id := Traveller.Get_Id;
   begin
      If This.Mock_Values.Population.Contains (Traveller_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Population
           .Insert (Key      => Traveller_Id,
                    New_Item => Traveller);
         Added := This.Mock_Values.Population.Contains (Traveller_Id);
      end if;
   end Add_Traveller;

   procedure Remove_Traveller (
      This         : in out Traveller_Registry.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Removed      :    out Boolean) is
   begin
      if not This.Mock_Values.Population.Contains (Traveller_Id) then
         Removed := FALSE;
      else
         This.Mock_Values.Population.Delete (Key => Traveller_Id);
         Removed := not This.Mock_Values.Population.Contains (Traveller_Id);
      end if;
   end Remove_Traveller;

end Reactive.Traveller_Registry.Mock;
