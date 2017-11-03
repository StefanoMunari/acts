with Active.Agent;
with Active.Bus_Service.Mock;

limited with Reactive.District.Mock;

package Active.Bus_Service.Utils.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is new Bus_Service.Utils.Object with private;
   type Reference is access all Bus_Service.Utils.Mock.Object'Class;

   function Create return Bus_Service.Utils.Mock.Reference;

   overriding
   function Is_A_Bus_Service (
      This         : in out Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

   overriding
   procedure On_Bus_Stop (This         : in out Mock.Object;
                          Traveller_Id : in     Agent.Agent_Id);

   not overriding
   procedure Set_Traveller_For_On_Bus_Stop (
      This            : in out Mock.Object;
      Traveller_Id    : in     Agent.Agent_Id;
      Bus_Service_Ref : in     Bus_Service.Mock.Reference);

private
   type Return_Values_Collection is record
      Bus_Service_Ref : Active.Bus_Service.Mock.Reference;
      Bus_Service_Ref_Existence : Boolean := FALSE;
      Traveller_Id : Agent.Agent_Id;
      Traveller_Id_Existence : Boolean := FALSE;
   end record;

   type Object is new Bus_Service.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Bus_Service.Utils.Mock;
