package Reactive.Infrastructure.Utils.Mock is

   type Object (<>) is new Infrastructure.Utils.Object with private;
   type Reference is access all Infrastructure.Utils.Mock.Object'Class;

   function Create return Infrastructure.Utils.Mock.Reference;

   overriding
   function Exists (
      This : in Infrastructure.Utils.Mock.Object;
      Infrastructure_Id : in Infra_Id) return Boolean;

   overriding
   procedure Tread (
      This              : in     Infrastructure.Utils.Mock.Object;
      Old_Position      : in     Infra_Id;
      Infrastructure_Id : in     Infra_Id;
      Traveller_Id      : in     Agent.Agent_Id;
      Advanced          :    out Boolean);

   not overriding
   procedure Set_Return_Value_For_Exists (
      This         : in out Infrastructure.Utils.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Tread (
      This         : in out Infrastructure.Utils.Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Exists : Boolean;
      Exists_Existence : Boolean := FALSE;
      Tread : Boolean;
      Tread_Existence : Boolean := FALSE;
   end record;

   type Object is new Infrastructure.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Utils.Mock;
