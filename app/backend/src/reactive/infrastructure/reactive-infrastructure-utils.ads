with Active.Agent;
limited with Reactive.District;

with Shared.Direction;

package Reactive.Infrastructure.Utils is

   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   use Reactive.Infra_Id_Type;

   type Object (<>) is tagged limited private;
   type Reference is access all Infrastructure.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Infrastructure.Utils.Reference;

   not overriding
   function Exists (
      This              : in Infrastructure.Utils.Object;
      Infrastructure_Id : in Infra_Id) return Boolean;

   not overriding
   procedure Tread (
      This         : in     Infrastructure.Utils.Object;
      Old_Position : in     Infra_Id;
      Treadable_Id : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean);

private
   type Object is tagged limited record
      District           : access Reactive.District.Object'Class;
   end record;

   Instance : Infrastructure.Utils.Reference;

end Reactive.Infrastructure.Utils;
