with Active.Agent;

with Reactive.Identifiable;

package Reactive.Treadable is

   package Agent renames Active.Agent;
   package Identifiable renames Reactive.Identifiable;

   type Object is interface
      and Identifiable.Object;
   type Reference is access all Treadable.Object'Class;

   not overriding
   procedure Tread (
      This         : in out Treadable.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is abstract;

   not overriding
   procedure Leave (
      This         : in out Treadable.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean) is abstract;

end Reactive.Treadable;
