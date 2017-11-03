with Reactive.Infrastructure.Stretch;

package Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator is

   type Object is
     abstract new Reactive.Infrastructure.Stretch.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Init (This     : in out Stretch_Decorator.Object;
                   Stretch_Ref : in Reactive.Infrastructure.Stretch.Reference);

   not overriding
   function Get_Stretch_Ref (This : in Stretch_Decorator.Object)
   return Reactive.Infrastructure.Stretch.Reference;

   not overriding
   function Get_Stretch_Ref_Id (This : in Stretch_Decorator.Object)
   return Infra_Id;

   overriding
   function Get_Id (This : in Stretch_Decorator.Object) return Infra_Id;

   overriding
   procedure Tread (This         : in out Stretch_Decorator.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   overriding
   function Find_Street (This : in Stretch_Decorator.Object)
                         return Infra_Id;

   overriding
   function Find_Lane (This : in Stretch_Decorator.Object) return Infra_Id;

   overriding
   function Calculate_Position (This : in Stretch_Decorator.Object)
                                return Natural;

   overriding
   function Is_Waiting_To_Enter_Stretch (
    This         : in Stretch_Decorator.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean;

   overriding
   procedure Leave (
      This         : in out Stretch_Decorator.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   overriding
   function Is_Before (This, Other: in Stretch_Decorator.Object) return Boolean;

   overriding
   procedure Set_Lane (This    : in out Stretch_Decorator.Object;
                       Lane_Id : in     Infra_Id);

   overriding
   procedure Set_Host (
      This    : in out Stretch_Decorator.Object;
      Host_Id : in     Infra_Id);

   overriding
   function Get_Host (This : in Stretch_Decorator.Object)
   return Infra_Id;

   overriding
   function Has_Host (This : in Stretch_Decorator.Object)
   return Boolean;

   overriding
   function Find_Intersections (This : in Stretch_Decorator.Object)
                                return Infra_Id_Set.Set;

   overriding
   function Get_Size (This : in Stretch_Decorator.Object) return Natural;

   overriding
   function Is_Contained_By (This         : in Stretch_Decorator.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   overriding
   function Is_Empty (This : in Stretch_Decorator.Object) return Boolean;

   overriding
   procedure Put_Traveller (This         : in Stretch_Decorator.Object;
                            Traveller_Id : in Agent.Agent_Id);

   overriding
   function Dump (This : Stretch_Decorator.Object) return G_JSON.JSON_Value;

private

   type Object is
     abstract new Reactive.Infrastructure.Stretch.Object with record
      Stretch_Ref : Reactive.Infrastructure.Stretch.Reference;
   end record;

   overriding
   function Get_Travellers_Queue (This : in Stretch_Decorator.Object)
   return access Stretch.Protected_Travellers_Queue;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;
