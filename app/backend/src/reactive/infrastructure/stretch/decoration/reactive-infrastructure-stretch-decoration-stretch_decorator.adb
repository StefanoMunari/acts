package body Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator is

   procedure Init
      (This     : in out Stretch_Decorator.Object;
       Stretch_Ref : in Reactive.Infrastructure.Stretch.Reference) is
   begin -- Init
      This.Stretch_Ref := Stretch_Ref;
   end Init;

   function Get_Stretch_Ref (This : in Stretch_Decorator.Object)
   return Reactive.Infrastructure.Stretch.Reference is (This.Stretch_Ref);

   function Get_Stretch_Ref_Id (This : in Stretch_Decorator.Object)
   return Infra_Id
   is (This.Stretch_Ref.Get_Id);

   function Get_Id (This : in Stretch_Decorator.Object) return Infra_Id
   is (This.Stretch_Ref.Get_Id);

   procedure Tread (This         : in out Stretch_Decorator.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean) is
   begin
      This.Stretch_Ref.Tread (Traveller_Id, Advanced);
   end Tread;

   function Find_Street (This : in Stretch_Decorator.Object)
                         return Infra_Id
   is (This.Stretch_Ref.Find_Street);

   function Find_Lane (This : in Stretch_Decorator.Object) return Infra_Id
   is (This.Stretch_Ref.Find_Lane);

   function Calculate_Position (This : in Stretch_Decorator.Object)
                                return Natural
   is (This.Stretch_Ref.Calculate_Position);

   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch_Decorator.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean
   is (This.Stretch_Ref.Is_Waiting_To_Enter_Stretch (Traveller_Id));

   procedure Leave (
      This         : in out Stretch_Decorator.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean) is
   begin
      This.Stretch_Ref.Leave (Traveller_Id, Left);
   end Leave;

   function Is_Before (This, Other: in Stretch_Decorator.Object) return Boolean
   is (This.Stretch_Ref.Is_Before (Stretch.Object'Class (Other)));

   procedure Set_Lane (This    : in out Stretch_Decorator.Object;
                       Lane_Id : in     Infra_Id) is
   begin
      This.Stretch_Ref.Set_Lane (Lane_Id);
   end Set_Lane;

   procedure Set_Host (
      This    : in out Stretch_Decorator.Object;
      Host_Id : in     Infra_Id) is
   begin
      This.Stretch_Ref.Set_Host (Host_Id);
   end Set_Host;

   function Get_Host (This : in Stretch_Decorator.Object)
   return Infra_Id
   is (This.Stretch_Ref.Get_Host);

   function Has_Host (This : in Stretch_Decorator.Object)
   return Boolean
   is (This.Stretch_Ref.Has_Host);

   function Find_Intersections (This : in Stretch_Decorator.Object)
                                return Infra_Id_Set.Set
   is (This.Stretch_Ref.Find_Intersections);

   function Get_Size (This : in Stretch_Decorator.Object) return Natural
   is (This.Stretch_Ref.Get_Size);

   function Is_Contained_By (This         : in Stretch_Decorator.Object;
                             Container_Id : in Infra_Id)
   return Boolean
   is (This.Stretch_Ref.Is_Contained_By (Container_Id));

   function Is_Empty (This : in Stretch_Decorator.Object) return Boolean
   is (This.Stretch_Ref.Is_Empty);

   procedure Put_Traveller (This         : in Stretch_Decorator.Object;
                            Traveller_Id : in Agent.Agent_Id)
   is
   begin
      This.Stretch_Ref.Put_Traveller (Traveller_Id);
   end Put_Traveller;

   function Dump (This : Stretch_Decorator.Object) return G_JSON.JSON_Value
   is (This.Stretch_Ref.Dump);

   function Get_Travellers_Queue (This : in Stretch_Decorator.Object)
   return access Stretch.Protected_Travellers_Queue
   is (This.Stretch_Ref.Get_Travellers_Queue);

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;
