-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

with Passive.Road_Sign.Bus_Stop;

with Reactive.District;
with Reactive.Infrastructure.Lane;
with Reactive.Infrastructure.Lane.Decoration.Lane_Decorator;
with Reactive.Infrastructure.Stretch;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;

with Shared.Agent_Id_List;
with Shared.Direction;
with Shared.Infra_Id_List;

package Reactive.Infrastructure.Factory.Street_Factory is

   package G_JSON        renames GNATCOLL.JSON;
   package Agent renames Active.Agent;
   package Bus_Stop_Pkg
      renames Passive.Road_Sign.Bus_Stop;
   package Lane_Decorator
      renames Reactive.Infrastructure.Lane.Decoration.Lane_Decorator;
   package Stretch_Decorator
      renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;
   use Shared.Direction;

   type Object is abstract tagged private;
   type Reference is access all Object'Class;

   not overriding
   procedure Init (
      This           : in out Street_Factory.Object;
      District       : access Reactive.District.Object'Class := null);

   not overriding
   function Decorate_Stretch (
      This        : in out Street_Factory.Object;
      Stretch_Ref : in     Stretch.Reference;
      Decoration  : in     G_JSON.JSON_Value)
   return Reactive.Infrastructure.Stretch.Reference;

   not overriding
   function Decorate_Lane (
      This       : in out Street_Factory.Object;
      Lane_Ref   : in     Lane.Reference;
      Decoration : in     G_JSON.JSON_Value)
   return Reactive.Infrastructure.Lane.Reference;

   not overriding
   function Create_Stretch (This : in out Street_Factory.Object)
      return Stretch.Reference is abstract;

   not overriding
   procedure Set_Stretch_Id (This : in out Street_Factory.Object;
                             Id   : in     Infra_Id);

   not overriding
   procedure Set_Stretch_Size (This : in out Street_Factory.Object;
                               Size : in     Natural);

   not overriding
   procedure Set_Stretch_Travellers (
      This       : in out Street_Factory.Object;
      Travellers : in     Agent_Id_List.List);

   not overriding
   function Create_Lane (This : in out Street_Factory.Object)
      return Lane.Reference is abstract;

   not overriding
   procedure Set_Lane_Id (This : in out Street_Factory.Object;
                          Id   : in     Infra_Id);

   not overriding
   procedure Set_Lane_Direction (
      This      : in out Street_Factory.Object;
      Direction : in     Shared.Direction.Straight);

   not overriding
   procedure Set_Lane_Stretches (
      This      : in out Street_Factory.Object;
      Stretches : in     Infra_Id_List.List);

   Bus_Stop_Field    : constant String := Bus_Stop_Pkg.Bus_Stop_Field;
   Bus_Id_Field      : constant String := Bus_Stop_Pkg.Id_Field;
   Bus_Waiting_Field : constant String := Bus_Stop_Pkg.Waiting_List_Field;
   Stops_Field       : constant String := Bus_Stop_Pkg.Stops_Field;

private

   type Object is abstract tagged record
     District           : access Reactive.District.Object'Class;
     Stretch_Id         : Infra_Id;
     Stretch_Size       : Natural;
     Stretch_Travellers : Agent_Id_List.List;
     Lane_Id            : Infra_Id;
     Lane_Direction     : Shared.Direction.Straight;
     Lane_Stretches     : Infra_Id_List.List;
   end record;

   function Get_Id_For_Bus (Bus_Stop_JSON : in G_JSON.JSON_Value)
   return Agent.Agent_Id;

   function Get_Waiting_List_For_Bus (Bus_Stop_JSON : G_JSON.JSON_Value)
   return Agent_Id_List.List;

   function Get_Stops_For_Bus (Bus_Stop_JSON : G_JSON.JSON_Value)
   return Infra_Id_List.List;

end Reactive.Infrastructure.Factory.Street_Factory;
