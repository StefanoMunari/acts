with Active.Agent;
with Active.Traveller.Utils;

with Reactive.Infrastructure.Street_Related_Infrastructure;
limited with Reactive.Infrastructure.Lane.Utils;
with Reactive.Infrastructure.Street.Utils;

with Shared.Agent_Id_To_Infra_Id_Map;
with Shared.Containable;
with Shared.Direction;
with Shared.Infra_Id_Set;
with Shared.Infra_Id_List;
with Shared.Natural_List;

package Reactive.Infrastructure.Way is

   package Agent renames Active.Agent;
   package Traveller renames Active.Traveller;
   package Street renames Reactive.Infrastructure.Street;
   package Agent_Id_To_Infra_Id_Map renames Shared.Agent_Id_To_Infra_Id_Map;
   package Containable renames Shared.Containable;
   package Direction renames Shared.Direction;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Infra_Id_Set renames Shared.Infra_Id_Set;
   package Natural_List renames Shared.Natural_List;
   use Reactive.Infra_Id_Type;

   type Object is
     abstract new Street_Related_Infrastructure.Object
     and Infrastructure.Object
     and Containable.Object
   with private;
   type Reference is access all Way.Object'Class;

   overriding
   function Find_Street (This : in Way.Object)
                         return Infra_Id;

   overriding
   function Get_Id (This : in Way.Object) return Infra_Id;

   overriding
   function Is_Contained_By (This         : in Way.Object;
                             Container_Id : in Infra_Id) return Boolean;

   not overriding
   procedure Add_Lane (This    : in out Way.Object;
                       Lane_Id : in     Infra_Id;
                       Added   :    out Boolean);

   not overriding
   procedure Validate (This : in out Way.Object);

   not overriding
   procedure Find_Lane_By_Direction (
      This             : in     Way.Object;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean);

   not overriding
   procedure Set_Street (This      :    out Way.Object;
                         Street_Id : in     Infra_Id);

   not overriding
   function Get_Stretches_On_Other_Lane (
      This         : Way.Object;
      Lane_Id      : Infra_Id;
      Stretch_List : Natural_List.List)
   return Infra_Id_List.List;

   overriding
   function Dump (This : Way.Object) return G_JSON.JSON_Value;

   function Id_Field    return String is ("id");
   function Lanes_Field return String is ("lanes");

private
   protected type Protected_Object is

      not overriding
      function Find_Last_Traveller_Lane (Traveller_Id : in Agent.Agent_Id)
      return Infra_Id;

      not overriding
      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean;

   private
      Travellers : Agent_Id_To_Infra_Id_Map.Map;
   end Protected_Object;

   type Object is
     abstract new Street_Related_Infrastructure.Object
     and Infrastructure.Object
     and Containable.Object with record
      Id           : Infra_Id;
      Street_Id    : Infra_Id;
      Direct_Lane  : Infra_Id;
      Inverse_Lane : Infra_Id;
      Direct_Lane_Existence : Boolean := FALSE;
      Inverse_Lane_Existence : Boolean := FALSE;
      Protected_Object : access Way.Protected_Object
        := new Way.Protected_Object;
      Traveller_Utils : access Traveller.Utils.Object'Class;
      Street_Utils : access Street.Utils.Object'Class;
      Lane_Utils : access Lane.Utils.Object'Class;
   end record;

   procedure Init (
      Way : in out Infrastructure.Way.Object'Class;
      Id  : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null);

end Reactive.Infrastructure.Way;
