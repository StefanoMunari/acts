-- core
with Ada.Strings.Unbounded;

-- local
with Active.Agent;
with Active.Traveller;

with Interface_Layer.Utils.Marshaller;
with Interface_Layer.Utils.Unmarshaller;

with Reactive;

with Shared.Indefinite_String_Map;
with Shared.Infra_Id_List;
with Shared.Slice;

package Interface_Layer.Utils.Explorer is

   package SU            renames Ada.Strings.Unbounded;
   package Agent         renames Active.Agent;
   package Traveller     renames Active.Traveller;
   package Marshaller    renames Interface_Layer.Utils.Marshaller;
   package Unmarshaller  renames Interface_Layer.Utils.Unmarshaller;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package String_Map    renames Shared.Indefinite_String_Map;
   package Slice         renames Shared.Slice;
   use Reactive.Infra_Id_Type;

   type Object is abstract
      new  Marshaller.Object
      and Unmarshaller.Object
   with record
      Id            : Agent.Agent_Id;
      Maximum_Speed : Natural;
      Current_Speed : Natural;
      Position      : Infra_Id;
      Route         : Infra_Id_List.List;
      Source        : Slice.Map;
      Destination   : Slice.Map;
   end record;
   type Reference is access all Explorer.Object'Class;

   procedure Init (
      This          : in out Explorer.Object'Class;
      Id            :        Agent.Agent_Id;
      Maximum_Speed : in     Natural;
      Current_Speed : in     Natural;
      Position      : in     Infra_Id;
      Route         :        Infra_Id_List.List;
      Source        : in     Slice.Map;
      Destination   : in     Slice.Map);

   overriding
   procedure Marshalling (
      This       : in     Explorer.Object;
      Stream_Map :    out String_Map.Data.Map);

   overriding
   procedure Unmarshalling (
      This       : in out Explorer.Object;
      Stream_Map : in     String_Map.Data.Map);

private
   function Separator       return String is (",");
   function Source_Key      return String is ("Source");
   function Destination_Key return String is ("Destination");

   function Encode_List (Id_List : Infra_Id_List.List)
   return SU.Unbounded_String;

end Interface_Layer.Utils.Explorer;
