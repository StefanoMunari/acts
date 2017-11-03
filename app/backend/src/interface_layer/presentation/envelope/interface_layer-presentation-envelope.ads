-- local
with Interface_Layer.Utils.Types;

with Reactive;

with Shared.Indefinite_String_Map;

package Interface_Layer.Presentation.Envelope is

   package String_Map renames Shared.Indefinite_String_Map;
   package Types renames Interface_Layer.Utils.Types;

   use Reactive.Infra_Id_Type;
   use Types.Recipient_Type_Pkg;

   type Object is tagged private;
   type Reference is access all Envelope.Object'Class;

   function Create return Envelope.Reference;

   function Get_Header (This : in Envelope.Object) return String_Map.Data.Map;

   procedure Set_Header (
      This       : in out Envelope.Object;
      Request_Id : in     String);

   procedure Set_Header (
      This    : in out Envelope.Object;
      Request :        Types.Request_Type);

   procedure Set_Header (
      This    : in out Envelope.Object;
      Content :        Types.Data_Type);

   procedure Set_Header (
      This : in out Envelope.Object;
      Call :        Types.Call_Type);

   procedure Set_Header (
      This      : in out Envelope.Object;
      Recipient :        Recipient_Type);

   procedure Set_Header (
      This    : in out Envelope.Object;
      Raw_Map :        String_Map.Data.Map);

   function Get_Message (This : in Envelope.Object) return String_Map.Data.Map;

   procedure Set_Message (
      This    : in out Envelope.Object;
      Raw_Map :        String_Map.Data.Map);

   function "=" (A, B : Envelope.Object) return Boolean;

   -- DEBUG
   procedure Debug (This : in Envelope.Object);

private
   type Object is tagged
   record
      Header  : String_Map.Data.Map;
      Message : String_Map.Data.Map;
   end record;

end Interface_Layer.Presentation.Envelope;
