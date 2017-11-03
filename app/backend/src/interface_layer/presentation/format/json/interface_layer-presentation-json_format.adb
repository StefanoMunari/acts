package body Interface_Layer.Presentation.JSON_Format is

   procedure Set_Header (
      Message : in out JSON_Format.Object;
      Header  : in     XFormat.Reference)
   is
      Aux_Message : G_JSON.JSON_Value := G_JSON.Create_Object;
      Aux_Header  : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
   -- Initialize Message with a Object
      Message     := JSON_Format.Create_Object;
      Aux_Message := G_JSON.JSON_Value (Message);
      Aux_Header  := G_JSON.JSON_Value (Header.all);
      Aux_Message.Set_Field (
         Field_Name => "Header",    -- Key
         Field      => Aux_Header); -- Value
   end Set_Header;

   procedure Set_Payload (
      Message : in out JSON_Format.Object;
      Payload : in     XFormat.Reference)
   is
      Aux_Message : G_JSON.JSON_Value := G_JSON.Create_Object;
      Aux_Payload : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      Aux_Message := G_JSON.JSON_Value (Message);
      Aux_Payload := G_JSON.JSON_Value (Payload.all);
      Aux_Message.Set_Field (
         Field_Name => "Payload",    -- Key
         Field      => Aux_Payload); -- Value
   end Set_Payload;

   function Get_Header (Message : in out JSON_Format.Object)
   return XFormat.Object'Class is
   begin
      return Message.Get ("Header");
   end Get_Header;

   function Get_Payload (Message : in out JSON_Format.Object)
   return XFormat.Object'Class is
   begin
         return Message.Get ("Payload");
   end Get_Payload;

   function "=" (A, B : JSON_Format.Object) return Boolean is
   begin
      return JSON_Format.Write (A) = JSON_Format.Write(B);
   end "=";

end Interface_Layer.Presentation.JSON_Format;
