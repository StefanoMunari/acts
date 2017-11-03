package Interface_Layer.Presentation.XFormat is

   type Object is interface;
   type Reference is access all XFormat.Object'Class;

   procedure Set_Header (
      Message : in out XFormat.Object;
      Header : in XFormat.Reference)
   is abstract;

   procedure Set_Payload (
      Message : in out XFormat.Object;
      Payload : in XFormat.Reference)
   is abstract;

   function Get_Header (Message : in out XFormat.Object)
   return XFormat.Object'Class is abstract;

   function Get_Payload (Message : in out XFormat.Object)
   return XFormat.Object'Class is abstract;

   function "=" (A, B : XFormat.Object)
   return Boolean is abstract;

end Interface_Layer.Presentation.XFormat;
