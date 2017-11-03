-- local
with Active.Traveller;
with Interface_Layer.Utils.Unmarshaller;
with Interface_Layer.Utils.Explorer;
with Shared.Indefinite_String_Map;
with Interface_Layer.Utils.Types;
-- core
with Ada.Strings.Unbounded;

package Interface_Layer.Wrappers.InterfaceL is

   package Traveller renames Active.Traveller;
   package Types renames Interface_Layer.Utils.Types;
   package Explorer renames Interface_Layer.Utils.Explorer;
   package Unmarshaller renames Interface_Layer.Utils.Unmarshaller;
   package String_Map renames Shared.Indefinite_String_Map;
   package SU renames Ada.Strings.Unbounded;

   type Object is tagged private;
   type Reference is access all InterfaceL.Object'Class;

   function Create (Data : Unmarshaller.Reference)
      return InterfaceL.Object;

-- Get ACK as an Application Object
   function Get_Data (This : in InterfaceL.Object) return Boolean;
-- Get MESSAGE as an Application Object
   function Get_Data (This : in InterfaceL.Object) return SU.Unbounded_String;
-- Get TRAVELLER as an Application Object
   function Get_Data (This : in InterfaceL.Object) return Traveller.Reference;

   procedure Finalize (This : in out InterfaceL.Object);

   Explorer_Type_Error : exception;

private
   type Object is tagged
   record
      Data : Unmarshaller.Reference;
   end record;

   function Extract_Concrete_Data_Type (Reference : Unmarshaller.Reference)
   return Types.Data_Type;

end Interface_Layer.Wrappers.InterfaceL;
