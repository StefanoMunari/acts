-- local
with Active.Traveller;

with Interface_Layer.Utils.Marshaller;
with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Types;

with Shared.Extractable;
with Shared.Indefinite_String_Map;
-- core
with Ada.Strings.Unbounded;

package Interface_Layer.Wrappers.Application is

   package Types renames Interface_Layer.Utils.Types;
   package SU renames Ada.Strings.Unbounded;
   package Explorer renames Interface_Layer.Utils.Explorer;
   package Extractable renames Shared.Extractable;
   package Marshaller renames Interface_Layer.Utils.Marshaller;
   package String_Map renames Shared.Indefinite_String_Map;

   type Object is tagged private;
   type Reference is access all Application.Object'Class;

   function Empty -- Empty Wrapper
     return Application.Reference;
   function Create (Ack : Boolean)-- Wrapper for ACK replies
      return Application.Reference;
   function Create (Message : SU.Unbounded_String)
      return Application.Reference;
   function Create (To_Extract : Active.Traveller.Reference)
      return Application.Reference;
   function Get_Data (This : in Application.Object)
      return String_Map.Data.Map;
   function Get_Concrete_Data_Type (This : in Application.Object)
      return Types.Data_Type;
   function "=" (A, B : Application.Object) return Boolean;
   procedure Finalize (Wrapper : in out Application.Object);
   procedure Finalize (Wrapper_Ref : in out Application.Reference);

private
   type Object is tagged
   record
      Data          : String_Map.Data.Map;
      Concrete_Type : Types.Data_Type;
   end record;

   function Extract_Concrete_Data_Type (Reference : in Active.Traveller.Reference) return Types.Data_Type;

end Interface_Layer.Wrappers.Application;