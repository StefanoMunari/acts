-- local
with Active.Traveller;

with Interface_Layer.Utils.Marshaller;
with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Types;

with Shared.Extractable;
with Shared.Indefinite_String_Map;
-- core
with Ada.Strings.Unbounded;

package Interface_Layer.Wrappers.Application.Mock is

   package Types renames Interface_Layer.Utils.Types;
   package SU renames Ada.Strings.Unbounded;
   package Explorer renames Interface_Layer.Utils.Explorer;
   package Extractable renames Shared.Extractable;
   package Marshaller renames Interface_Layer.Utils.Marshaller;
   package String_Map renames Shared.Indefinite_String_Map;

   type Object is new Wrappers.Application.Object with private;
   type Reference is access all Application.Mock.Object'Class;

   function Empty -- Empty Wrapper
     return Application.Mock.Reference;
   function Create (Ack : Boolean)-- Wrapper for ACK replies
      return Application.Mock.Reference;
   function Create (Message : SU.Unbounded_String)
      return Application.Mock.Reference;
   function Create (To_Extract : Active.Traveller.Reference)
      return Application.Mock.Reference;
   function Get_Data (This : in Application.Mock.Object)
      return String_Map.Data.Map;
   function Get_Concrete_Data_Type (This : in Application.Mock.Object)
      return Types.Data_Type;
   --function "=" (A, B : Application.Mock.Object) return Boolean;
   procedure Finalize (Wrapper : in out Application.Mock.Object);
   procedure Finalize (Wrapper_Ref : in out Application.Mock.Reference);

private
   type Object is new Wrappers.Application.Object
      with record
      A : Integer;
   end record;

end Interface_Layer.Wrappers.Application.Mock;