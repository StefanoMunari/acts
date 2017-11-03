with Active.Traveller.Extractor_Factory;
with Interface_Layer.Utils.Explorer_Factory;
-- core
with Ada.Unchecked_Deallocation;
with Ada.Containers;
with Ada.Tags;
-- library
with GNAT.String_Split;

package body Interface_Layer.Wrappers.Application.Mock is
    use Ada.Containers; -- view = operator

   package Extractor_Factory renames Active.Traveller.Extractor_Factory;
   package Explorer_Factory renames Interface_Layer.Utils.Explorer_Factory;
   package GSplit renames GNAT.String_Split;

   function Empty return Application.Mock.Reference is
     Instance : Application.Mock.Reference;
   begin
      Instance := new Application.Mock.Object;
      return Instance;
   end Empty;

   function Create (Ack : Boolean) -- Wrapper for ACK replies
      return Application.Mock.Reference is
      Instance : Application.Mock.Reference;
   begin
      Instance := new Application.Mock.Object;
      return Instance;
   end Create;

   function Create (Message : SU.Unbounded_String)
      return Application.Mock.Reference is
      Instance : Application.Mock.Reference;
   begin
      Instance := new Application.Mock.Object;
      return Instance;
   end Create;

   function Create (To_Extract : Active.Traveller.Reference)
      return Application.Mock.Reference is
      Instance : Application.Mock.Reference;
   begin
      Instance := new Application.Mock.Object;
      return Instance;
   end Create;

   function Get_Data (This : in Application.Mock.Object)
         return String_Map.Data.Map is
   begin
         return This.Data;
   end Get_Data;

   function Get_Concrete_Data_Type (This : in Application.Mock.Object)
   return Types.Data_Type is
   begin
     return This.Concrete_Type;
   end Get_Concrete_Data_Type;

-- TODO: Is implementation for "=" needed?
   --function "=" (A, B : Application.Mock.Object)
   --   return Boolean is
   --begin
   --   return True;
   --end "=";

   procedure Finalize (Wrapper : in out Application.Mock.Object) is
   begin
    null;
   end Finalize;

   procedure Finalize (Wrapper_Ref : in out Application.Mock.Reference) is
    procedure Free is new  Ada.Unchecked_Deallocation (Application.Mock.Object'Class, Application.Mock.Reference);
   begin
    Free (Wrapper_Ref);
   end Finalize;

end Interface_Layer.Wrappers.Application.Mock;
