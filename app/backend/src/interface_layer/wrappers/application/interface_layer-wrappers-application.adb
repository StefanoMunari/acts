with Active.Traveller.Extractor_Factory;
with Interface_Layer.Utils.Explorer_Factory;
-- core
with Ada.Unchecked_Deallocation;
with Ada.Containers;
with Ada.Tags;
-- library
with GNAT.String_Split;

package body Interface_Layer.Wrappers.Application is
    use Ada.Containers; -- view = operator

   package Extractor_Factory renames Active.Traveller.Extractor_Factory;
   package Explorer_Factory renames Interface_Layer.Utils.Explorer_Factory;
   package GSplit renames GNAT.String_Split;

   function Empty return Application.Reference is
     Instance : Application.Reference;
   begin
      Instance := new Application.Object;
      Instance.Data.Insert ("EMPTY", "EMPTY");
      return Instance;
   end Empty;

   function Create (Ack : Boolean) -- Wrapper for ACK replies
      return Application.Reference is
      Instance : Application.Reference;
   begin
      Instance := new Application.Object;
      Instance.Data.Insert ("Ack", Boolean'Image (Ack));
      return Instance;
   end Create;

   function Create (Message : SU.Unbounded_String)
      return Application.Reference is
      Instance : Application.Reference;
   begin
      Instance := new Application.Object;
      Instance.Data.Insert ("Message", SU.To_String (Message));
      return Instance;
   end Create;

   function Create (To_Extract : Active.Traveller.Reference)
      return Application.Reference
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Extractable.Object'Class, Extractable.Reference);
      procedure Free is new  Ada.Unchecked_Deallocation (
         Explorer.Object'Class, Explorer.Reference);
      Instance : Application.Reference;
      To_Extract_Type : Types.Data_Type :=
         Extract_Concrete_Data_Type (To_Extract);
      Extractor : Extractable.Reference :=
         Extractor_Factory.Get_Extractor (To_Extract_Type);
      Extracted : Explorer.Reference :=
         Explorer_Factory.Get_Explorer (To_Extract_Type);
   begin
      Instance := new Application.Object;
      -- Extract only the record od the Traveller (decouple a local object and
      --+ a distributed object)
      Extractor.Extract (To_Extract.all, Extracted.all);
      -- Convert the extracted Object to a String Map
      Extracted.Marshalling (Instance.Data);
      -- Set the corresponding Concrete Data Type
      Instance.Concrete_Type := To_Extract_Type;
      -- Free resources
      Free (Extractor);
      Free (Extracted);
      return Instance;
   end Create;

   function Get_Data (This : in Application.Object)
         return String_Map.Data.Map is
   begin
         return This.Data;
   end Get_Data;

   function Get_Concrete_Data_Type (This : in Application.Object)
   return Types.Data_Type is
   begin
      return This.Concrete_Type;
   end Get_Concrete_Data_Type;

   function "=" (A, B : Application.Object)
      return Boolean is
      A_Pair : String_Map.Data.Cursor;
      B_Pair : String_Map.Data.Cursor;
   begin
      if A.Data.Length = B.Data.Length then
        A_Pair := A.Data.First;
        B_Pair := B.Data.First;
        while String_Map.Data.Has_Element (A_Pair)
              and String_Map.Data.Has_Element (B_Pair)
        loop
            if (String_Map.Data.Key (A_Pair) /= String_Map.Data.Key (B_Pair)
                or
               String_Map.Data.Element (A_Pair)
                  /= String_Map.Data.Element (B_Pair))
            then
              -- mismatch => exit returning False
              return False;
            end if;
            String_Map.Data.Next (A_Pair);
            String_Map.Data.Next (B_Pair);
        end loop;
      else
        -- mismatch => exit returning False
        return False;
      end if;
      -- complete match (NOT mismatch) => exit returning True
      return True;
   end "=";

   procedure Finalize (Wrapper : in out Application.Object) is
   begin
    null;
   end Finalize;

   procedure Finalize (Wrapper_Ref : in out Application.Reference)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Application.Object'Class, Application.Reference);
   begin
      Free (Wrapper_Ref);
   end Finalize;

-- private
   function Extract_Concrete_Data_Type (Reference : Active.Traveller.Reference)
   return Types.Data_Type
   is
      Full_Type_Representation : String :=
         Ada.Tags.Expanded_Name (Reference.all'Tag);
      Separator : String := ".";
      Substrings : GSplit.Slice_Set;
      Number_Of_Substrings : GSplit.Slice_Number;
   begin
      GSplit.Create (S          => Substrings,
                     From       => Full_Type_Representation,
                     Separators => Separator,
                     Mode       => GSplit.Multiple);
      Number_Of_Substrings := GSplit.Slice_Count (Substrings);
         declare
            Extracted_Type_Representation : constant String :=
               GSplit.Slice (
                  Substrings,
                  GSplit.Slice_Number (Natural (Number_Of_Substrings) -1)
               );
         begin
         -- return the extracted type converted to enum
            return Types.Data_Type'Value (Extracted_Type_Representation);
         end;
   end Extract_Concrete_Data_Type;

end Interface_Layer.Wrappers.Application;
