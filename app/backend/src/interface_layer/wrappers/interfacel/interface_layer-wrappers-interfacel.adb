-- core
with Ada.Tags;
with Ada.Unchecked_Deallocation;

--lib
with GNAT.String_Split;

with Active.Traveller.Pedestrian;
with Active.Traveller.Vehicle.Bicycle;
with Active.Traveller.Vehicle.Bus;
with Active.Traveller.Vehicle.Private_Motor_Vehicle;
with Active.Traveller.Extractor;
with Active.Traveller.Pedestrian.Extractor;
with Active.Traveller.Vehicle.Bicycle.Extractor;
with Active.Traveller.Vehicle.Bus.Extractor;
with Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor;

with Interface_Layer.Utils.Unmarshaller.Ack;
with Interface_Layer.Utils.Unmarshaller.Message;

with Shared.Extractable;

package body Interface_Layer.Wrappers.InterfaceL is

   package Extractable renames Shared.Extractable;
   package GSplit renames GNAT.String_Split;

   function Create (Data : Unmarshaller.Reference)
   return InterfaceL.Object is
      Instance : InterfaceL.Object;
   begin
      Instance.Data := Data;
      return Instance;
   end Create;

   -- Get ACK as an Application Object
   function Get_Data (This : in InterfaceL.Object)
   return Boolean is
      Result : Boolean;
   begin
      Result := Unmarshaller.Ack.Object (This.Data.all).Ack;
      return Result;
   end Get_Data;

   -- Get MESSAGE as an Application Object
   function Get_Data (This : in InterfaceL.Object)
   return SU.Unbounded_String
   is
      Result : SU.Unbounded_String;
   begin
      Result := Unmarshaller.Message.Object (This.Data.all).Message;
      return Result;
   end Get_Data;

   -- Get TRAVELLER as an Application Object
   function Get_Data (This : in InterfaceL.Object)
   return Traveller.Reference
   is
      Result        : Traveller.Reference;
      Extractor     : Traveller.Extractor.Reference;
      Explorer_Type : Types.Data_Type
         := Extract_Concrete_Data_Type (This.Data);
   -- TODO: implement scoped_pointers
      procedure Free is new  Ada.Unchecked_Deallocation (
         Traveller.Extractor.Object'Class, Traveller.Extractor.Reference);
   begin


      case Explorer_Type is
         when Types.PEDESTRIAN               =>
            Result := new Traveller.Pedestrian.Object;
            Extractor := new Traveller.Pedestrian.Extractor.Object;
         when Types.BICYCLE                  =>
            Result := new Traveller.Vehicle.Bicycle.Object;
            Extractor := new Traveller.Vehicle.Bicycle.Extractor.Object;
         when Types.BUS                      =>
            Result := new Traveller.Vehicle.Bus.Object;
            Extractor := new Traveller.Vehicle.Bus.Extractor.Object;
         when Types.PRIVATE_MOTOR_VEHICLE    =>
            Result := new Traveller.Vehicle.Private_Motor_Vehicle.Object;
            Extractor :=
               new Traveller.Vehicle.Private_Motor_Vehicle.Extractor.Object;
         when Types.ACK                      =>
            raise Explorer_Type_Error;
         when Types.MESSAGE                  =>
            raise Explorer_Type_Error;
      end case;

      Extractor.Extract (Explorer.Object (This.Data.all), Result.all);
      Free (Extractor);

      return Result;
   end Get_Data;

   procedure Finalize (This : in out InterfaceL.Object) is
   -- TODO: implement scoped_pointers
      procedure Free is new  Ada.Unchecked_Deallocation (
         Unmarshaller.Object'Class, Unmarshaller.Reference);
   begin
      Free (This.Data);
   end Finalize;

-- private
   function Extract_Concrete_Data_Type (Reference : Unmarshaller.Reference)
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
               GSplit.Slice_Number (Natural (Number_Of_Substrings) -1));
      begin
      -- return the extracted type converted to enum
         return Types.Data_Type'Value (Extracted_Type_Representation);
      end;
   end Extract_Concrete_Data_Type;

end Interface_Layer.Wrappers.InterfaceL;
