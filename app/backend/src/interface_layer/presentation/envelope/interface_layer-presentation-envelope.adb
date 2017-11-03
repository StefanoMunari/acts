-- core
with Ada.Containers;

package body Interface_Layer.Presentation.Envelope is
  use Ada.Containers; --make "=" operator visible

   function Create
   return Envelope.Reference
   is
      Envelope_Ref : Envelope.Reference := new Envelope.Object;
   begin
      return Envelope_Ref;
   end Create;

   function Get_Header (This : in Envelope.Object)
   return String_Map.Data.Map
   is
   begin
      return This.Header;
   end Get_Header;

   procedure Set_Header (
      This       : in out Envelope.Object;
      Request_Id : in     String)
   is
   begin
      This.Header.Insert ("Request_Id", Request_Id);
   end Set_Header;

   procedure Set_Header (
      This    : in out Envelope.Object;
      Request :        Types.Request_Type)
   is
   begin
      This.Header.Insert ("Request", Types.Request_Type'Image (Request));
   end Set_Header;

   procedure Set_Header (
      This    : in out Envelope.Object;
      Content :        Types.Data_Type)
   is
   begin
      This.Header.Insert ("Type", Types.Data_Type'Image (Content));
   end Set_Header;

   procedure Set_Header (
      This : in out Envelope.Object;
      Call :        Types.Call_Type)
   is
   begin
      This.Header.Insert ("Call", Types.Call_Type'Image (Call));
   end Set_Header;

   procedure Set_Header (
      This      : in out Envelope.Object;
      Recipient :        Recipient_Type)
   is
   begin
      This.Header.Insert (
         "Recipient", Infra_Id'Image (Recipient.Id));
      This.Header.Insert (
         "Recipient_Type", Types.Recipient_Sort'Image (Recipient.Sort));
   end Set_Header;

   procedure Set_Header (
      This    : in out Envelope.Object;
      Raw_Map :        String_Map.Data.Map)
   is
   begin
      This.Header := Raw_Map;
   end Set_Header;

   function Get_Message (This : in Envelope.Object)
   return String_Map.Data.Map
   is
   begin
      return This.Message;
   end Get_Message;

   procedure Set_Message (
      This    : in out Envelope.Object;
      Raw_Map :        String_Map.Data.Map)
   is
   begin
      This.Message := Raw_Map;
   end Set_Message;

   function "=" (A, B : Envelope.Object)
   return Boolean
   is
      use String_Map; --make "=" operator visible
      A_Pair : String_Map.Data.Cursor;
      B_Pair : String_Map.Data.Cursor;
   begin
      -- Check if Messages are equal
      if A.Message.Length = B.Message.Length then
         A_Pair := A.Message.First;
         B_Pair := B.Message.First;
         while  String_Map.Data.Has_Element (A_Pair)
            and String_Map.Data.Has_Element (B_Pair)
         loop
            if (String_Map.Data.Key (A_Pair) /= String_Map.Data.Key (B_Pair)
               or
               String_Map.Data.Element (A_Pair)
                  /= String_Map.Data.Element (B_Pair))
            then
            -- mismatch => False
               return False;
            end if;
            String_Map.Data.Next (A_Pair);
            String_Map.Data.Next (B_Pair);
        end loop;
      else
      -- mismatch => False
         return False;
      end if;
   -- Check if Headers are equal
      if A.Header.Length = B.Header.Length then
         A_Pair := A.Header.First;
         B_Pair := B.Header.First;
         while  String_Map.Data.Has_Element (A_Pair)
            and String_Map.Data.Has_Element (B_Pair)
        loop
            if (String_Map.Data.Key (A_Pair) /= String_Map.Data.Key (B_Pair)
               or
               String_Map.Data.Element (A_Pair)
                  /= String_Map.Data.Element (B_Pair))
            then
            -- mismatch => False
               return False;
            end if;
            String_Map.Data.Next (A_Pair);
            String_Map.Data.Next (B_Pair);
         end loop;
      else
      -- mismatch => False
         return False;
      end if;
   -- if A.Message = B.Message then there is a complete match => True
      return True;
   end "=";

   -- DEBUG
   procedure Debug (This : in Envelope.Object) is
   begin
      null;
   end Debug;

end Interface_Layer.Presentation.Envelope;
