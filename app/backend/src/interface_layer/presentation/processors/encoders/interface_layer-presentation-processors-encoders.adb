-- core
with Ada.Unchecked_Deallocation;

-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Presentation.JSON_Format;
with Interface_Layer.Presentation.XFormat;

with Shared.Indefinite_String_Map;

package body Interface_Layer.Presentation.Processors.Encoders is
-- packages
   package String_Map renames Shared.Indefinite_String_Map;
   package Envelope_Data renames Interface_Layer.Presentation.Envelope;
   package XFormat renames Interface_Layer.Presentation.XFormat;
   -- TODO: remove JSON_Format :
   -- only XFormat to use polymorphic Messages (not format dependent)
   package JSON_Format renames Interface_Layer.Presentation.JSON_Format;

-- specs
   -- TODO: implement scoped_pointers
   procedure Free is new
      Ada.Unchecked_Deallocation (
         JSON_Format.Object'Class, JSON_Format.Reference);
   procedure Free is new
      Ada.Unchecked_Deallocation (
         Envelope_Data.Object'Class, Envelope_Data.Reference);
   -- TODO: implement scoped_pointers
   procedure Free is new  Ada.Unchecked_Deallocation
      (Encoder, Encoder_Reference);

   procedure Init is separate;
   procedure Start is separate;
   procedure Stop is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Encoder is
      use PT; -- make '=' visible for Process_Types
      use Interface_Layer.Containers.Queues; -- view queues instances
   begin
      select
         accept Encode;
            declare
               Envelope        : Envelope_Data.Reference;
               Raw_Map         : String_Map.Data.Map;
               Message         : JSON_Format.Reference;
               Message_Header  : JSON_Format.Reference;
               Message_Payload : JSON_Format.Reference;
               Action_Header   : JSON_Format.Object;
            begin
               while Encoder_State = PT.ACTIVE loop
                  -- when ready execute, otherwise terminate
                     Message := new JSON_Format.Object;
                     Message_Header := new JSON_Format.Object;
                     Message_Payload := new JSON_Format.Object;
                  -----
                  -- Get Raw Data (if any exists) from the Envelope Queue
                  -----
                     Encoder_Envelope_Queue.Dequeue (Envelope);
                  -- DEBUG
                  -- DEBUG
                  -----
                  -- Convert Raw Data into a JSON formatted Message
                  -----
                  -- Convert Header
                     Raw_Map := Envelope.all.Get_Header;
                     Message_Header.all :=
                        JSON_Format.Object'Class (
                           JSON_Format.Object (
                              Format_Converter.all.Encode (Raw_Map)));
                     Message.all.Set_Header (
                        XFormat.Reference (Message_Header));
                  -- DEBUG
                  -- DEBUG
                  -- Convert Payload
                     Raw_Map := Envelope.all.Get_Message;
                     Message_Payload.all :=
                        JSON_Format.Object'Class (
                           JSON_Format.Object (
                              Format_Converter.all.Encode (Raw_Map)));
                     Message.all.Set_Payload (
                        XFormat.Reference (Message_Payload));
                  -- DEBUG
                  -- DEBUG
                  -- Get the Header as Object (necessary to check for shutdown)
                     Action_Header := JSON_Format.Object (Message.Get_Header);
                  -----
                  -- Put the formatted Message into the Sender's Queue
                  -----
                     Sender_Message_Queue.Enqueue (
                        XFormat.Reference (Message));
                  -- DEBUG
                  -- DEBUG
                  -- Eventually change the Decoder state to STOPPED.
                  -- Thus, Decoder can complete its loop and
                  -- terminate gracefully
                     if Is_Shutdown (Action_Header.Get ("Request"))
                     then
                        Encoder_State := PT.STOPPED;
                     end if;
                  -- Free resources
                     Free (Envelope);
                     Free (Message_Header);
                     Free (Message_Payload);
               end loop;
                  -- otherwise terminate
                  Encoders.Shutdown;
            end;
      or
        terminate;
      end select;
   end Encoder;

   -- begin
   -- -- Free resources
   -- Free (Encoder_Ref);
end Interface_Layer.Presentation.Processors.Encoders;