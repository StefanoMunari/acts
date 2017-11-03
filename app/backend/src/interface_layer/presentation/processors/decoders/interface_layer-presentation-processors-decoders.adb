-- core
with Ada.Unchecked_Deallocation;

-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Presentation.JSON_Format;
with Interface_Layer.Presentation.XFormat;

with Shared.Indefinite_String_Map;


package body Interface_Layer.Presentation.Processors.Decoders is
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
      (Decoder, Decoder_Reference);

   procedure Init is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Decoder is
      use PT; -- make '=' visible for Process_Types
      use Interface_Layer.Containers.Queues; -- view queues instances
   begin
      select
         accept Decode;
            declare
               Raw_Map         : String_Map.Data.Map;
               Message         : JSON_Format.Reference;
               Envelope        : Envelope_Data.Reference;
               Message_Header  : JSON_Format.Reference;
               Message_Payload : JSON_Format.Reference;
               Action_Header   : JSON_Format.Object;
            begin
               while Decoder_State = PT.ACTIVE loop
               -- when ready execute, otherwise terminate
                  Envelope        := new Envelope_Data.Object;
                  Message_Header  := new JSON_Format.Object;
                  Message_Payload := new JSON_Format.Object;
               -----
               -- Get Formatted Data (if any exists) from the Message Queue
               -----
                  Decoder_Message_Queue.Dequeue (XFormat.Reference (Message));
               -- DEBUG
               -- DEBUG
               -- Get the Header as Object (necessary to check for shutdown)
                  Action_Header := JSON_Format.Object (Message.Get_Header);
               -----
               -- Convert a JSON formatted Message into a Raw String Map
               --+ (easier to process in Ada)
               -----
               -- Decode Header
                  Message_Header.all :=
                     JSON_Format.Object'Class (Message.Get_Header);
                  Raw_Map := Format_Converter.Decode (Message_Header.all);
                  Envelope.Set_Header (Raw_Map);
               -- DEBUG
               -- DEBUG
               -- Decode Payload
                  Message_Payload.all :=
                     JSON_Format.Object'Class (Message.Get_Payload);
                  Raw_Map := Format_Converter.Decode (Message_Payload.all);
                  Envelope.Set_Message (Raw_Map);
               -- DEBUG
               -- DEBUG
               -----
               -- Put the Envelope into the Unmarshaller's Queue
               -----
                  Splitter_Envelope_Queue.Enqueue (Envelope);
               -- DEBUG
               -- Eventually change the Decoder state to STOPPED.
               -- Thus, Decoder can complete its loop and
               -- terminate gracefully
                  if Is_Shutdown (Action_Header.Get ("Request"))
                  then
                     Decoder_State := PT.STOPPED;
                  end if;
               -- DEBUG
               -- Free resources
                  Free (Message);
                  Free (Message_Header);
                  Free (Message_Payload);
               end loop;
               -- otherwise terminate
               Decoders.Shutdown;
            end;
      or
        terminate;
      end select;
   end Decoder;

   -- begin
   -- -- Free resources
   -- Free (Decoder_Ref);
end Interface_Layer.Presentation.Processors.Decoders;