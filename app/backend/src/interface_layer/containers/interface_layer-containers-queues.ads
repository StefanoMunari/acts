with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Presentation.XFormat;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Interface_Layer.Containers.Pair;
-- The queues are instantiated statically. These are interface_layer queues
package Interface_Layer.Containers.Queues is

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package XFormat renames Interface_Layer.Presentation.XFormat;
   package Pair renames Interface_Layer.Containers.Pair;

   package Envelope_Interface is
   new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Envelope.Reference);

   package Unbounded_Envelope_Package is
   new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Envelope_Interface);

   package Message_Interface is
   new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => XFormat.Reference);

   package Unbounded_Message_Package is
   new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Message_Interface);

   package Request_Interface is
   new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Pair.Object);

   package Unbounded_Request_Package is
   new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Request_Interface);

   Activator_Request_Queue : Unbounded_Request_Package.Queue;
   Activator_Ack_Queue : Unbounded_Request_Package.Queue;
   Encoder_Envelope_Queue : Unbounded_Envelope_Package.Queue;
   Splitter_Envelope_Queue : Unbounded_Envelope_Package.Queue;
   Sender_Message_Queue : Unbounded_Message_Package.Queue;
   Decoder_Message_Queue : Unbounded_Message_Package.Queue;
end Interface_Layer.Containers.Queues;