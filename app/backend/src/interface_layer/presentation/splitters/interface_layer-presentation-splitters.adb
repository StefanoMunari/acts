-- local
with Interface_Layer.Containers.Pair;
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Unmarshaller.Utility;
with Interface_Layer.Utils.Unmarshaller;
with Interface_Layer.Wrappers.InterfaceL;
with Shared.Indefinite_String_Map;
-- core
with Ada.Unchecked_Deallocation;
-- DEBUG
-- DEBUG


-- The Elaborate pragma requires the body of the named unit be elaborated before
-- the unit in which the pragma occurs. Use this pragma if the current unit
-- calls at elaboration time, directly or indirectly, some subprogram in
-- the named unit.
pragma Elaborate (Shared.Indefinite_String_Map);
pragma Elaborate (Interface_Layer.Presentation.Envelope);
pragma Elaborate (Interface_Layer.Utils.Unmarshaller);
pragma Elaborate (Interface_Layer.Utils.Unmarshaller.Utility);
pragma Elaborate (Interface_Layer.Wrappers.InterfaceL);
pragma Elaborate (Interface_Layer.Containers.Pair);


package body Interface_Layer.Presentation.Splitters is
   use  Interface_Layer.Containers.Queues; -- view queues

   package String_Map         renames Shared.Indefinite_String_Map;
   package Envelope_Data      renames Interface_Layer.Presentation.Envelope;
   package Unmarshaller       renames Interface_Layer.Utils.Unmarshaller;
   package Unmarshaller_Utils
      renames Interface_Layer.Utils.Unmarshaller.Utility;
   package Interface_Wrapper  renames Interface_Layer.Wrappers.InterfaceL;
   package Pair               renames Interface_Layer.Containers.Pair;

   -- TODO: implement scoped_pointers
   procedure Free is new  Ada.Unchecked_Deallocation (Splitter, Reference);

   procedure Init is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Splitter is
      -- TODO: implement scoped_pointers
      procedure Free is new
         Ada.Unchecked_Deallocation (
            Envelope_Data.Object'Class, Envelope_Data.Reference);
   begin
      select
         accept Split;
            declare
               use PT; -- make '=' visible for Process_Types
               Envelope         : Envelope_Data.Reference;
               Unmarshaller_Ref : Unmarshaller.Reference;
               Header_Map       : String_Map.Data.Map;
               Payload_Map      : String_Map.Data.Map;
            begin
               while Splitter_State = PT.ACTIVE loop
               -- when ready execute, otherwise terminate
               -----
               -- Get the Envelope
               -----
                  Splitter_Envelope_Queue.Dequeue (Envelope);
               -- DEBUG
               -- DEBUG
                  Header_Map := Envelope.Get_Header;
                  Unmarshaller_Ref :=
                     Unmarshaller_Utils.Get_Unmarshaller (
                        Header_Map.Element ("Type"));
                  Payload_Map := Envelope.Get_Message;
                  Unmarshaller_Ref.Unmarshalling (Payload_Map);
                  if     (Header_Map.Element ("Request") = "QUERY" or
                          Header_Map.Element ("Request") = "ENTER")
                     and Header_Map.Element ("Type") = "ACK"
                  then
                     Activator_Ack_Queue.Enqueue (
                        Pair.Create (
                           Header_Map,
                           Interface_Wrapper.Create (Unmarshaller_Ref)));
                  else -- it could be also a Shutdown message
                     declare
                        -- Eventually represents a shutdown message
                        Action : String :=
                           Header_Map.Element ("Request");
                     begin
                        Activator_Request_Queue.Enqueue (
                           Pair.Create (
                              Header_Map,
                              Interface_Wrapper.Create (Unmarshaller_Ref)));
                        -- Eventually change Splitter state to STOPPED.
                        -- Thus, Splitter can complete its loop and
                        -- terminate gracefully
                        if Is_Shutdown (Action)
                        then
                           -- Stops also the Activators
                           Activator_Ack_Queue.Enqueue (
                              Pair.Create (
                                 Header_Map,
                                 Interface_Wrapper.Create (Unmarshaller_Ref)));
                           Splitter_State := PT.STOPPED;
                        end if;
                     end;
                  end if;
                  -- Free resources
                  Free (Envelope);
               end loop;
            -- otherwise terminate
            -- call Shutdown on the Splitter
               Splitters.Shutdown;
            end;
      or
        terminate;
      end select;
   end Splitter;

   -- begin
   -- -- free resources
   -- Free (Splitter_Ref);

end Interface_Layer.Presentation.Splitters;
