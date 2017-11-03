with Interface_Layer.Presentation.Converter;

with Shared.Process_Types;

package Interface_Layer.Presentation.Processors.Decoders is
   package PT renames Shared.Process_Types;
   package Base_Converter renames Interface_Layer.Presentation.Converter;

   procedure Init;
   procedure Start;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Decoder is
      entry Decode;
   end Decoder;
   type Decoder_Reference is access Decoder;

   -- static data fields
   Decoder_State : PT.Process_T := PT.TERMINATED;
   Decoder_Ref : Decoder_Reference := NULL;

end Interface_Layer.Presentation.Processors.Decoders;
