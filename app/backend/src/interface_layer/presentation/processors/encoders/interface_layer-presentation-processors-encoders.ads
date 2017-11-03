with Interface_Layer.Presentation.Converter;

with Shared.Process_Types;

package Interface_Layer.Presentation.Processors.Encoders is
   package PT renames Shared.Process_Types;
   package Base_Converter renames Interface_Layer.Presentation.Converter;

   procedure Init;
   procedure Start;
   procedure Stop;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Encoder is
      entry Encode;
   end Encoder;
   type Encoder_Reference is access Encoder;

   -- static data fields
   Encoder_State : PT.Process_T := PT.TERMINATED;
   Encoder_Ref : Encoder_Reference := NULL;

end Interface_Layer.Presentation.Processors.Encoders;
