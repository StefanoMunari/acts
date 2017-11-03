-- core
with Ada.Unchecked_Deallocation;

-- local
with Interface_Layer.Presentation.Processors.Decoders;
with Interface_Layer.Presentation.Processors.Encoders;

package body Interface_Layer.Presentation.Processors is

   package Decoders_Pkg renames
      Interface_Layer.Presentation.Processors.Decoders;
   package Encoders_Pkg renames
      Interface_Layer.Presentation.Processors.Encoders;

   procedure Init (Format_Reference : Base_Converter.Reference) is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

end Interface_Layer.Presentation.Processors;