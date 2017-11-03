with Interface_Layer.Utils.Types;

with Shared.Extractable;

package Active.Traveller.Extractor_Factory is

   Extractor_Factory_Illegal_Argument_Exception : exception;

   package Types renames Interface_Layer.Utils.Types;

   function Get_Extractor (Traveller_Type : Types.Data_Type)
   return Shared.Extractable.Reference;

end Active.Traveller.Extractor_Factory;
