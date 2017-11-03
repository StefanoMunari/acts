-- local
with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Types;

package Interface_Layer.Utils.Explorer_Factory is

   package Explorer renames Interface_Layer.Utils.Explorer;
   package Types    renames Interface_Layer.Utils.Types;

   function Get_Explorer (Traveller_Type : Types.Data_Type)
   return Explorer.Reference;

end Interface_Layer.Utils.Explorer_Factory;
