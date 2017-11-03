package body Interface_Layer.Utils.Procedure_Call is

   function ID_Hashed (Id: Types.Request_Type)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Val (Types.Request_Type'Pos (Id));
   end ID_Hashed;

end Interface_Layer.Utils.Procedure_Call;
