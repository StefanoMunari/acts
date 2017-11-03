pragma Ada_05;
-- core
with Ada.Containers.Hashed_Maps;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.InterfaceL;
with Interface_Layer.Remote.Skeleton;

package Interface_Layer.Utils.Procedure_Call is
   package Types             renames Interface_Layer.Utils.Types;
   package Interface_Wrapper renames Interface_Layer.Wrappers.InterfaceL;
   package Skeleton           renames Interface_Layer.Remote.Skeleton;

   type Procedure_T is access procedure (
      This      : in     Skeleton.Object;
      Parameter : in out Interface_Wrapper.Object);

   function ID_Hashed (Id: Types.Request_Type)
   return Ada.Containers.Hash_Type;

   package Callback is new Ada.Containers.Hashed_Maps (
      Key_Type        => Types.Request_Type,
      Element_Type    => Procedure_T,
      Hash            => ID_Hashed,
      Equivalent_Keys => Types."=");

end Interface_Layer.Utils.Procedure_Call;
