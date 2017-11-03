-- local
with Interface_Layer.Utils.Procedure_Call;
with Interface_Layer.Wrappers.InterfaceL;
with Interface_Layer.Remote.Skeleton;

package Interface_Layer.Tables.Dispatcher is

   package PC                renames Interface_Layer.Utils.Procedure_Call;
   package Interface_Wrapper renames Interface_Layer.Wrappers.InterfaceL;
   package Skeleton          renames Interface_Layer.Remote.Skeleton;

   type Object is tagged record
      null;
    end record;
   type Reference is access all Dispatcher.Object'Class;

   procedure Init;

   procedure Dispatch (This    :        Dispatcher.Object;
                       Method  : in     String;
                       Wrapper : in out Interface_Wrapper.Object);

private

   Dispatch_Table : PC.Callback.Map := PC.Callback.Empty_Map;
   District_Skel  : Skeleton.Object;

end Interface_Layer.Tables.Dispatcher;
