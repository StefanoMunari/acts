with Active.Space_Master;

with Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
with Interface_Layer.Tables.Query_Dispatcher;
with Interface_Layer.Wrappers.InterfaceL;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive;

package Interface_Layer.Remote.Skeleton is

   package Space_Master_Pkg
      renames Active.Space_Master;
   package Interface_Wrapper
      renames Interface_Layer.Wrappers.InterfaceL;
   package Abstract_Handler_Factory
      renames Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
   package Abstract_Wrapper_Factory
      renames Interface_Layer.Wrappers.Application.Abstract_Factory;
   package Query_Dispatcher
      renames Interface_Layer.Tables.Query_Dispatcher;
   package App_Wrapper_Pkg renames Interface_Layer.Wrappers.Application;

   use Reactive.Infra_Id_Type;

   type Object is tagged private;
   type Reference is access all Skeleton.Object'Class;

   function Create (
      Handler_Factory : access Abstract_Handler_Factory.Object'Class := null;
      Wrapper_Factory : access Abstract_Wrapper_Factory.Object'Class := null;
      Dispatcher      : access Query_Dispatcher.Object'Class         := null;
      Space_Master    : access Space_Master_Pkg.Object'Class         := null)
   return Skeleton.Object;

--   procedure Finalize (This : in out Skeleton.Object);
--   procedure Finalize (This_Ref : in out Skeleton.Reference);


   procedure Enter (This   : in     Skeleton.Object;
                    Entity : in out Interface_Wrapper.Object);

   procedure Query (This    : in     Skeleton.Object;
                    Message : in out Interface_Wrapper.Object);

   procedure Snapshot (This    : in     Skeleton.Object;
                       Request : in out Interface_Wrapper.Object);

   procedure Start (This    : in     Skeleton.Object;
                   Request : in out Interface_Wrapper.Object);

   procedure Shutdown (This    : in     Skeleton.Object;
                       Request : in out Interface_Wrapper.Object);

private
  type Object is tagged
  record
    Handler_Factory : access Abstract_Handler_Factory.Object'Class;
    Wrapper_Factory : access Abstract_Wrapper_Factory.Object'Class;
    Dispatcher      : access Query_Dispatcher.Object'Class;
    Space_Master    : access Space_Master_Pkg.Object'Class;
  end record;

end Interface_Layer.Remote.Skeleton;
