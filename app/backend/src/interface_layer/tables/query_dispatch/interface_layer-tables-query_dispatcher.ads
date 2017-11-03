with Interface_Layer.Remote.Query_Decoder;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive;
with Reactive.District;
with Reactive.Infrastructure.Building.Host.Utils;
with Reactive.Infrastructure.Stretch.Utils;

package Interface_Layer.Tables.Query_Dispatcher is

   package Query_Decoder renames Interface_Layer.Remote.Query_Decoder;
   package App_Wrapper   renames Interface_Layer.Wrappers.Application;
   package Building      renames Reactive.Infrastructure.Building;
   package District_Pkg  renames Reactive.District;
   package Stretch_Utils_Pkg
      renames Reactive.Infrastructure.Stretch.Utils;
   use Reactive.Infra_Id_Type;

   type Object is tagged private;
   type Reference is access all Query_Dispatcher.Object'Class;

   function Get_Instance (
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Stretch_Utils   : access Stretch_Utils_Pkg.Object'Class := null;
      Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null
   )
   return Query_Dispatcher.Reference;

   function Dispatch (This  :    Query_Dispatcher.Object;
                      Query : in String)
   return App_Wrapper.Reference;

private
   type Object is tagged record
      Stretch_Utils   : access Stretch_Utils_Pkg.Object'Class := null;
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null;
   end record;

   function Book_Parking (This    : Query_Dispatcher.Object;
                          Decoder : Query_Decoder.Reference)
   return App_Wrapper.Reference;

   function Seek (This    : Query_Dispatcher.Object;
                  Decoder : Query_Decoder.Reference)
   return App_Wrapper.Reference;

end Interface_Layer.Tables.Query_Dispatcher;
