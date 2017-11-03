-- core
with Ada.Strings.Unbounded;

-- local
with Active.Traveller;

package Interface_Layer.Wrappers.Application.Abstract_Factory is

   package App_Wrapper renames Interface_Layer.Wrappers.Application;
   package SU renames Ada.Strings.Unbounded;

   type Object is interface;
   type Reference is access all Abstract_Factory.Object'Class;

   function Create_Wrapper (This : Abstract_Factory.Object)
   return App_Wrapper.Reference is abstract;

   function Create_Wrapper (This : Abstract_Factory.Object;
                            Ack  : Boolean)
   return App_Wrapper.Reference is abstract;

   function Create_Wrapper (This    : Abstract_Factory.Object;
                            Message : SU.Unbounded_String)
   return App_Wrapper.Reference is abstract;

   function Create_Wrapper (This       : Abstract_Factory.Object;
                            To_Extract : Active.Traveller.Reference)
   return App_Wrapper.Reference is abstract;

end Interface_Layer.Wrappers.Application.Abstract_Factory;
