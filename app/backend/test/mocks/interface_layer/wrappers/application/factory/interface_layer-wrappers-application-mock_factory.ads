-- core
with Ada.Strings.Unbounded;

-- local
with Active.Traveller;

with Interface_Layer.Wrappers.Application.Abstract_Factory;

package Interface_Layer.Wrappers.Application.Mock_Factory is

   package App_Wrapper renames Interface_Layer.Wrappers.Application;
   package SU renames Ada.Strings.Unbounded;

   type Object is new Abstract_Factory.Object with null record;
   type Reference is access all Mock_Factory.Object'Class;

   function Create return Mock_Factory.Reference;

   function Create_Wrapper (This : Mock_Factory.Object)
   return App_Wrapper.Reference;

   function Create_Wrapper (This : Mock_Factory.Object;
                            Ack  : Boolean)
   return App_Wrapper.Reference;

   function Create_Wrapper (This    : Mock_Factory.Object;
                            Message : SU.Unbounded_String)
   return App_Wrapper.Reference;
   overriding
   function Create_Wrapper (This       : Mock_Factory.Object;
                            To_Extract : Active.Traveller.Reference)
   return App_Wrapper.Reference;

end Interface_Layer.Wrappers.Application.Mock_Factory;
