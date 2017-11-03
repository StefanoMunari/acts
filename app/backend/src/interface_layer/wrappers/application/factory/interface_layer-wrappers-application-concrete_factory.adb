-- core
with Ada.Strings.Unbounded;

-- local
with Active.Traveller;

with Interface_Layer.Wrappers.Application;

package body Interface_Layer.Wrappers.Application.Concrete_Factory is

   --package App_Wrapper renames Interface_Layer.Wrappers.Application;

   function Create_Wrapper (This : Concrete_Factory.Object)
   return App_Wrapper.Reference is (App_Wrapper.Empty);

   function Create_Wrapper (This : Concrete_Factory.Object;
                            Ack  : Boolean)
   return App_Wrapper.Reference is (App_Wrapper.Create (Ack));

   function Create_Wrapper (This    : Concrete_Factory.Object;
                            Message : SU.Unbounded_String)
   return App_Wrapper.Reference is (App_Wrapper.Create (Message));

   function Create_Wrapper (This       : Concrete_Factory.Object;
                            To_Extract : Active.Traveller.Reference)
   return App_Wrapper.Reference is (App_Wrapper.Create (To_Extract));

end Interface_Layer.Wrappers.Application.Concrete_Factory;
