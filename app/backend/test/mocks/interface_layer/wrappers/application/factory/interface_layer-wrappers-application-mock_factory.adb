-- core
with Ada.Strings.Unbounded;

-- local
with Active.Traveller;

with Interface_Layer.Wrappers.Application.Mock;

package body Interface_Layer.Wrappers.Application.Mock_Factory is

   function Create return Mock_Factory.Reference
   is (new Mock_Factory.Object);

   function Create_Wrapper (This : Mock_Factory.Object)
   return App_Wrapper.Reference is
      (App_Wrapper.Reference (
         App_Wrapper.Mock.Reference (App_Wrapper.Mock.Empty)));

   function Create_Wrapper (This : Mock_Factory.Object;
                            Ack  : Boolean)
   return App_Wrapper.Reference is
      (App_Wrapper.Reference (
         App_Wrapper.Mock.Reference (App_Wrapper.Mock.Create (Ack))));

   function Create_Wrapper (This    : Mock_Factory.Object;
                            Message : SU.Unbounded_String)
   return App_Wrapper.Reference is
      (App_Wrapper.Reference (
         App_Wrapper.Mock.Reference (App_Wrapper.Mock.Create (Message))));

   function Create_Wrapper (This       : Mock_Factory.Object;
                            To_Extract : Active.Traveller.Reference)
   return App_Wrapper.Reference is
   begin
      return (App_Wrapper.Reference (App_Wrapper.Mock.Create (To_Extract)));
   end;

end Interface_Layer.Wrappers.Application.Mock_Factory;
