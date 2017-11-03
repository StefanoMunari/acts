with Interface_Layer.Wrappers.Application;

with Shared.Shared_References;

package Shared.Shared_References_App_Wrapper is
new Shared.Shared_References (
   T => Interface_Layer.Wrappers.Application.Object'Class);
