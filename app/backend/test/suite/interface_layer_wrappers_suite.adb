with Interface_Layer.Wrappers.Application.Tests;
-- with Interface_Layer.Wrappers.InterfaceL.Tests;
-- with Interface_Layer.Wrappers.Request.Tests;

package body Interface_Layer_Wrappers_Suite is
   Result  : aliased TS.Test_Suite;
   Application_Test :
      aliased Interface_Layer.Wrappers.Application.Tests.Application_Test;
   -- InterfaceL_Test : aliased Interface_Layer.Wrappers.InterfaceL.Tests.InterfaceL_Test;
   -- Request_Test : aliased Interface_Layer.Wrappers.Request.Tests.Request_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Application_Test'Access);
      -- TS.Add_Test (Result'Access, InterfaceL_Test'Access);
      -- TS.Add_Test (Result'Access, Request_Test'Access);
   return Result'Access;
   end Suite;

end Interface_Layer_Wrappers_Suite;