with Interface_Layer.Presentation.Tests;
with Interface_Layer.Service.Tests;
with Interface_Layer.Session.Tests;
with Interface_Layer.Session.Senders.Tests;
with Interface_Layer.Session.Receivers.Tests;


package body Interface_Layer_Suite is
   Result  : aliased TS.Test_Suite;
   Presentation_Test :
      aliased Interface_Layer.Presentation.Tests.Presentation_Test;
   Service_Test : aliased Interface_Layer.Service.Tests.Service_Test;
   Session_Test : aliased Interface_Layer.Session.Tests.Session_Test;
   Senders_Test : aliased Interface_Layer.Session.Senders.Tests.Senders_Test;
   Receivers_Test :
      aliased Interface_Layer.Session.Receivers.Tests.Receivers_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Presentation_Test'Access);
      TS.Add_Test (Result'Access, Service_Test'Access);
      TS.Add_Test (Result'Access, Session_Test'Access);
      TS.Add_Test (Result'Access, Senders_Test'Access);
      TS.Add_Test (Result'Access, Receivers_Test'Access);
   return Result'Access;
   end Suite;
end Interface_Layer_Suite;
