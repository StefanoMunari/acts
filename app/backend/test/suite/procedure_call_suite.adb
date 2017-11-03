with Procedure_Call.Tests;

package body Procedure_Call_Suite is
   Result:   aliased TS.Test_Suite;
   Procedure_Call0: aliased Procedure_Call.Tests.Procedure_Call_Test;
   
   function Suite 
      return TS.Access_Test_Suite is
   begin
  TS.Add_Test (Result'Access, Procedure_Call0'Access);
  return Result'Access;
   end Suite;
end Procedure_Call_Suite;
