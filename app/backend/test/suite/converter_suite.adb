with Converter.Tests;

package body Converter_Suite is
   Result:   aliased TS.Test_Suite;
   Converter0: aliased Converter.Tests.Converter_Test;
   
   function Suite 
      return TS.Access_Test_Suite is
   begin
	TS.Add_Test (Result'Access, Converter0'Access);
	return Result'Access;
   end Suite;
end Converter_Suite;
