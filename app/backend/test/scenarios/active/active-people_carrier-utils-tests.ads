with AUnit;
with AUnit.Test_Cases;

with Active.Agent;

with Reactive.District;

package Active.People_Carrier.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;
   package People_Carrier_Utils renames Active.People_Carrier.Utils;

   type People_Carrier_Utils_Test is new TC.Test_Case with record
      People_Carrier_Utils_Ref : People_Carrier_Utils.Reference;
      District_Ref             : Reactive.District.Reference;
   end record;

   overriding
   procedure Set_Up (T : in out People_Carrier_Utils_Test);

   -- Test Routines:
   procedure Test_Pedestrian_Is_Not_A_People_Carrier (T : in out People_Carrier_Utils_Test);
   procedure Test_Vehicle_Is_A_People_Carrier (T : in out People_Carrier_Utils_Test);
   procedure Test_Get_Passengers (T : in out People_Carrier_Utils_Test);
   procedure Test_Board (T : in out People_Carrier_Utils_Test);
   procedure Test_Free (T : in out People_Carrier_Utils_Test);

   procedure Register_Tests (T : in out People_Carrier_Utils_Test);
   overriding
   function Name (T : in People_Carrier_Utils_Test) return AU.Message_String;

end Active.People_Carrier.Utils.Tests;
