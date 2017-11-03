with Interface_Layer.Service;
with Interface_Layer.Presentation;
with Interface_Layer.Session;
with Interface_Layer.Remote.Stub;
-- core
with Ada.Unchecked_Deallocation;

package body Interface_Layer.System is

   procedure Shutdown
   is
      procedure Free is new
         Ada.Unchecked_Deallocation (
            Interface_Layer.Remote.Stub.Object'Class,
            Interface_Layer.Remote.Stub.Reference);
      Stub : Interface_Layer.Remote.Stub.Reference :=
         Interface_Layer.Remote.Stub.Create;
   begin
      Stub.Shutdown;
      Free (Stub);
   end Shutdown;

end Interface_Layer.System;
