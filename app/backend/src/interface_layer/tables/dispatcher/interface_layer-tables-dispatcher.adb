-- local
with Interface_Layer.Utils.Types;

package body Interface_Layer.Tables.Dispatcher is

   package Types renames Interface_Layer.Utils.Types;

   procedure Init is separate;

   procedure Dispatch (This    :        Dispatcher.Object;
                       Method  : in     String;
                       Wrapper : in out Interface_Wrapper.Object)
   is
      Dispatch_Operation : PC.Procedure_T;
   begin
      Dispatch_Operation := Dispatch_Table.Element (
         Types.Request_Type'Value (Method)
      );
      Dispatch_Operation (District_Skel, Wrapper);
      -- Here above Wrapper is the argument of the procedure we extract from
      --+ the map!
   end Dispatch;

end Interface_Layer.Tables.Dispatcher;
