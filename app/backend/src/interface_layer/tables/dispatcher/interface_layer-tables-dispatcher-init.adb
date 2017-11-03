separate (Interface_Layer.Tables.Dispatcher)

procedure Init is
begin
   District_Skel := Skeleton.Object (Skeleton.Create);
-- (ENTER, SNAPSHOT, QUERY, BOOT, SHUTDOWN, REPLY)
   PC.Callback.Insert(Container => Dispatch_Table,
                      Key       => Types.ENTER, -- TRAVELLER
                      New_Item  => Skeleton.Enter'Access);
   PC.Callback.Insert(Container => Dispatch_Table,
                      Key       => Types.SNAPSHOT, -- MESSAGE/ACK
                      New_Item  => Skeleton.Snapshot'Access);
   Dispatch_Table.Include(
      Key       => Types.QUERY, -- MESSAGE
      New_Item  => Skeleton.Query'Access);
   PC.Callback.Insert(Container => Dispatch_Table,
                      Key       => Types.BOOT, -- MESSAGE/ACK
                      New_Item  => Skeleton.Start'Access);
   PC.Callback.Insert(Container => Dispatch_Table,
                      Key       => Types.SHUTDOWN, -- MESSAGE/ACK
                      New_Item  => Skeleton.Shutdown'Access);
end Init;
