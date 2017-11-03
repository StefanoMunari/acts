package Interface_Layer.System is
   -- Terminate the processing of messages
   -- from APP to IL (APP => IL terminated)
   -- in a sequential way (stop incrementally each sub layer)
   -- such that no message will be lost during termination.
   -- This is achieved by emptying all the queues before terminating
   -- the workers
   procedure Shutdown;

end Interface_Layer.System;
