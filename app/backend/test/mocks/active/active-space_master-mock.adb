package body Active.Space_Master.Mock is

   function Create return Space_Master.Mock.Reference
   is (new Space_Master.Mock.Object);

end Active.Space_Master.Mock;
