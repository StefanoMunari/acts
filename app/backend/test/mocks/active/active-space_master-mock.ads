package Active.Space_Master.Mock is

   type Object is new Active.Space_Master.Object with null record;
   type Reference is access all Space_Master.Mock.Object'Class;

   function Create return Space_Master.Mock.Reference;

   overriding
   procedure Defer (This         : in Space_Master.Mock.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Retry_Action : in Boolean) is null;

end Active.Space_Master.Mock;
