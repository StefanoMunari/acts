with Interface_Layer.Service.Pipelines.Handler;

package Interface_Layer.Service.Pipelines.Handler.Call.Mock is

   type Object (<>) is new Handler.Call.Object with private;
   type Reference is access all Call.Mock.Object'Class;

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Call.Mock.Reference;

   overriding
   procedure Handle (This : Handler.Call.Mock.Object);

   overriding
   procedure Handle (This : Handler.Call.Mock.Object;
                     Env  : Envelope.Reference);

   not overriding
   procedure Set_Answer_For_Rendezvous (This   : in out Call.Mock.Object;
                                        Answer : in Boolean);

private

   type Mocked_Values_Collection is record
      Next           : Handler.Reference;
      Next_Existence : Boolean := False;
      Req            : Req_Wrapper.Object;
      Req_Existence  : Boolean := False;
   end record;

   type Return_Values_Collection is record
      Handle     : Boolean;
      Handle_Set : Boolean := FALSE;
   end record;

   type Object is
     new Handler.Call.Object
   with record
      Mocked_Values : Mocked_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Interface_Layer.Service.Pipelines.Handler.Call.Mock;
