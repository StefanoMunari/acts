package Interface_Layer.Service.Pipelines.Handler.Data.Traveller.Mock is

   type Object (<>) is new Handler.Data.Traveller.Object with private;
   type Reference is access all Data.Traveller.Mock.Object'Class;

   function Create (Next : Handler.Reference;
                    Req : Req_Wrapper.Object := Empty_Request)
   return Data.Traveller.Mock.Reference;

   overriding
   procedure Handle (This : Handler.Data.Traveller.Mock.Object) is null;

   overriding
   procedure Handle (This : Handler.Data.Traveller.Mock.Object;
                     Env  : Envelope.Reference) is null;

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
     new Handler.Data.Traveller.Object
   with record
      Mocked_Values : Mocked_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Interface_Layer.Service.Pipelines.Handler.Data.Traveller.Mock;
