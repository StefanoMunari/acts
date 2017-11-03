package Interface_Layer.Service.Pipelines.Handler.Queue.Mock is

   type Object (<>) is new Handler.Queue.Object with private;
   type Reference is access all Queue.Mock.Object'Class;

   function Create
   return Queue.Mock.Reference;

   overriding
   procedure Handle (This : Handler.Queue.Mock.Object) is null;

   overriding
   procedure Handle (This : Handler.Queue.Mock.Object;
                     Env  : Envelope.Reference) is null;

private

   type Return_Values_Collection is record
      Handle     : Boolean;
      Handle_Set : Boolean := FALSE;
   end record;

   type Object is
     new Handler.Queue.Object
   with record
      Return_Values : Return_Values_Collection;
   end record;

end Interface_Layer.Service.Pipelines.Handler.Queue.Mock;
