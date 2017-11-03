package Interface_Layer.Service.Pipelines.Handler.Boot.Mock is

   type Object (<>) is new Handler.Boot.Object with private;
   type Reference is access all Boot.Mock.Object'Class;

   function Create
   return Boot.Mock.Reference;

   overriding
   procedure Handle (This : Handler.Boot.Mock.Object) is null;

   overriding
   procedure Handle (This : Handler.Boot.Mock.Object;
                     Env  : Envelope.Reference) is null;

private

   type Return_Values_Collection is record
      Handle     : Boolean;
      Handle_Set : Boolean := FALSE;
   end record;

   type Object is
     new Handler.Boot.Object
   with record
      Return_Values : Return_Values_Collection;
   end record;

end Interface_Layer.Service.Pipelines.Handler.Boot.Mock;
