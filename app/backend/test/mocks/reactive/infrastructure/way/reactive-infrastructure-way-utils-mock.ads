package Reactive.Infrastructure.Way.Utils.Mock is

   type Object (<>) is new Way.Utils.Object with private;
   type Reference is access all Way.Utils.Mock.Object'Class;

   function Create return Way.Utils.Mock.Reference;

   overriding
   function Find_Street (This : in Way.Utils.Mock.Object;
                         Way_Id : in Infra_Id)
                         return Infra_Id;

   overriding
   function Is_Contained_By (This : in Way.Utils.Mock.Object;
                             Way_Id, Container_Id : in Infra_Id)
                             return Boolean;

   not overriding
   procedure Set_Return_Value_For_Find_Street
     (This : in out Way.Utils.Mock.Object;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By
     (This : in out Way.Utils.Mock.Object;
      Return_Value : in Boolean);

private
   type Return_Values_Collection is record
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
   end record;

   type Object is new Way.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Way.Utils.Mock;
