package Active.Travel.Travel_Completed.Mock is

   type Object (<>)
   is limited new Travel_Completed.Object with private;
   type Reference is access all Travel_Completed.Mock.Object'Class;

   function Create return Travel_Completed.Mock.Reference;

   overriding
   procedure Plan (This   : in out Travel_Completed.Mock.Object;
                   Travel : in out Active.Travel.Object'Class) is null;

   overriding
   procedure Advance (This   : in out Travel_Completed.Mock.Object;
                      Travel : in out Active.Travel.Object'Class) is null;

   overriding
   function Has_Next_Step (This   : in Travel_Completed.Mock.Object;
                           Travel : in Active.Travel.Object'Class)
                           return Boolean;

   overriding
   function Is_Progressing (This   : in Travel_Completed.Mock.Object;
                            Travel : in Active.Travel.Object'Class)
                            return Boolean;

   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Travel_Completed.Mock.Object;
      Return_Value : in Boolean);

   procedure Set_Return_Value_For_Is_Progressing (
      This         : in out Travel_Completed.Mock.Object;
      Return_Value : in Boolean);

private
   type Return_Values_Collection is record
      Has_Next_Step : Boolean;
      Has_Next_Step_Existence : Boolean := FALSE;
      Is_Progressing : Boolean;
      Is_Progressing_Existence : Boolean := FALSE;
   end record;

   type Object is limited new Travel_Completed.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Travel.Travel_Completed.Mock;
