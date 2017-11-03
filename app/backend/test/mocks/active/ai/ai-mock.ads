package AI.Mock is

   type Object is new AI.Object with private;
   type Reference is access all AI.Mock.Object'Class;

   overriding
   procedure Set_Clients_Limit (
      This                : in AI.Mock.Object;
      New_Clients_Limit   :    Natural)
   is null;

   overriding
   procedure Init (This           : in out AI.Mock.Object;
      Clients_Limit  :        Natural;
      Data_Path      :        String;
      File_Prefix    :        String;
      File_Extension :        String;
      Agent_Id       :        String);

   overriding
   function Find_Path (
      This        : in AI.Mock.Object;
      Source      :    String;
      Destination :    String;
      Algorithm   :    Natural;
      S_Type      : in String;
      Agent_Id    : in String)
   return AI.Step_List.List;

   overriding
   procedure Finalize (This : in out AI.Mock.Object)
   is null;

   not overriding
   function Get_Init_Called (This : AI.Mock.Object) return Boolean;

   not overriding
   procedure Set_Return_Value_For_Find_Path (
      This         : in out AI.Mock.Object;
      Return_Value : in     AI.Step_List.List);

private
   type Mock_Values_Collection is record
      Init_Called : Boolean;
   end record;

   type Return_Values_Collection is record
      Find_Path : AI.Step_List.List;
      Find_Path_Existence : Boolean := False;
   end record;

   type Object is
     new AI.Object
   with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

end AI.Mock;
