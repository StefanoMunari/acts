package Reactive.Infrastructure.Build.Street_Builder.Mock is

   type Object is new Street_Builder.Object with private;
   type Reference is access all Mock.Object'Class;

   function Create return Street_Builder.Mock.Reference;

-- TODO: Fix With_X implementation
   overriding
   function With_Bikeway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id;

   overriding
   function With_Footway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id;

   overriding
   function With_Roadway (This : in out Street_Builder.Mock.Object;
                           JSON : in     G_JSON.JSON_Value)
      return Infra_Id;

   overriding
   function Get_Street (This : in out Street_Builder.Mock.Object;
                        JSON : in     G_JSON.JSON_Value)
      return Infra_Id;

   not overriding
   procedure Set_Return_Value_For_With_Bikeway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_With_Footway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_With_Roadway (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Street (
      This         : in out Street_Builder.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   function Get_With_Bikeway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean;

   not overriding
   function Get_With_Footway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean;

   not overriding
   function Get_With_Roadway_Called (This : in out Street_Builder.Mock.Object)
   return Boolean;

private
   type Return_Values_Collection is record
      With_Bikeway : Infra_Id;
      With_Bikeway_Existence : Boolean := FALSE;
      With_Footway : Infra_Id;
      With_Footway_Existence : Boolean := FALSE;
      With_Roadway : Infra_Id;
      With_Roadway_Existence : Boolean := FALSE;
      Get_Street : Infra_Id;
      Get_Street_Existence : Boolean := FALSE;
   end record;

   type Mock_Values_Collection is record
      With_Bikeway_Called : Boolean := False;
      With_Footway_Called : Boolean := False;
      With_Roadway_Called : Boolean := False;
   end record;

   type Object is new Street_Builder.Object with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Build.Street_Builder.Mock;
