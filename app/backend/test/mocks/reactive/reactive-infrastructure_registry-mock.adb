with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure_Registry.Mock is

   function Create return Infrastructure_Registry.Mock.Reference
   is (new Infrastructure_Registry.Mock.Object);

   function Contains_Infrastructure (
      This              : in Infrastructure_Registry.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Mock_Values.Roadmap.Contains (Infrastructure_Id)
             or
             This.Return_Values.Find_Treadable.Contains (Infrastructure_Id);
   end Contains_Infrastructure;

   function Contains_Treadable (
      This         : in Infrastructure_Registry.Mock.Object;
      Treadable_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Contains_Infrastructure (Treadable_Id);
   end Contains_Treadable;

   function Find_Infrastructure_By_Id (
      This              : in Infrastructure_Registry.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference is
   begin
      if not This.Return_Values.Find.Contains (Infrastructure_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Infrastructure_By_Id",
            Function_Param => "Infrastructure_Id = "
            & Infra_Id'Image (Infrastructure_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return This.Return_Values.Find.Element (Infrastructure_Id);
   end Find_Infrastructure_By_Id;

   function Find_Treadable_By_Id (
      This         : in Infrastructure_Registry.Mock.Object;
      Treadable_Id : in Infra_Id)
   return Treadable.Reference is
   begin
      if not This.Return_Values.Find_Treadable.Contains (Treadable_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "Find_Treadable_By_Id",
            Function_Param => "Treadable_Id = "
            & Infra_Id'Image (Treadable_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return This.Return_Values.Find_Treadable.Element (Treadable_Id);
   end Find_Treadable_By_Id;

   function Find_Intersection_By_Id (
      This            : in Infrastructure_Registry.Mock.Object;
      Intersection_Id : in Infra_Id)
   return Intersection.Reference is
   begin
      if not This.Return_Values.Find.Contains (Intersection_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Intersection_By_Id",
            Function_Param => "Intersection_Id = "
            & Infra_Id'Image (Intersection_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Intersection.Reference (
         This.Return_Values.Find.Element (Intersection_Id));
   end Find_Intersection_By_Id;

   function Find_Street_By_Id (
      This      : in Infrastructure_Registry.Mock.Object;
      Street_Id : in Infra_Id)
   return Street.Reference is
   begin
      if not This.Return_Values.Find.Contains (Street_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Street_By_Id",
            Function_Param => "Street_Id = "
            & Infra_Id'Image (Street_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Street.Reference (
         This.Return_Values.Find.Element (Street_Id));
   end Find_Street_By_Id;

   function Find_Way_By_Id (
      This   : in Infrastructure_Registry.Mock.Object;
      Way_Id : in Infra_Id)
   return Way.Reference is
   begin
      if not This.Return_Values.Find.Contains (Way_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Way_By_Id",
            Function_Param => "Way_Id = "
            & Infra_Id'Image (Way_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Way.Reference (
         This.Return_Values.Find.Element (Way_Id));
   end Find_Way_By_Id;

   function Find_Roadway_By_Id (
      This       : in Infrastructure_Registry.Mock.Object;
      Roadway_Id : in Infra_Id)
   return Roadway.Reference is
   begin
      if not This.Return_Values.Find.Contains (Roadway_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Roadway_By_Id",
            Function_Param => "Roadway_Id = "
            & Infra_Id'Image (Roadway_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Roadway.Reference (
         This.Return_Values.Find.Element (Roadway_Id));
   end Find_Roadway_By_Id;

   function Find_Footway_By_Id (
      This       : in Infrastructure_Registry.Mock.Object;
      Footway_Id : in Infra_Id)
   return Footway.Reference is
   begin
      if not This.Return_Values.Find.Contains (Footway_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Footway_By_Id",
            Function_Param => "Footway_Id = "
            & Infra_Id'Image (Footway_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Footway.Reference (
         This.Return_Values.Find.Element (Footway_Id));
   end Find_Footway_By_Id;

   function Find_Bikeway_By_Id (
      This       : in Infrastructure_Registry.Mock.Object;
      Bikeway_Id : in Infra_Id)
   return Bikeway.Reference is
   begin
      if not This.Return_Values.Find.Contains (Bikeway_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Bikeway_By_Id",
            Function_Param => "Bikeway_Id = "
            & Infra_Id'Image (Bikeway_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Bikeway.Reference (
         This.Return_Values.Find.Element (Bikeway_Id));
   end Find_Bikeway_By_Id;

   function Find_Lane_By_Id (
      This    : in Infrastructure_Registry.Mock.Object;
      Lane_Id : in Infra_Id)
   return Lane.Reference is
   begin
      if not This.Return_Values.Find.Contains (Lane_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Lane_By_Id",
            Function_Param => "Lane_Id = "
            & Infra_Id'Image (Lane_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Lane.Reference (
         This.Return_Values.Find.Element (Lane_Id));
   end Find_Lane_By_Id;

   function Find_Stretch_By_Id (
      This       : in Infrastructure_Registry.Mock.Object;
      Stretch_Id : in Infra_Id)
   return Stretch.Reference is
   begin
      if not This.Return_Values.Find.Contains (Stretch_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Stretch_By_Id",
            Function_Param => "Stretch_Id = "
            & Infra_Id'Image (Stretch_Id),
            Package_Name   => "Reactive.Infrastructure_Registry.Mock");
      end if;

      return Stretch.Reference (
         This.Return_Values.Find.Element (Stretch_Id));
   end Find_Stretch_By_Id;

   procedure Add_Intersection (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
     if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
        Added := FALSE;
     else
        This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
        Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
     end if;
   end Add_Intersection;

   procedure Add_Street (
      This      : in out Infrastructure_Registry.Mock.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     : out Boolean)
   is
      Street_Id : Infra_Id := SR_Street.Get.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Street_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Street_Id);
         Added := This.Mock_Values.Roadmap.Contains(Street_Id);
      end if;
   end Add_Street;

   procedure Add_Roadway (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Roadway;

   procedure Add_Footway (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Footway;

   procedure Add_Bikeway (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Bikeway;

   procedure Add_Lane (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Lane;

   procedure Add_Stretch (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (Infrastructure_Id);
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Stretch;

   not overriding
   procedure Set_Return_Value_For_Contains (
      This         : in out Infrastructure_Registry.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Mock_Values.Roadmap.Insert (Return_Value);
   end Set_Return_Value_For_Contains;

   procedure Set_Return_Value_For_Find (
      This         : in out Infrastructure_Registry.Mock.Object;
      Key   : in     Infra_Id;
      Value : in     Infrastructure.Reference) is
   begin
      This.Return_Values.Find.Insert (
         Key      => Key,
         New_Item => Value);
   end Set_Return_Value_For_Find;

   procedure Set_Return_Value_For_Find_Treadable (
      This         : in out Infrastructure_Registry.Mock.Object;
      Key   : in     Infra_Id;
      Value : in     Treadable.Reference) is
   begin
      This.Return_Values.Find_Treadable.Insert (
         Key      => Key,
         New_Item => Value);
   end Set_Return_Value_For_Find_Treadable;

end Reactive.Infrastructure_Registry.Mock;
