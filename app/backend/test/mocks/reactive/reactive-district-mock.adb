with Active.Agent;
with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.District.Mock is

   package Agent renames Active.Agent;

   function Create return District.Mock.Reference
   is (new District.Mock.Object);

   function Contains_Infrastructure (
      This : in District.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Mock_Values.Roadmap.Contains (Infrastructure_Id);
   end Contains_Infrastructure;

   function Find_Infrastructure_By_Id (
      This              : in District.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference is
   begin
      if not This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Infrastructure_By_Id",
            Function_Param => "Infrastructure_Id = "
            & Infra_Id'Image (Infrastructure_Id),
            Package_Name   => "Reactive.District.Mock");
      end if;

      return This.Mock_Values.Roadmap.Element (Infrastructure_Id);
   end Find_Infrastructure_By_Id;

   function Find_Intersection_By_Id (
      This            : in District.Mock.Object;
      Intersection_Id : in Infra_Id)
   return Intersection.Reference is
   begin
      return Intersection.Reference (This.Find_Infrastructure_By_Id (Intersection_Id));
   end Find_Intersection_By_Id;

   function Find_Street_By_Id (
      This                             : in District.Mock.Object;
      Street_Related_Infrastructure_Id : in Infra_Id)
   return Street.Reference is
   begin
      return Street.Reference (This.Find_Infrastructure_By_Id (Street_Related_Infrastructure_Id));
   end Find_Street_By_Id;

   function Find_Way_By_Id (
      This   : in District.Mock.Object;
      Way_Id : in Infra_Id)
   return Way.Reference is
   begin
      return Way.Reference (This.Find_Infrastructure_By_Id (Way_Id));
   end Find_Way_By_Id;

   function Find_Roadway_By_Id (
      This       : in District.Mock.Object;
      Roadway_Id : in Infra_Id)
   return Roadway.Reference is
   begin
      return Roadway.Reference (This.Find_Infrastructure_By_Id (Roadway_Id));
   end Find_Roadway_By_Id;

   function Find_Footway_By_Id (
      This : in District.Mock.Object;
      Footway_Id : in Infra_Id)
   return Footway.Reference is
   begin
      return Footway.Reference (This.Find_Infrastructure_By_Id (Footway_Id));
   end Find_Footway_By_Id;

   function Find_Bikeway_By_Id (
      This       : in District.Mock.Object;
      Bikeway_Id : in Infra_Id)
   return Bikeway.Reference is
   begin
      return Bikeway.Reference (This.Find_Infrastructure_By_Id (Bikeway_Id));
   end Find_Bikeway_By_Id;

   function Find_Lane_By_Id (
      This    : in District.Mock.Object;
      Lane_Id : in Infra_Id)
   return Lane.Reference is
   begin
      return Lane.Reference (This.Find_Infrastructure_By_Id (Lane_Id));
   end Find_Lane_By_Id;

   function Find_Stretch_By_Id (
      This       : in District.Mock.Object;
      Stretch_Id : in Infra_Id)
   return Stretch.Reference is
   begin
      return Stretch.Reference (This.Find_Infrastructure_By_Id (Stretch_Id));
   end Find_Stretch_By_Id;

   function Find_Traveller_By_Id (
      This         : in District.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference is
   begin
      if not This.Mock_Values.Population.Contains (Traveller_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Traveller_By_Id",
            Function_Param => "Traveller_Id = "
            --& Agent.Agent_Id'Image (Traveller_Id),
            ,
            Package_Name   => "Reactive.District.Mock");
      end if;

      return This.Mock_Values.Population.Element (Traveller_Id);
   end Find_Traveller_By_Id;

   function Find_Host_By_Id (This    : in District.Mock.Object;
                             Host_Id : in Infra_Id)
   return Host_Pkg.Reference is
   begin
      if not This.Mock_Values.Buildings.Contains (Host_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Find_Host_By_Id",
            Function_Param => "Host_Id = "
                               & Infra_Id'Image (Host_Id),
            Package_Name   => "Reactive.District.Mock");
      end if;

      return This.Mock_Values.Buildings.Element (Host_Id);
   end Find_Host_By_Id;

   procedure Add_Intersection (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Infrastructure'Unchecked_Access);
      Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
    end if;
   end Add_Intersection;

   procedure Add_Street (
      This      : in out District.Mock.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean)
   is
      Street_Id : Infra_Id := SR_Street.Get.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Street_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert (
            Key      => Street_Id,
            New_Item => Reactive.Infrastructure.Reference (SR_Street.Get_Reference));
         Added := This.Mock_Values.Roadmap.Contains (Street_Id);
      end if;
   end Add_Street;

   procedure Add_Roadway (
      This           : in out District.Mock.Object;
      Infrastructure : aliased
         in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Reactive.Infrastructure.Reference (Infrastructure));
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Roadway;

   procedure Add_Footway (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Reactive.Infrastructure.Reference (Infrastructure));
         Added := This.Mock_Values.Roadmap.Contains(Infrastructure_Id);
      end if;
   end Add_Footway;

   procedure Add_Bikeway (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Reactive.Infrastructure.Reference (Infrastructure));
         Added := This.Mock_Values.Roadmap.Contains (Infrastructure_Id);
      end if;
   end Add_Bikeway;

   procedure Add_Lane (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Reactive.Infrastructure.Reference (Infrastructure));
         Added := This.Mock_Values.Roadmap.Contains (Infrastructure_Id);
      end if;
   end Add_Lane;

   procedure Add_Stretch (
      This : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          : out Boolean)
   is
      Infrastructure_Id : Infra_Id := Infrastructure.Get_Id;
   begin
      if This.Mock_Values.Roadmap.Contains (Infrastructure_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Roadmap.Insert
           (Key      => Infrastructure_Id,
            New_Item => Reactive.Infrastructure.Reference (Infrastructure));
         Added := This.Mock_Values.Roadmap.Contains (Infrastructure_Id);
      end if;
   end Add_Stretch;

   procedure Add_Traveller (
      This      : in out District.Mock.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean)
   is
      Traveller_Id : Agent.Agent_Id := Traveller.Get_Id;
   begin
      If This.Mock_Values.Population.Contains (Traveller_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Population
           .Insert (Key      => Traveller_Id,
                    New_Item => Traveller);
         Added := This.Mock_Values.Population.Contains (Traveller_Id);
      end if;
   end Add_Traveller;

   procedure Add_Host (
      This     :         in out District.Mock.Object;
      Host_Ref : aliased in out Host_Pkg.Object'Class;
      Added    :            out Boolean)
   is
      Host_Id : Infra_Id := Host_Ref.Get_Id;
   begin
      If This.Mock_Values.Buildings.Contains (Host_Id) then
         Added := FALSE;
      else
         This.Mock_Values.Buildings
           .Insert (Key      => Host_Id,
                    New_Item => Host_Ref'Unchecked_Access);
         Added := This.Mock_Values.Buildings.Contains (Host_Id);
      end if;
   end Add_Host;

   procedure Remove_Traveller (
      This         : in out District.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Removed      : out Boolean) is
   begin
      if not This.Mock_Values.Population.Contains (Traveller_Id) then
         Removed := FALSE;
      else
         This.Mock_Values.Population.Delete (Key => Traveller_Id);
         Removed := not This.Mock_Values.Population.Contains (Traveller_Id);
      end if;
   end Remove_Traveller;

end Reactive.District.Mock;
