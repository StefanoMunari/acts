with Reactive.District;

package body Reactive.Infrastructure.Stretch.Utils is

   function Get_Instance
     (District : access Reactive.District.Object'Class := null)
      return Stretch.Utils.Reference is
   begin
      if Stretch.Utils.Instance = null then
         Stretch.Utils.Instance := new Stretch.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Stretch.Utils.Instance;
   end;

   procedure Tread (
      This         : in     Stretch.Utils.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is
   begin
      This.District
        .Find_Stretch_By_Id (Stretch_Id)
        .Tread (Traveller_Id => Traveller_Id,
                Advanced     => Advanced);
   end Tread;

   function Get_Id (This : in Stretch.Utils.Object;
                    Stretch_Id : in Infra_Id) return Infra_Id is
   begin
      return This.District
        .Find_Stretch_By_Id (Stretch_Id)
        .Get_Id;
   end Get_Id;

   not overriding
   function Find_Lane (This       : in Stretch.Utils.Object;
                       Stretch_Id : in Infra_Id)
   return Infra_Id
   is
   begin
      return This.District
        .Find_Stretch_By_Id (Stretch_Id)
        .Find_Lane;
   end Find_Lane;

   not overriding
   function Calculate_Position (This       : in Stretch.Utils.Object;
                                Stretch_Id : in Infra_Id)
   return Natural is
   begin
      return This.District
                 .Find_Stretch_By_Id (Stretch_Id)
                 .Calculate_Position;
   end Calculate_Position;

   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Utils.Object;
      Stretch_Id   : in Infra_Id;
      Traveller_Id : in Agent.Agent_Id) return Boolean is
   begin
      return This.District
        .Find_Stretch_By_Id (Stretch_Id)
        .Is_Waiting_To_Enter_Stretch (Traveller_Id => Traveller_Id);
   end Is_Waiting_To_Enter_Stretch;

   procedure Leave (
      This         : in     Stretch.Utils.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean) is
   begin
      This.District
        .Find_Stretch_By_Id (Stretch_Id)
        .Leave (Traveller_Id => Traveller_Id,
                Left         => Left);
   end Leave;

   function Is_Before (
      This  : in Stretch.Utils.Object;
      Left  : in Infra_Id;
      Right : in Infra_Id)
   return Boolean is
   begin
      return This.District
        .Find_Stretch_By_Id (Stretch_Id => Left)
        .Is_Before (Other =>
                      This.District
                    .Find_Stretch_By_Id (Stretch_Id => Right).all);
   end Is_Before;

   function Get_Host (This       : in Stretch.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Infra_Id is
   begin
      return This.District
                 .Find_Stretch_By_Id (Stretch_Id)
                 .Get_Host;
   end Get_Host;

   function Has_Host (This       : in Stretch.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Boolean is
   begin
      return This.District
                 .Find_Stretch_By_Id (Stretch_Id)
                 .Has_Host;
   end Has_Host;

   function Find_Intersections (This : in Stretch.Utils.Object;
                                Stretch_Id : in Infra_Id)
                                return Infra_Id_Set.Set is
   begin
      return This.District
        .Find_Stretch_By_Id (Stretch_Id => Stretch_Id)
        .Find_Intersections;
   end Find_Intersections;

   function Get_Size_Of (This       : in Stretch.Utils.Object;
                         Stretch_Id : in Infra_Id) return Natural is
   begin
      return This.District
                 .Find_Stretch_By_Id (Stretch_Id)
                 .Get_Size;
   end Get_Size_Of;

   function Attempt_Overtake (
      This                : in Stretch.Utils.Object;
      Stretch_Id          : in Infra_Id;
      Traveller_Id        : in Agent.Agent_Id)
   return Boolean is
   begin
      return This.District
               .Find_Stretch_By_Id (Stretch_Id)
               .Book (Traveller_Id);
   end Attempt_Overtake;

   procedure Unbook (
      This                : in Stretch.Utils.Object;
      Stretch_Id          : in Infra_Id;
      Traveller_Id        : in Agent.Agent_Id) is
   begin
      This.District
               .Find_Stretch_By_Id (Stretch_Id)
               .Unbook (Traveller_Id);
   end Unbook;

   function Dump (This : Stretch.Utils.Object; Stretch_Id : in Infra_Id)
   return G_JSON.JSON_Value is
   begin
      return This.District
                 .Find_Stretch_By_Id (Stretch_Id)
                 .Dump;
   end Dump;

end Reactive.Infrastructure.Stretch.Utils;
