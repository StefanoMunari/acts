with Ada.Strings.Unbounded;

with Reactive.Intersectable;
with Reactive.District;
with Reactive.Infrastructure;
with Reactive.Treadable;

with Shared.Infra_Id_Set;

package body Reactive.Infrastructure.Utils is

   package SU           renames Ada.Strings.Unbounded;
   package Infra_Id_Set renames Shared.Infra_Id_Set;

   use Reactive.Treadable;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Infrastructure.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Infrastructure.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end Get_Instance;

   function Exists (
      This              : in Infrastructure.Utils.Object;
      Infrastructure_Id : in Infra_Id) return Boolean is
   begin
      return This.District
        .Contains_Infrastructure (Infrastructure_Id => Infrastructure_Id);
   end Exists;

   procedure Tread (
      This         : in     Infrastructure.Utils.Object;
      Old_Position : in     Infra_Id;
      Treadable_Id : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean)
   is
      Old_Position_Ref : Treadable.Reference;
      Left             : Boolean := False;
      Moved            : Boolean := False;
   begin
      This.District.Try_To_Tread_Infrastructure (
            Treadable_Id => Treadable_Id,
            Traveller_Id => Traveller_Id,
            Advanced     => Advanced);

      if Advanced then
      -- Leave old treadable, since traveller moved to another one
         Old_Position_Ref :=
            This.District.Find_Treadable_By_Id (Old_Position);
         if Old_Position_Ref /= null then
            Old_Position_Ref.Leave (Traveller_Id, Left);
         end if;
      end if;
   end Tread;

end Reactive.Infrastructure.Utils;
