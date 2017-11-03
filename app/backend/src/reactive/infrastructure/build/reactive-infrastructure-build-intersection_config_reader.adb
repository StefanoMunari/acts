-- core
with Ada.Strings.Unbounded;

with Active.Agent;

with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

with Shared.Direction;
with Shared.Infra_Id_List;

package body Reactive.Infrastructure.Build.Intersection_Config_Reader is

   package SU            renames Ada.Strings.Unbounded;
   package Agent         renames Active.Agent;
   package Infra_Id_List renames Shared.Infra_Id_List;

   procedure Set_Builder (
      This    :    out Intersection_Config_Reader.Object;
      Builder : access Intersection_Builder_Pkg.Object'Class := null) is
   begin
      This.Builder := Builder;
   end Set_Builder;

   function Read (This              : in out Intersection_Config_Reader.Object;
                   Intersection_Json : in     G_JSON.JSON_Value)
   return Infra_Id
   is
      Exits               : G_JSON.JSON_Array;
      An_Exit             : G_JSON.JSON_Value;
      Street_Id_Int       : Integer;
      Street_Id           : Infra_Id;
      Triplets            : G_JSON.JSON_Array;
      Triplet             : G_JSON.JSON_Array;
      Stretch_Id_Int      : Integer;
      Stretches           : Infra_Id_List.List;
      Traffic_Light_Id_Int: Integer;
      Traffic_Light_Id    : Agent.Agent_Id;
      Direction_SU        : SU.Unbounded_String;
      Direction           : Shared.Direction.Cardinal;
   begin
      Exits := Intersection_Json.Get (Exits_Field);
      for I in 1 .. G_JSON.Length (Exits) loop
         An_Exit := G_JSON.Get (Exits, I);

         if not An_Exit.Has_Field (Direction_Field) then
            Raise_Missing_Field_For_Intersection (Direction_Field);
         end if;
         Direction_SU := An_Exit.Get (Direction_Field).Get;
         Direction :=
            Shared.Direction.Cardinal'Value (SU.To_String (Direction_SU));

         if not An_Exit.Has_Field (Street_Id_Field) then
            Raise_Missing_Field_For_Intersection (Street_Id_Field);
         end if;
         Street_Id_Int := An_Exit.Get (Street_Id_Field);
         Street_Id     := Infra_Id (Natural (Street_Id_Int));

         if not An_Exit.Has_Field (Stretch_Id_Field) then
            Raise_Missing_Field_For_Intersection (Stretch_Id_Field);
         end if;
         Triplets := An_Exit.Get (Stretch_Id_Field).Get;
         for J in 1 .. G_JSON.Length (Triplets) loop
            Triplet := G_JSON.Get (Triplets, J).Get;
            Stretch_Id_Int := G_JSON.Get (Triplet, 3).Get;
            Stretches.Append(Infra_Id (Natural (Stretch_Id_Int)));
         end loop;
         This.Builder.With_Street (Street_Id, Stretches, Direction);

         if not An_Exit.Has_Field (Traffic_Light_Field) then
            Raise_Missing_Field_For_Intersection (Traffic_Light_Field);
         end if;
         Traffic_Light_Id_Int := An_Exit.Get (Traffic_Light_Field);
         Traffic_Light_Id :=
            Agent.Create_Id_From_Natural (Traffic_Light_Id_Int);
         This.Builder.With_Traffic_Light (Traffic_Light_Id, Direction);
      end loop;

      return This.Builder.Get_Result;
   -- At this point, intersection is already registered in district by builder
   end Read;

end Reactive.Infrastructure.Build.Intersection_Config_Reader;
