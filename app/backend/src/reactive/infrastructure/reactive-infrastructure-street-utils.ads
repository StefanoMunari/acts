with Active.Agent;

limited with Reactive.District;

with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Street.Utils is

   package Agent renames Active.Agent;
   package Infra_Id_Set renames Shared.Infra_Id_Set;

   type Object (<>) is tagged limited private;
   type Reference is access all Street.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Street.Utils.Reference;

   function Get_Id (This      : in Street.Utils.Object;
                    Street_Id : in Infra_Id)
   return Infra_Id;

   function Is_Not_Treadable_In_Direction (
      This      : in Street.Utils.Object;
      Street_Id : in Infra_Id;
      Direction :    Shared.Direction.Cardinal)
   return Boolean;

   not overriding
   function Get_Orientation (This      : in Street.Utils.Object;
                             Street_Id : in Infra_Id)
   return Direction.Orientation;

   not overriding
   function Get_Footways (This      : Street.Utils.Object;
                          Street_Id : in Infra_Id)
   return Infra_Id_List.List;

   not overriding
   function Find_Lanes_By_Direction (This             : in Street.Utils.Object;
                                     Street_Id        : in Infra_Id;
                                     Travel_Direction : in Direction.Straight)
                                     return Infra_Id_Set.Set;

   not overriding
   function Is_Contained_By (This : in Street.Utils.Object;
                             Street_Id, Container_Id : in Infra_Id)
                             return Boolean;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Street.Utils.Reference;

end Reactive.Infrastructure.Street.Utils;
