with Reactive.District;

package body Reactive.Infrastructure.Street.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Street.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Street.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end Get_Instance;

   function Get_Id (This      : in Street.Utils.Object;
                    Street_Id : in Infra_Id) return Infra_Id is
   begin
      return This.District
        .Find_Street_By_Id (Street_Id)
        .Get_Id;
   end Get_Id;

   function Is_Not_Treadable_In_Direction (
      This      : in Street.Utils.Object;
      Street_Id : in Infra_Id;
      Direction : Shared.Direction.Cardinal)
   return Boolean is
   begin
      return This.District
        .Find_Street_By_Id (Street_Id)
        .Is_Not_Treadable_In_Direction(Direction => Direction);
   end Is_Not_Treadable_In_Direction;

   function Get_Orientation (This      : in Street.Utils.Object;
                             Street_Id : in Infra_Id)
   return Direction.Orientation
   is
      Street_Ref : Street.Reference;
      Street_Orientation : Direction.Orientation;
   begin
      Street_Ref := This.District.Find_Street_By_Id (Street_Id);
      Street_Orientation := Street_Ref.Get_Orientation;
      return Street_Orientation;
   end Get_Orientation;

   function Get_Footways (This      : Street.Utils.Object;
                          Street_Id : in Infra_Id)
   return Infra_Id_List.List
   is (This.District.Find_Street_By_Id (Street_Id).Get_Footways);

   function Find_Lanes_By_Direction (This             : in Street.Utils.Object;
                                     Street_Id        : in Infra_Id;
                                     Travel_Direction : in Direction.Straight)
   return Infra_Id_Set.Set is
   begin
      return This.District
        .Find_Street_By_Id (Street_Id)
        .Find_Lanes_By_Direction (Travel_Direction => Travel_Direction);
   end Find_Lanes_By_Direction;

   function Is_Contained_By (This                    : in Street.Utils.Object;
                             Street_Id, Container_Id : in Infra_Id)
   return Boolean is
   begin
      return This.District
        .Find_Street_By_Id (Street_Id)
        .Is_Contained_By (Container_Id => Container_Id);
   end Is_Contained_By;

end Reactive.Infrastructure.Street.Utils;
