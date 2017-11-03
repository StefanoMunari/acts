with Active.Agent;

with Reactive.Infrastructure.Lane.Footway_Lane;
with Reactive.Infrastructure.Stretch.Footway_Stretch;

package body Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory is

   package Agent renames Active.Agent;
   package Footway_Lane
      renames Reactive.Infrastructure.Lane.Footway_Lane;
   package Footway_Stretch
      renames Reactive.Infrastructure.Stretch.Footway_Stretch;

   function Create_Stretch (This : in out Footway_Factory.Object)
      return Stretch.Reference
   is
      Stretch_Ref : Stretch.Reference;
      Traveller   : Agent.Agent_Id;
   begin
      Stretch_Ref :=
         Stretch.Reference (
            Footway_Stretch.Create (This.Stretch_Id,
                                    This.Stretch_Size
                                   )
            );
   -- Make treading travellers be in the stretch
      for Traveller of This.Stretch_Travellers loop
         Stretch_Ref.Put_Traveller (Traveller);
      end loop;
      return Stretch_Ref;
   end Create_Stretch;

   function Create_Lane (This : in out Footway_Factory.Object)
      return Lane.Reference
   is
   begin
      return Lane.Reference (
         Footway_Lane.Create (This.Lane_Id,
                              This.Lane_Direction
         )
      );
   end Create_Lane;

end Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory;
