with Active.Agent;

with Reactive.Infrastructure.Lane.Roadway_Lane;
with Reactive.Infrastructure.Stretch.Roadway_Stretch;

package body Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory is

   package Agent renames Active.Agent;
   package Roadway_Lane
      renames Reactive.Infrastructure.Lane.Roadway_Lane;
   package Roadway_Stretch
      renames Reactive.Infrastructure.Stretch.Roadway_Stretch;

   function Create_Stretch (This : in out Roadway_Factory.Object)
      return Stretch.Reference
   is
      Stretch_Ref : Stretch.Reference;
      Traveller   : Agent.Agent_Id;
   begin
      Stretch_Ref :=
         Stretch.Reference (
            Roadway_Stretch.Create (This.Stretch_Id,
                                    This.Stretch_Size
                                   )
            );
   -- Make treading travellers be in the stretch
      for Traveller of This.Stretch_Travellers loop
         Stretch_Ref.Put_Traveller (Traveller);
      end loop;
      return Stretch_Ref;
   end Create_Stretch;

   function Create_Lane (This : in out Roadway_Factory.Object)
      return Lane.Reference
   is
   begin
      return Lane.Reference (
         Roadway_Lane.Create (This.Lane_Id,
                              This.Lane_Direction
         )
      );
   end Create_Lane;

end Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory;
